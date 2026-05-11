# `pin_call_chain_and_boxed_registers` infinite-loops on darwin-arm64 with sb-fiber

## Summary

On darwin-arm64, an SBCL process with `sb-fiber` enabled wedges 100% of
one core inside `garbage_collect_generation` whenever a fiber triggers
GC during heavy allocation.  All other threads parked at
`sig_stop_for_gc_handler` wait for stop-the-world to release; from
outside the process this looks like the entire runtime is hung.  The
hot loop is the `while(1)` frame-chain walker in
`pin_call_chain_and_boxed_registers` (gencgc.c:3097, called from
gencgc.c:3505).  The loop terminates only on `ocfp == 0`, and on
arm64+sb-fiber the live thread's frame chain is reaching that walker
without a NULL terminator.

The same reproducer runs cleanly on linux-x86_64 with the same SBCL
fork.  x86-64 reuses the native C stack as the Lisp control stack
(`LISP_FEATURE_C_STACK_IS_CONTROL_STACK`); arm64 has a separate
mmap'd Lisp control stack, allocated per-fiber by
`sb_fiber_lisp_stack_alloc` in `runtime/fiber-arm64.c`.  This
architectural asymmetry exactly matches the platform asymmetry of
the bug.

## Environment

- SBCL fork: <https://github.com/jbouwman/sbcl>
- Commit: `cdef8f6c0` ("sb-fiber: binding-stack guard, park/unpark,
  stack introspection")
- Build: `--fancy --with-sb-core-compression`, contribs sb-posix,
  sb-rotate-byte
- Host: macOS 26.4.1 (Darwin 25.4.0), Apple Silicon (M-class)
- Reproducer: see "Reproduction" below

Linux-x86_64 with the same fork commit, same dynamic-space-size, same
reproducer: clean.  Bug is darwin-arm64-only.  arm64 Linux has not
been tested yet but the diagnosis below points at the
`fiber-arm64.c` code path, so arm64 Linux is expected to reproduce
as well.

## Symptom

`sample(1)` of the wedged process shows one carrier thread at 100% CPU
with the leaf frame inside `garbage_collect_generation`:

```
Thread_*: carrier-0
  call_into_lisp     (sbcl) +172   arm64-assem.S:270    [N times, growing
  ???                                                    indentation -- the
  ???                                                    "deep recursive
  ???                                                    call_into_lisp"
  ???                                                    pattern in the
  ???                                                    sample]
  ???
  collect_garbage    (sbcl) +736   gencgc.c:4014
  garbage_collect_generation (sbcl) +3616, +3604, ...   gencgc.c:3505
```

Every other thread is stopped at `sig_stop_for_gc_handler →
thread_wait_until_not → semaphore_wait_trap`, waiting for
stop-the-world to release.  The leaf-offset list `+ 3616, 3604` is a
narrow two-instruction window inside `pin_call_chain_and_boxed_registers`
— a tight loop.

The only `while(1)` in the function on arm64 (which lacks `reg_RA`
and so takes the `#else` branch) is:

```c
/* gencgc.c:3114-3125 */
lispobj *cfp = access_control_frame_pointer(th);
if (cfp) {
    while (1) {
        lispobj* ocfp = (lispobj *) cfp[0];
        lispobj lr = cfp[1];
        if (ocfp == 0)
            break;
        maybe_pin_code(lr);
        cfp = ocfp;
    }
}
```

The walker exits only on `ocfp == 0`.  On the wedged thread, the
chain therefore either has a cycle or walks indefinitely without ever
reading 0 from `cfp[0]`.

## Reproduction

Self-contained reproducer in the calling project at
`epsilon-contrib/scheduler/scripts/repro-fiber-keepalive.lisp`.  In
brief:

1. Allocate a per-fiber Lisp control stack (default 64 KiB).
2. Spawn a fiber (via `sb-fiber:make-fiber`) that:
   - Builds a deeply-nested HAMT of ~4096 entries (forces ~200 KB of
     transient cons/vector allocation).
   - JSON-encodes it to a string (more allocation, deep recursion).
3. Switch into the fiber.

The fiber's first call into Lisp triggers GC partway through the
allocation, and the carrier thread enters
`garbage_collect_generation`'s frame-chain walker and never returns.

A purely SBCL-only repro that sidesteps the calling project should
amount to:

```lisp
(require :sb-fiber)

(defun heavy ()
  (let ((acc nil))
    (dotimes (i 100000)
      (setf acc (cons (cons i (cons i (cons i nil))) acc)))
    (length acc)))

(defun fiber-body () (loop repeat 50 do (heavy)))

(let ((main  (sb-fiber:make-main-fiber))
      (child (sb-fiber:make-fiber #'fiber-body)))
  (sb-fiber:fiber-switch main child))
```

This has not been minimised and confirmed yet; the calling project's
reproducer has been confirmed.  Capture the wedge with:

```bash
PID=$(pgrep sbcl)
sample "$PID" 3 1 -file /tmp/wedge.txt    # any writable path
```

and look for `garbage_collect_generation` as the leaf frame on the
hot carrier thread.

## Diagnosis

### What the walker is doing

`pin_call_chain_and_boxed_registers` (runtime/gencgc.c:3097) is called
from `garbage_collect_generation` (gencgc.c:3505) for each thread
during the conservative-stack-scan portion of `#else defined
reg_LINK_RETURN`.  Its job: walk the per-thread Lisp control-frame
chain, pinning the code object reachable through each frame's
saved link register so a moving GC doesn't relocate code that an
in-flight call will return to.

The walk starts at `th->control_frame_pointer` and follows frame[0]
(outer CFP) until it reads NULL.  This relies on the compiler's
prologue having stored 0 into `frame[0]` of the bottom-most frame on
the thread's control stack — i.e., the prologue of the very first
Lisp function entered on that stack.

### What `th->control_frame_pointer` is on arm64+sb-fiber

`access_control_frame_pointer(th)` reads `th->control_frame_pointer`
(a memory location) on arm64.  The runtime updates it at:

- Thread init (alloc.c:782): `access_control_frame_pointer(th) = 0`.
- Fiber resume (fiber-arm64.c:179): `th->control_frame_pointer =
  f->control_frame_pointer`, where `f->control_frame_pointer` was
  either `(lispobj*)p` (initial alloc, line 128) or whatever the
  fiber saved at its last suspend (line 164).
- Fiber suspend (fiber-arm64.c:164): saves the live value out.
- `build_fake_control_stack_frames` (interrupt.c:780): writes a
  fresh fake frame address into `th->control_frame_pointer` for
  threads receiving STOP_FOR_GC.
- Whatever Lisp code does to keep it in sync with `reg_CFP` (= x29 on
  arm64 per `runtime/arm64-lispregs.h:63`).

That last point is the load-bearing one and is harder to verify
without a deeper read of the arm64 backend.  If `th->control_frame_pointer`
diverges from the live x29 between updates, GC sees a stale pointer.
A stale pointer that happens to land in a region the GC walker can
follow indefinitely reproduces the symptom.

### What the bottom-of-stack frame looks like at fiber init

For a freshly allocated child fiber:

- `sb_fiber_prepare` (fiber-arm64.c:17–47) seeds `f->ctx.fp = 0` and
  `f->ctx.x19 = (void*)f`; `lr = fiber_trampoline_asm`.
- `sb_fiber_lisp_stack_alloc` (line 97–132) mmaps a 3-guard-page
  region and sets `f->control_stack_pointer = f->control_frame_pointer
  = (lispobj*)p` (the base of the mmap).  POSIX guarantees the
  region is zero-filled.

When the fiber is first switched to, `fiber_swap_context`
(fiber_switch_arm64.S:46) restores `x29 = f->ctx.fp = 0` and `lr =
fiber_trampoline_asm`, then `RET`s.  The trampoline tail-calls
`fiber_trampoline_c`, which (in C) eventually calls into Lisp via
`call_into_lisp`.  The arm64 prologue should then store the *current*
x29 (= 0) into `frame[0]` of the new frame at csp = p, so the
bottom-most frame correctly has `frame[0] = 0`.

This *should* terminate the walker on the first iteration.  That it
doesn't means one of:

1. The arm64 prologue does not, in fact, store 0 into `frame[0]` for
   the very first frame on a fiber's stack — perhaps because the
   prologue elides the store when it observes its caller is foreign C.
2. `th->control_frame_pointer` is not pointing at the bottom-most
   frame at all by the time GC fires; it's pointing further up the
   chain at a frame whose `frame[0]` reaches an unrelated memory
   region (e.g. a previous fiber's freed mmap'd stack, an SBCL
   heap page, or the carrier's main-fiber stack), and the chain
   walks until it hits whatever happens to be at the rightful
   address.
3. A second fiber on the same carrier suspended mid-allocation, its
   `f->control_frame_pointer` points to a frame on its mmap'd stack,
   and the walker (iterating `for_each_thread`) is following *that*
   thread's chain, not carrier-0's.  But carrier-1's chain on a
   separate fiber stack should not loop infinitely either, unless
   the same root-cause applies to it.

The walker has no defence against any of these — once it dereferences
a corrupt `cfp[0]`, it will follow that pointer and load the next
"ocfp" from wherever it lands.  In a 1024 MB dynamic space full of
non-zero words, hitting a 0 by chance is rare; the loop runs
effectively forever.

### Why x86-64 doesn't reproduce

On x86-64, `LISP_FEATURE_C_STACK_IS_CONTROL_STACK` is defined.  The
Lisp control stack IS the native C stack; `sb_fiber_lisp_stack_alloc`
is not built (search `Config.x86-64-*` — there is no per-fiber Lisp
stack allocation step), and the frame chain follows the C ABI's
RBP chain rooted at the native stack top, which the SysV ABI prologue
zeroes for the entry frame as a matter of routine.  The asymmetry
matches.

## Proposed Fixes

These are listed in increasing order of invasiveness.  Pick one or
combine.

### Fix A: Bound the walker (defensive).  ~5 lines.

Cap the iteration count and `lose` on overflow with enough info to
debug.  Cheap, fully local, and turns a hang into a panic with a
backtrace.

```c
/* gencgc.c, replacing the while(1) at line 3117 */
if (cfp) {
    int frames = 0;
    while (frames++ < 1 << 20) {
        lispobj* ocfp = (lispobj *) cfp[0];
        lispobj lr = cfp[1];
        if (ocfp == 0)
            break;
        maybe_pin_code(lr);
        cfp = ocfp;
    }
    if (frames >= 1 << 20)
        lose("control frame chain at %p did not terminate within %d frames",
             (void*)access_control_frame_pointer(th), 1 << 20);
}
```

This does not fix the underlying bug; it makes it diagnosable.
Recommended as a first step regardless: a runaway frame chain is
*always* a bug, and silently looping is the worst possible response.

### Fix B: Bounds-check against the thread's control stack.  ~10 lines.

Refuse to follow a chain pointer that lands outside
`[control_stack_start, control_stack_end)`.  Graceful degradation:
malformed chain → walker stops, some pinning is missed, no hang and
no panic.

```c
if (cfp) {
    lispobj *stack_lo = th->control_stack_start;
    lispobj *stack_hi = th->control_stack_end;
    while (1) {
        if (cfp < stack_lo || cfp >= stack_hi - 1)
            break;                                  /* off-stack */
        lispobj* ocfp = (lispobj *) cfp[0];
        lispobj lr = cfp[1];
        if (ocfp == 0)
            break;
        maybe_pin_code(lr);
        cfp = ocfp;
    }
}
```

Note this needs `th->control_stack_start/end` to reflect the *current
fiber's* bounds, which `sb_fiber_lisp_stack_resume` (fiber-arm64.c:171)
already sets.  Combine with Fix A's iteration cap to defend against
in-region cycles.

### Fix C: Force the bottom-of-stack zero terminator at fiber init.

If the suspicion is that the arm64 prologue elides storing 0 into
`frame[0]` for the very first frame on a fiber's mmap'd stack, then
fixing it from the runtime side is one store at fiber-creation time:

```c
/* fiber-arm64.c sb_fiber_lisp_stack_alloc, after the mprotect calls */
*(lispobj*)p = 0;       /* explicit ocfp = 0 sentinel for the
                           bottom-most Lisp frame, defensive against
                           prologues that don't store ocfp on the
                           very first call_into_lisp into this fiber. */
```

Cheap and architecturally local, but only addresses one of the
three failure modes hypothesised above.  Worth doing in conjunction
with Fix A so any remaining failure mode surfaces visibly.

### Fix D: Synchronise `th->control_frame_pointer` with reg_CFP at every
fiber-switch entry/exit and at every alloc-trap.

This is the most invasive — it would mean reading the live x29 in
the fiber-switch VOP / asm and storing it in `th->control_frame_pointer`
on suspend, and similarly on resume.  Probably necessary if the
diagnosis is that `th->control_frame_pointer` is going stale and the
walker is dereferencing a freed stack page.  Recommended only after
Fix A captures a panic backtrace that confirms which thread / fiber
the bad chain belongs to.

## Recommended Sequence

1. Land **Fix A** alone, rebuild, re-run the reproducer, capture the
   panic.  Expected output: address of the runaway `cfp`, count of
   iterations before the cap, identity of the thread.
2. Inspect the address: is it inside any thread's
   `control_stack_start..control_stack_end`?  Inside an mmap'd fiber
   stack?  Inside the SBCL dynamic-space heap?  Inside a freed page?
   This narrows the failure mode.
3. Apply **Fix B** and/or **Fix C** as appropriate to that diagnosis.
   Re-run the reproducer; expect it to pass.
4. If after B+C the reproducer still wedges, escalate to **Fix D**.

## Test

A regression test for `contrib/sb-fiber/tests/` along the lines of:

```lisp
(deftest fiber-heavy-allocation-survives-gc
  (let ((iters 100))
    (dotimes (i iters)
      (let ((main  (sb-fiber:make-main-fiber))
            (child (sb-fiber:make-fiber
                    (lambda ()
                      (let ((acc nil))
                        (dotimes (j 100000)
                          (setf acc (cons (cons j (cons j nil)) acc)))
                        (length acc))))))
        (sb-fiber:fiber-switch main child)))
    iters))
```

Should complete in seconds; with the bug, hangs on darwin-arm64
within the first iteration's first GC cycle.

## Attachments

A `sample(1)` capture of the wedge is at the project's
`.scratch/sample-darwin-arm64.txt` (~30 KB).  Key features:

- One thread (carrier-0) has 500 stack samples all at the same
  `call_into_lisp` PC (`+172`, arm64-assem.S:270), i.e. a deep
  recursive sequence of identical-PC frames.
- Bottom (leaf) frame of that thread is `garbage_collect_generation`
  (`gencgc.c:3505`) with the offset window `+ 3616, 3604`.
- All other threads are stopped at `sig_stop_for_gc_handler`.
- "Sort by top of stack, same collapsed" shows
  `garbage_collect_generation 2363` and `semaphore_wait_trap 18904`,
  consistent with one carrier hot-looping in GC and the rest waiting.

## References

- gencgc.c:3097-3152 — `pin_call_chain_and_boxed_registers`.
- gencgc.c:3505 — call site under `# elif defined reg_LINK_RETURN`.
- runtime/fiber-arm64.c — arm64 sb-fiber stack management.
- runtime/fiber_switch_arm64.S — context-swap implementation.
- runtime/arm64-lispregs.h:63 — `reg_CFP = x29`.
- runtime/interrupt.c:773-790 — `build_fake_control_stack_frames`,
  the path that writes `th->control_frame_pointer` for threads
  receiving STOP_FOR_GC.
