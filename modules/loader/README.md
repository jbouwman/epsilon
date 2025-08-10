This is a placeholder for a 'third loader'.

The first loader is a bootstrapping script (in scripts/boot.lisp)
that loads lisp files in fixed order, just enough to be able to
execute the second loader.

The second loader is package and dependency aware within the scope of
the epsilon source repository, and is able to load networking and
security packages.

The third loader should support, at least:

- resolving and fetching remote packages
- largs-scale, fine-grained source dependencies
- file watching an hot reload of packages

It should eventually go on to support multiple subprocess images for
parallel editing, time travel and other avant garde features.
