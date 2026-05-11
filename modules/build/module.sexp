(:name "epsilon.build"
 :module-set "epsilon-stdlib"
 :description "Parallel module compilation via SBCL subprocess pool"
 :stability :stable
 :requires ("epsilon.process")
 :commands ((:name "build-worker"
             :description "Run as an epsilon.build worker subprocess (internal)"
             :usage-hint "build-worker [--max-jobs N]"
             :usage "epsilon build-worker [--max-jobs N]"
             :handler "epsilon.build.worker:run-worker-cli"
             :options ((:name "--max-jobs" :arg "N"
                        :description "Retire after N completed jobs (default 20)")))
            (:name "build"
             :description "Compile a module and its dependencies in parallel"
             :usage-hint "build [modules...]"
             :usage "epsilon build [--workers N] [--max-jobs-per-worker N] [--force] [--fail-fast] [--all] [--verbose] [--print-plan] [modules...]"
             :handler "epsilon.build.cli:run"
             :options ((:name "--workers" :arg "N"
                        :description "Number of parallel SBCL workers (default: auto)")
                       (:name "--max-jobs-per-worker" :arg "N"
                        :description "Recycle workers after N jobs (default 20)")
                       (:name "--force" :description "Bypass FASL cache and force recompile")
                       (:name "--fail-fast" :description "Abort run on first module failure")
                       (:name "--all" :description "Build every module known to the workspace")
                       (:name "--verbose"
                        :description "Print the build plan before starting and a parallelism speedup line after")
                       (:name "--print-plan"
                        :description "Print the topo-sorted build plan with critical-path heights and exit without building")
                       (:name "--load-targets"
                        :description "After build succeeds, sequentially load targets into this parent image"))
             :examples (("epsilon build epsilon.json" "Build epsilon.json + deps")
                        ("epsilon build --all --workers 16" "Build everything with 16 workers")
                        ("epsilon build --print-plan --all" "Print the linearized build order for the whole tree")
                        ("epsilon build --verbose epsilon.http" "Build with per-module timing + parallelism speedup")))))
