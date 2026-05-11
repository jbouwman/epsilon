(:name "epsilon.repl"
 :module-set "epsilon-stdlib"
 :description "Minimal stdio read/eval/print loop. The simple REPL that ships with the core distribution; the full RPC/wire/LSP/MCP development surface is `epsilon server` (in avalon)."
 :stability :stable
 :requires ("epsilon")
 :commands ((:name "repl"
             :description "Start a stdio read/eval/print loop"
             :usage-hint "repl"
             :usage "epsilon repl [--package NAME] [--no-banner]"
             :handler "epsilon.repl.cli:run-repl"
             :options ((:name "--package" :arg "NAME"
                        :description "Initial package (default: CL-USER)")
                       (:name "--no-banner"
                        :description "Suppress the startup banner"))
             :examples ("epsilon repl"
                        "epsilon repl --package epsilon.json"
                        "rlwrap epsilon repl     # for line editing + history"))))
