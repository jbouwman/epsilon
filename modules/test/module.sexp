(:name "epsilon.test"
 :module-set "epsilon-stdlib"
 :description "Test framework for Epsilon"
 :stability :stable
 :requires ("epsilon.process" "epsilon.crypto" "epsilon.annotate" "epsilon.channel" "epsilon.stacktrace" "epsilon.build")
 :commands ((:name "test"
             :description "Run or list tests for specified modules"
             :usage-hint "test [run|list] [modules]"
             :usage "epsilon test [run|list] [modules...] [options]"
             :handler "epsilon.test.cli:run"
             :options ((:name "--all" :description "Test every known module (cache hits still skip)")
                       (:name "--fail-fast" :description "Stop on first module failure")
                       (:name "--parallel" :arg "N" :description "Run N modules in parallel (default: auto-detect)")
                       (:name "--sequential" :description "Force sequential module execution")
                       (:name "--verbose" :description "Verbose output (full output per module)")
                       (:name "--noclean" :description "Retain test work directories")
                       (:name "--update-snapshots" :description "Update snapshot files")
                       (:name "--integration" :description "Run integration tests only")
                       (:name "--force" :description "Bypass test result cache (re-run even if unchanged)")
                       (:name "--tag"
                        :arg "TAG"
                        :description "Only run tests with tag TAG (repeatable)")
                       (:name "--no-tag"
                        :arg "TAG"
                        :description "Exclude tests with tag TAG (repeatable)")
                       (:name "--tags" :description "List all known tags (list subcommand)")
                       (:name "--name"
                        :arg "PATTERN"
                        :description "Filter tests by name substring (list subcommand)"))
             :examples (("epsilon test"
                         "Test modules changed since main + transitive dependents (default)")
                        ("epsilon test json" "Run JSON module tests + dependents")
                        ("epsilon test --force json" "Re-run JSON module tests, ignoring cache")
                        ("epsilon test --all" "Test every module (cache hits still skip)")
                        ("epsilon test --all --force" "Re-run every test from scratch")
                        ("epsilon test run json --tag slow" "Run only tests tagged :slow")
                        ("epsilon test list" "List all tests across all modules")
                        ("epsilon test list json" "List tests in the JSON module")
                        ("epsilon test list --name parse" "List tests matching 'parse'")
                        ("epsilon test list --tags" "List all known test tags")))))
