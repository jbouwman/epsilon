(:name "epsilon.fs"
 :module-set "epsilon-devtools"
 :description "Filesystem query and watch facility"
 :stability :stable
 :requires ("epsilon")
 :commands ((:name "find"
             :description "Query the filesystem with composable filters"
             :usage-hint "find [options] <query> [roots...]"
             :usage "epsilon find [options] <query> [roots...]"
             :handler "epsilon.fs.cli:run"
             :options ((:name "--format" :arg "FORMAT" :description "Output: path, name, detail")
                       (:name "--module" :arg "MODULE" :description "Scan module sources"))
             :examples ("epsilon find '(name? \"*.lisp\")' src/"
                        "epsilon find '(and (name? \"*.lisp\") (contains? \"defpackage\"))' ."
                        "epsilon find --module epsilon.refactor '(name? \"*-tests.lisp\")'"))))
