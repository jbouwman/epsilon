(:name "epsilon.doc"
 :module-set "epsilon-devtools"
 :description "Documentation extraction, indexing, and structured docstring parsing"
 :stability :experimental
 :requires ("epsilon" "epsilon.json" "epsilon.annotate" "epsilon.http" "epsilon.log")
 :commands ((:name "doc"
             :description "Generate or serve documentation artifacts: static site (`site`) or live HTTP server (`serve`)"
             :usage-hint "doc <subcommand> [options]"
             :usage "epsilon doc <subcommand> [options]"
             :handler "epsilon.doc.cli:run-doc"
             :examples ("epsilon doc site --output docs/api"
                        "epsilon doc site --output docs/api --load-all"
                        "epsilon doc serve"
                        "epsilon doc serve --port 9000 --address 0.0.0.0"))))
