(:name "epsilon.json"
 :module-set "epsilon-stdlib"
 :description "JSON parser"
 :stability :stable
 :data ("tests/json")
 :requires ("epsilon.parse")
 :commands ((:name "json"
             :description "Pretty-print or validate a JSON document"
             :usage-hint "json [options] [FILE]"
             :usage "epsilon json [--check] [--compact] [--sort-keys] [--canonical] [FILE]"
             :handler "epsilon.json.cli:run"
             :options ((:name "--check"
                        :description "Validate only; print nothing on success")
                       (:name "--compact"
                        :description "Emit on one line, no indentation")
                       (:name "--sort-keys"
                        :description "Sort object members lexicographically")
                       (:name "--canonical"
                        :description "RFC 8785 (JCS) canonical form (implies --compact)"))
             :examples ("epsilon json config.json"
                        "curl -s https://api/x | epsilon json"
                        "epsilon json --check config.json"
                        "epsilon json --canonical doc.json | sha256sum"))))
