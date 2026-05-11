(:name "epsilon"
 :description "Bootstrap library for Epsilon"
 :commands ((:name "modules"
             :description "List available modules"
             :usage-hint "modules [--json]"
             :usage "epsilon modules [--json]"
             :options ((:name "--json" :description "Output module graph as JSON"))
             :handler "epsilon.commands:cmd-modules")))
