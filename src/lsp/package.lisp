(:name "epsilon.lsp"
 :version "0.2.0"
 :author "Epsilon Project"
 :description "Language Server Protocol implementation with code evaluation support"
 :sources ("src" "src/evaluation")
 :tests ("tests")
 :dependencies ("epsilon.core" "epsilon.parsing" "epsilon.json")
 :provides ("epsilon.lsp" "epsilon.lsp.evaluation.session" "epsilon.lsp.evaluation.protocol" "epsilon.lsp.evaluation.api" "epsilon.lsp.evaluation.runner" "epsilon.lsp.evaluation.integration"))