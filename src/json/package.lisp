(:name "epsilon.json"
 :version "1.0.0"                       ; TODO core modules should track main release version
 :author "Jesse Bouwman"
 :description "JSON parser"
 :sources ("src")
 :tests ("tests")
 :data ("tests/json")                   ; JSON test files used for testing
 :dependencies ("epsilon.core" "epsilon.parsing"))
