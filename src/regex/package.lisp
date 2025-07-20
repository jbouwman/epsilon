(:name "epsilon.regex"
 :version "0.1.0"
 :author "Jesse Bouwman"
 :description "Complete regular expression engine with NFA/DFA conversion and optimization"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core")
 :provides ("epsilon.lib.regex" "epsilon.lib.charset"))