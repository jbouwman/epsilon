(:name "epsilon.foreign"
 :version "0.1.0"
 :author "Jesse Bouwman"
 :description "Foreign Function Interface and C language integration"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core" "epsilon.parsing")
 :provides ("epsilon.foreign" "epsilon.clang"))