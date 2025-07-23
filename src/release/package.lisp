(:name "epsilon.release"
 :version "1.0.0"
 :author "Jesse Bouwman"
 :description "Release and distribution tool for Epsilon"
 :sources ("src")
 :provides ("epsilon.tool.release")
 :dependencies ("epsilon.core"
                "epsilon.json"
                "epsilon.yaml")
 :platforms ("linux" "darwin"))