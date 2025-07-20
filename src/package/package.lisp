(:name "epsilon.package"
 :version "0.2.0"
 :author "Jesse Bouwman"
 :description "Package management and distribution system for Epsilon modules with dependency resolution, version constraints, EPK format support, and efficient boot caching"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core")
 :provides ("epsilon.lib.package" "epsilon.lib.package.repository" "epsilon.lib.package.dependency" "epsilon.lib.package.boot" "epsilon.lib.package.builder" "epsilon.lib.package.system"))