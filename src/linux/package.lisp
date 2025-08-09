(:name "epsilon.linux"
 :version "1.0.0"
 :author "Jesse Bouwman"
 :description "Linux-specific functions (epoll networking)"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core" "epsilon.foreign")
 :platform "linux"
 :provides ("epsilon.net"))