(:name "epsilon.darwin"
 :version "0.1.0"
 :author "Jesse Bouwman"
 :description "Darwin/macOS platform services including kqueue event system, async networking, and TLS/SSL support via LibreSSL"
 :sources ("src")
 :tests ("tests")
 :requires ("epsilon.core" "epsilon.foreign")
 :platform "darwin"
 :provides ("epsilon.kqueue" "epsilon.net"))
