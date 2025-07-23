(:name "epsilon.http"
 :version "1.0.0"
 :author "Jesse Bouwman"
 :description "HTTP client and server implementation"
 :sources ("src")
 :tests ("tests")
 :dependencies ("epsilon.core"
                #+darwin "epsilon.darwin"
                #+linux "epsilon.linux"  
                #+win32 "epsilon.windows")
 :provides ("epsilon.http"))