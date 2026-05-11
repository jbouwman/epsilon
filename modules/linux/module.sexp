(:name "epsilon.linux"
 :module-set "epsilon-core"
 :description "Linux-specific functions (epoll networking)"
 :stability :stable
 :requires ("epsilon.foreign")
 :platform "linux"
 :provides ("epsilon.net" "epsilon.async"))
