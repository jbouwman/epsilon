(:name "epsilon.windows"
 :module-set "epsilon-core"
 :description "Windows-specific functions (IOCP networking)"
 :stability :stable
 :requires ("epsilon.foreign")
 :platform "windows"
 :provides ("epsilon.net" "epsilon.async"))
