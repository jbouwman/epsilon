(:name "epsilon.darwin"
 :module-set "epsilon-core"
 :description "Darwin/macOS platform services including kqueue event system and async networking"
 :stability :stable
 :requires ("epsilon.foreign")
 :platform "darwin"
 :provides ("epsilon.net" "epsilon.async"))
