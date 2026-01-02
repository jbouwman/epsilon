;;;; Epsilon Workspace Definition
;;;;
;;;; This file defines the modules that comprise the epsilon system.
;;;;
;;;; The epsilon loader reads workspace.lisp from EPSILON_HOME (set by
;;;; the epsilon shell script). Additional workspaces can be loaded
;;;; dynamically using the ,workspace REPL command.
;;;;
;;;; To create a project that uses epsilon modules:
;;;;   (:name "my-project"
;;;;    :modules ("modules/my-module")
;;;;    :workspaces ("/path/to/epsilon"))

(:name "epsilon"

 ;; All modules in this workspace, listed in dependency order.
 ;; The order here affects display but not loading (the loader
 ;; resolves dependencies from module.lisp :requires fields).
 :modules
 (;; Core runtime and utilities
  "modules/core"

  ;; Foundational libraries (no epsilon dependencies)
  "modules/parsing"
  "modules/regex"
  "modules/digest"
  "modules/library"

  ;; Build and compilation tools
  "modules/compiler"
  "modules/loader"

  ;; Foreign function interface
  "modules/foreign"

  ;; Platform-specific implementations
  ;; (only one loads per platform via :platform field)
  "modules/linux"
  "modules/darwin"
  "modules/windows"

  ;; I/O and async runtime
  "modules/io"

  ;; Binary encoding (endian-aware integer conversion)
  "modules/binary"

  ;; Data formats
  "modules/json"

  ;; Security and networking
  "modules/crypto"
  "modules/http"

  ;; Development tools
  "modules/test"
  "modules/lint"
  "modules/nx"
  "modules/cli"

  ;; Distribution and updates
  "modules/install"
  "modules/release"
  "modules/update"))
