(defsystem "thoughtbound"
  :version "0.0.3"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-tiled
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "globals")
                 (:file "sprites")
                 (:file "map")
                 (:file "characters")
                 (:file "sounds")
                 (:file "ai")
                 (:file "ui")
                 (:file "stairs")
                 (:file "win")
                 (:file "main"))))
  :description "A Spring Lisp Game Jam 2023 entry."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"thoughtbound"
  :entry-point "thoughtbound:main")
