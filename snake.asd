;;;; snake.asd

(asdf:defsystem #:snake
  :version "1.0"
  :description "A Remake of the Classic Game Snake"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-mixer)
  :serial t
  :components ((:file "package")
               (:file "snake")))

