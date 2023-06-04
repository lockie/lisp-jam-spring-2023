(in-package #:thoughtbound)

(define-constant +window-width+ 1280)
(define-constant +window-height+ 720)

(declaim (type single-float +scale+))
(define-constant +scale+ 2.0)

(define-constant +tile-size+ 16)
(define-constant +scaled-tile-size+ (* +scale+ +tile-size+))

(define-constant +repl-update-interval+ 0.1d0)

(define-constant +font-path+ "../Resources/fonts/inconsolata.ttf"
  :test #'string=)
(define-constant +font-size+ 24)
(define-constant +ui-font-path+ "../Resources/fonts/bookxel.otf"
  :test #'string=)
(define-constant +ui-font-size+ 32)

(define-constant +config-path+ "../config.cfg"
  :test #'string=)

(declaim (type ecs::storage *storage*))
(defvar *storage*)

(declaim (type ecs::entity *player-entity*))
(defvar *player-entity*)

(declaim (type boolean *deathp*))
(defvar *deathp* nil)

(defvar *ui-font*)

(defvar *ui-context* (cffi:null-pointer))

(defvar *quit*)
(defvar *restart*)
