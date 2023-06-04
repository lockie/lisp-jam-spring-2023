(in-package #:lisp-jam-spring-2023)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 720)

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

(declaim (type fixnum *fps*))
(defvar *fps* 0)
(defvar *fpsp*)

(defun update (dt)
  (when *fpsp*
    (unless (zerop dt)
      (setf *fps* (round 1 dt))))
  (ecs:run-systems *storage* :dt dt))

(defvar *font*)
(defvar *ui-font*)

(defvar *ui-context*)

(defun render ()
  (when *fpsp*
    (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                  (format nil "~d FPS" *fps*)))
  (nk:allegro-render))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                         (al:show-native-message-box
                          (cffi:null-pointer) "Hey guys"
                          "We got a big error here :("
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace e :stream s))
                          (cffi:null-pointer) :error)))))
    (al:set-app-name "lisp-jam-spring-2023")
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (setf *fpsp* (al:get-config-value (al:get-system-config)
                                      "game" "show-fps"))
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Lisp Jam Spring 2023")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (setf *font*
                   (al:ensure-loaded #'al:load-ttf-font
                                     +font-path+ (- +font-size+) 0)
                   *ui-font*
                   (al:ensure-loaded #'nk:allegro-font-create-from-file
                                     +ui-font-path+ (- +ui-font-size+) 0)
                   *window-background*
                   (al:ensure-loaded #'nk:allegro-create-image
                                     +window-background-path+)
                   *button-background*
                   (al:ensure-loaded #'nk:allegro-create-image
                                     +button-background-path+)
                   *button-background2*
                   (al:ensure-loaded #'nk:allegro-create-image
                                     +button-background2-path+)
                   *ui-context*
                   (nk:allegro-init
                    *ui-font* display +window-width+ +window-height+))
             (setf *storage* (ecs:make-storage))
             (load-sprites)
             (load-sounds)
             (load-map "../Resources/maps/test.tmx")
             (ecs:run-systems *storage* :dt 0d0) ;; HACK: prime system bitmaps
             (let ((player (ecs:make-object
                            *storage*
                            `((:player)
                              (:character :speed 75.0
                                          :target-x 160.0
                                          :target-y 160.0)
                              (:animation-state :sprite :player)
                              (:sprite-sheet)
                              (:size)
                              (:position :x 160.0 :y 160.0))))
                   (orc (ecs:make-object
                         *storage*
                         `((:character :speed 50.0
                                       :target-x 300.0
                                       :target-y 300.0)
                           (:animation-state :sprite :orc)
                           (:sprite-sheet)
                           (:size)
                           (:ai :notice-range 20000.0
                                :attack-range 1000.0)
                           (:position :x 300.0 :y 300.0)))))
               (change-animation *storage* player :idle)
               (setf *player-entity* player
                     *deathp* nil)
               (change-animation *storage* orc :move))
             (livesupport:setup-lisp-repl)
             (trivial-garbage:gc :full t)
             (loop :named event-loop
                   :with ticks :of-type double-float := (al:get-time)
                   :with last-repl-update :of-type double-float := ticks
                   :with dt :of-type double-float := 0d0
                   :while (loop :initially (nk:input-begin *ui-context*)
                                :while (al:get-next-event event-queue event)
                                :for type :=
                                          (cffi:foreign-slot-value
                                           event '(:union al:event) 'al::type)
                                :do (nk:allegro-handle-event event)
                                :always (not (eq type :display-close))
                                :finally (nk:input-end *ui-context*))
                   :do (let ((new-ticks (al:get-time)))
                         (setf dt (- new-ticks ticks)
                               ticks new-ticks))
                       (when (> (- ticks last-repl-update)
                                +repl-update-interval+)
                         (livesupport:update-repl-link)
                         (setf last-repl-update ticks))
                       (al:clear-to-color (al:map-rgb 0 0 0))
                       (livesupport:continuable
                         (update dt)
                         (render))
                       (al:flip-display)
                   :finally
                      (nk:allegro-shutdown)
                      (nk:allegro-del-image *button-background2*)
                      (nk:allegro-del-image *button-background*)
                      (nk:allegro-del-image *window-background*)
                      (nk:allegro-font-del *ui-font*)
                      (al:destroy-font *font*)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:uninstall-audio)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %main))))

