(in-package #:lisp-jam-spring-2023)

(declaim (type single-float +scale+))
(define-constant +scale+ 2.0)

(ecs:defcomponent position
  "The object position in pixels."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(ecs:defcomponent sprite-sheet
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(ecs:defcomponent animation
  "There is a separate entity for every animation of every sprite, having this
component (and also sprite-sheet and size). This separate entity is very much
like a prefab."
  (name :|| :type keyword :index animation :unique t :documentation
        "player-idle, skeleton-move etc")
  (x 0.0 :type single-float :documentation "start of animation x")
  (y 0.0 :type single-float :documentation "start of animation y")
  (nframes 0 :type non-negative-fixnum)
  (speed 0d0 :type double-float))

(ecs:defcomponent size
  (width  0.0 :type single-float)
  (height 0.0 :type single-float))

(ecs:defcomponent animation-state
  (sprite :|| :type keyword)
  (current :|| :type keyword)
  ;; those should be copied from the animation component for performance reasons
  (x 0.0 :type single-float :documentation "start of animation x")
  (y 0.0 :type single-float :documentation "start of animation y")
  (nframes 0 :type non-negative-fixnum)
  (speed 0d0 :type double-float)
  ;; state
  (frame 0 :type non-negative-fixnum)
  (time 0d0 :type double-float)
  (left nil :type boolean :documentation "T if sprite should be flipped left"))

(define-constant +tint-color+ (al:map-rgba 255 255 255 255) :test #'equalp)

(ecs:defsystem render-animation
  (:components-ro (position size sprite-sheet animation-state)
   :pre (al:hold-bitmap-drawing t)
   :post (al:hold-bitmap-drawing nil))
  (al:draw-tinted-scaled-rotated-bitmap-region
   sprite-sheet-bitmap
   (+ animation-state-x
      (* animation-state-frame size-width))
   animation-state-y
   size-width
   size-height
   +tint-color+
   (/ size-width 2)
   (/ size-height 2)
   position-x
   position-y
   +scale+
   +scale+
   0.0
   (if animation-state-left
       (cffi:foreign-bitfield-value
        'al::draw-flags
        '(:flip-horizontal))
       0)))

(ecs:defsystem update-animation
  (:components-rw (animation-state)
   :arguments ((:dt double-float)))
  (incf animation-state-time dt)
  (when (> animation-state-time
           animation-state-speed)
    (multiple-value-bind (nframes rest-time)
        (floor animation-state-time animation-state-speed)
      (setf animation-state-time rest-time)
      (incf animation-state-frame nframes)
      (when (>= animation-state-frame animation-state-nframes)
        (setf animation-state-frame
              (rem animation-state-frame animation-state-nframes))))))

(defun change-animation (storage entity animation)
  (with-animation-state () storage entity
    (unless (eq animation current)
      (setf current animation
            frame 0
            time 0d0)
      (let* ((animation-name (format-symbol :keyword "~a-~a" sprite animation))
             (animation-entity (animation-entity storage animation-name)))
        (with-animation
            (_ animation-x animation-y animation-nframes animation-speed)
            storage animation-entity
          (setf x animation-x
                y animation-y
                nframes animation-nframes
                speed animation-speed))
        (with-size (animation-width animation-height) storage animation-entity
          (with-size () storage entity
            (setf width animation-width
                  height animation-height)))
        (with-sprite-sheet (animation-bitmap) storage animation-entity
          (with-sprite-sheet () storage entity
            (setf bitmap animation-bitmap)))))))

(cffi:defcallback load-sprite :int
    ((file (:pointer (:struct al::fs-entry))) (data :pointer))
  (declare (ignore data))
  (with-open-file (stream (al::get-fs-entry-name file))
    (dolist (animation (eval (read stream)))
      (ecs:make-object *storage* animation)))
  (if (zerop (logand (al::get-fs-entry-mode file)
                     (cffi:foreign-bitfield-value 'al::file-mode '(:isdir))))
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :skip)))

(defun load-sprites ()
  (let ((sprite-dir (al:create-fs-entry "../Resources/sprites")))
    (unwind-protect
         (= (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
            (al::for-each-fs-entry sprite-dir
                                   (cffi:callback load-sprite)
                                   (cffi:null-pointer)))
      (al:destroy-fs-entry sprite-dir))))
