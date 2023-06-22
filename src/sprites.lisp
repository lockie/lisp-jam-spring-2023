(in-package #:thoughtbound)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (single-float single-float) fixnum) tile-index))
  (defun tile-index (x y)
    (let ((x* (truncate x))
          (y* (truncate y)))
      ;; NOTE: negative map coords are not supported
      (declare (type (integer 0 2147483647) x* y*))
      (logior (ash x* 32) y*))))

(ecs:defcomponent position
  "The object position in pixels."
  (x 0.0 :type single-float
         :documentation "pre-scaled x position aka screen pixel coordinate")
  (y 0.0 :type single-float
         :documentation "pre-scaled y position aka screen pixel coordinate")
  (tile-index (tile-index x y)
              :type fixnum :index tile
              :documentation "Tile index, for fast map tile lookups."))

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
  (repeat 1 :type bit :documentation "1 if the animation is looping")
  (left 0 :type bit :documentation "1 if sprite should be flipped left"))

(define-constant +tint-color+ (al:map-rgba 255 255 255 255) :test #'equalp)

(ecs:defsystem render-animation
  (:components-ro (position size sprite-sheet animation-state)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-tinted-scaled-rotated-bitmap-region
   sprite-sheet-bitmap
   (+ animation-state-x
      (* animation-state-frame size-width))
   animation-state-y
   size-width
   size-height
   +tint-color+
   0 0
   position-x
   position-y
   +scale+
   +scale+
   0.0
   (if (zerop animation-state-left)
       0
       (cffi:foreign-bitfield-value
        'al::draw-flags
        '(:flip-horizontal)))))

(ecs:defsystem update-animation
  (:components-rw (animation-state)
   :arguments ((:dt double-float)))
  (incf animation-state-time dt)
  (when (> animation-state-time
           animation-state-speed)
    (multiple-value-bind (nframes rest-time)
        (floor animation-state-time animation-state-speed)
      (declare (type non-negative-fixnum nframes))
      (setf animation-state-time rest-time)
      (incf animation-state-frame nframes)
      (when (>= animation-state-frame animation-state-nframes)
        (setf animation-state-frame
              (if (zerop animation-state-repeat)
                  (1- animation-state-nframes)
                  (rem animation-state-frame animation-state-nframes)))))))

(defun change-animation (entity animation &key turn-left (cycle t))
  (with-animation-state () entity
    (setf left (if turn-left 1 0)
          repeat (if cycle 1 0))
    (unless (eq animation current)
      (setf current animation
            frame 0
            time 0d0)
      (let* ((animation-name (format-symbol :keyword "~a-~a" sprite animation))
             (animation-entity (animation-entity animation-name)))
        (with-animation
            (_ animation-x animation-y animation-nframes animation-speed)
            animation-entity
          (setf x animation-x
                y animation-y
                nframes animation-nframes
                speed animation-speed))
        (replace-size entity animation-entity)
        (replace-sprite-sheet entity animation-entity))))
  nil)

(cffi:defcallback load-sprite :int
    ((file (:pointer (:struct al::fs-entry))) (data :pointer))
  (declare (ignore data))
  (if (zerop (logand (al::get-fs-entry-mode file)
                     (cffi:foreign-bitfield-value 'al::file-mode '(:isdir))))
      (with-open-file (stream (al::get-fs-entry-name file))
        (dolist (animation (eval (read stream)))
          (ecs:make-object animation))
        (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok))
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :skip)))

(defun load-sprites ()
  (let ((sprite-dir (al:create-fs-entry "../Resources/sprites")))
    (unwind-protect
         (= (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
            (al::for-each-fs-entry sprite-dir
                                   (cffi:callback load-sprite)
                                   (cffi:null-pointer)))
      (al:destroy-fs-entry sprite-dir))))
