(in-package #:lisp-jam-spring-2023)


(define-constant +tile-size+ 16)
(define-constant +scaled-tile-size+ (* +scale+ +tile-size+))

(ecs:defcomponent player)

(ecs:defcomponent character
  (speed 0.0 :type single-float)
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float))

(defun advance-tile (x &optional (increment 1))
  (* +scaled-tile-size+
     (+ increment 0.5
        (floor x +scaled-tile-size+))))

(ecs:defsystem player-control
  (:components-ro (player position)
   :components-rw (character))
  (al:with-current-keyboard-state keyboard-state
    (when (al:key-down keyboard-state :w)
      (setf character-target-y
            (advance-tile position-y -1)))
    (when (al:key-down keyboard-state :a)
      (setf character-target-x
            (advance-tile position-x -1)))
    (when (al:key-down keyboard-state :s)
      (setf character-target-y
            (advance-tile position-y 1)))
    (when (al:key-down keyboard-state :d)
      (setf character-target-x
            (advance-tile position-x 1)))))

(declaim (inline approx-equal)
         (ftype (function (single-float single-float &optional single-float)
                          boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(ecs:defsystem character
  (:components-rw (position character)
   :arguments ((:dt double-float)))
  (if (and (approx-equal position-x character-target-x)
           (approx-equal position-y character-target-y))
      (change-animation
       *storage* entity :idle
       (not (zerop (animation-state-left-aref *storage* entity))))
      (let* ((angle (atan (- character-target-y position-y)
                          (- character-target-x position-x)))
             (dt (coerce dt 'single-float))
             (dx (* dt character-speed (cos angle)))
             (dy (* dt character-speed (sin angle))))
        (change-animation *storage* entity :move (minusp dx))
        (incf position-x dx)
        (incf position-y dy))))
