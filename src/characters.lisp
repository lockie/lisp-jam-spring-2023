(in-package #:lisp-jam-spring-2023)


(ecs:defcomponent player
  (player 1 :type bit :index player :unique t))

(ecs:defcomponent character
  (speed 0.0 :type single-float)
  (target-x -1.0 :type single-float)
  (target-y -1.0 :type single-float))

(declaim (inline tile-start))
(defun tile-start (x y)
  (values
   (* +scaled-tile-size+ (round x +scaled-tile-size+))
   (* +scaled-tile-size+ (round y +scaled-tile-size+))))

(ecs:defsystem player-control
  (:components-ro (player position)
   :components-rw (character))
  (unless *deathp*
    (al:with-current-keyboard-state keyboard-state
      (multiple-value-bind (current-tile-x current-tile-y)
          (tile-start position-x position-y)
        (when (al:key-down keyboard-state :W)
          (setf character-target-y (- current-tile-y +scaled-tile-size+)))
        (when (al:key-down keyboard-state :A)
          (setf character-target-x (- current-tile-x +scaled-tile-size+)))
        (when (al:key-down keyboard-state :S)
          (setf character-target-y (+ current-tile-y +scaled-tile-size+)))
        (when (al:key-down keyboard-state :D)
          (setf character-target-x (+ current-tile-x +scaled-tile-size+)))))))

(declaim (inline approx-equal)
         (ftype (function (single-float single-float &optional single-float)
                          boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(declaim (ftype (function (single-float single-float) boolean) obstaclep))
(defun obstaclep (x y)
  (loop :for tile-entity :of-type ecs:entity :in
           (tile-entities *storage* (multiple-value-call #'tile-index
                                      (tile-start x y)))
        :thereis (plusp (tile-obstaclep-aref *storage* tile-entity))))

(declaim (ftype (function (single-float single-float non-negative-fixnum)
                          boolean) collidesp))
(defun collidesp (x y size)
  (loop :for i :of-type non-negative-fixnum :from 0 :below size
        :thereis (loop :for j :of-type non-negative-fixnum :from 0 :below size
                       :thereis (obstaclep (+ x (* i +scaled-tile-size+))
                                           (+ y (* j +scaled-tile-size+))))))

(ecs:defsystem character
  (:components-rw (position character size)
   :arguments ((:dt double-float)))
  (unless *deathp*
    (when (or (minusp character-target-x)
              (minusp character-target-y))
      (setf character-target-x position-x
            character-target-y position-y))
    (if (and (approx-equal position-x character-target-x)
             (approx-equal position-y character-target-y))
        (change-animation
         *storage* entity :idle
         :turn-left (plusp (animation-state-left-aref *storage* entity)))
        (let* ((angle (atan (- character-target-y position-y)
                            (- character-target-x position-x)))
               (dt (coerce dt 'single-float))
               (dx (* dt character-speed (cos angle)))
               (dy (* dt character-speed (sin angle)))
               (new-x (+ position-x dx))
               (new-y (+ position-y dy)))
          (if (collidesp new-x new-y (floor size-width +tile-size+))
              (setf character-target-x position-x
                    character-target-y position-y)
              (progn
                (change-animation *storage* entity :move :turn-left (minusp dx))
                (add-sound *storage* entity (if (= *player-entity* entity)
                                                :step-cloth1
                                                :step-lth1))
                (setf position-x new-x
                      position-y new-y)))))))
