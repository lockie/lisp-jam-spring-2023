(in-package #:lisp-jam-spring-2023)


(ecs:defcomponent ai
  (seen 0 :type bit)
  (notice-range 0.0 :type single-float
                    :documentation "Square of notice distance in pixels")
  (attack-range 0.0 :type single-float
                    :documentation "Square of attack distance in pixels"))

(ecs:defsystem ai
  (:components-ro (position)
   :components-rw (ai character))
  (unless *deathp*
    (multiple-value-bind (player-x player-y)
        (with-position () *storage* *player-entity*
          (values x y))
      (let ((player-distance (distance position-x position-y
                                       player-x player-y)))
        (when (< player-distance ai-notice-range)
          (setf character-target-x player-x
                character-target-y player-y)
          (when (zerop ai-seen)
            (add-sound *storage* entity :gotcha :throughp t)
            (setf ai-seen 1)))
        (when (< player-distance ai-attack-range)
          (setf *deathp* t)
          (change-animation
           *storage* entity :idle
           :turn-left (plusp
                       (animation-state-left-aref *storage* entity)))
          (let ((old-width (size-width-aref *storage* *player-entity*)))
            (change-animation
             *storage* *player-entity* :death
             :turn-left (plusp
                         (animation-state-left-aref *storage* *player-entity*))
             :cycle nil)
            (let ((new-width (size-width-aref *storage* *player-entity*)))
              ;; compensate for different sprite sizes
              (decf (position-x-aref *storage* *player-entity*)
                    (- new-width old-width))))
          (add-sound *storage* *player-entity* :death))))))
