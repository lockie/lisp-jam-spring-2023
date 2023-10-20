(in-package #:thoughtbound)


(ecs:defcomponent ai
  (seen 0 :type bit)
  (notice-range 0.0 :type single-float
                    :documentation "Square of notice distance in pixels")
  (attack-range 0.0 :type single-float
                    :documentation "Square of attack distance in pixels"))

(ecs:defsystem ai
  (:components-ro (position)
   :components-rw (ai character)
   :enable (not *deathp*)
   :with ((player-x player-y)
          :of-type (single-float single-float)
          := (with-position () *player-entity*
               (values x y))))
  (let ((player-distance (distance position-x position-y
                                   player-x player-y)))
    (when (< player-distance ai-notice-range)
      (setf character-target-x player-x
            character-target-y player-y)
      (when (zerop ai-seen)
        (add-sound entity :gotcha :throughp t)
        (setf ai-seen 1)))
    (when (< player-distance ai-attack-range)
      (setf *deathp* t)
      (change-animation
       entity :idle
       :turn-left (plusp (animation-state-left entity)))
      (let ((old-width (size-width *player-entity*)))
        (change-animation
         *player-entity* :death
         :turn-left (plusp
                     (animation-state-left *player-entity*))
         :cycle nil)
        (let ((new-width (size-width *player-entity*)))
          ;; compensate for different sprite sizes
          (decf (position-x *player-entity*)
                (- new-width old-width))))
      (add-sound *player-entity* :death))))
