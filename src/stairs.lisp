(in-package #:thoughtbound)


(ecs:defcomponent stairs
  (level "" :type string))

(define-constant +stairs-position-threshold+ 3000)

(ecs:defsystem stairs
  (:components-ro (stairs position)
   :with ((player-x player-y)
          :of-type (single-float single-float)
          := (with-position () *storage* *player-entity*
               (values x y))))
  (when (< (distance position-x position-y player-x player-y)
           +stairs-position-threshold+)
    (setf *restart* stairs-level)
    (delete-position *storage* entity)))
