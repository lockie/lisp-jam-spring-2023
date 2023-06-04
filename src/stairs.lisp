(in-package #:thoughtbound)


(ecs:defcomponent stairs
  (level "" :type string))

(define-constant +stairs-position-threshold+ 3000)

(ecs:defsystem stairs
  (:components-rw (stairs)
   :components-ro (position))
  (multiple-value-bind (player-x player-y)
      (with-position () *storage* *player-entity*
        (values x y))
    (when (< (distance position-x position-y player-x player-y)
             +stairs-position-threshold+)
      (setf *restart* stairs-level)
      (delete-position *storage* entity))))
