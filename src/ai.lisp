(in-package #:lisp-jam-spring-2023)


(ecs:defcomponent ai
  (range 0.0 :type single-float
             :documentation "Square of activation distance in pixels"))

(ecs:defsystem ai
  (:components-ro (ai position)
   :components-rw (character))
  (multiple-value-bind (player-x player-y)
      (with-position () *storage* *player-entity*
        (values x y))
    (when (< (distance position-x position-y player-x player-y)
             ai-range)
      (setf character-target-x player-x
            character-target-y player-y))))
