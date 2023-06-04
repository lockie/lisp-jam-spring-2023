(in-package #:lisp-jam-spring-2023)


(ecs:defcomponent win)

(ecs:defsystem win
  (:components-ro (win position))
  (with-position (player-x player-y) *storage* *player-entity*
    (when (< (distance player-x player-y position-x position-y)
             (* 4f0 +scaled-tile-size+ +scaled-tile-size+))
      (al:show-native-message-box (cffi:null-pointer)
                                  "You win!" "" "Thanks for playing!"
                                  (cffi:null-pointer) 0)
      (setf *quit* t))))
