(in-package #:thoughtbound)


(ecs:defcomponent win)

(ecs:defsystem win
  (:components-ro (win position))
  (with-position (player-x player-y) *storage* *player-entity*
    (when (< (distance player-x player-y position-x position-y)
             (* 4f0 +scaled-tile-size+ +scaled-tile-size+))
      (al:show-native-message-box (cffi:null-pointer)
                                  "You won!" "" "You've beat the game!"
                                  (cffi:null-pointer) 0)
      (setf *quit* t))))
