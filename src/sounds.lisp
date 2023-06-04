(in-package #:thoughtbound)


(ecs:defcomponent sound-sample
  (name :|| :type keyword :index sound :unique t)
  (sample (cffi:null-pointer) :type cffi:foreign-pointer))

(ecs:defcomponent sound
  (sample (cffi:null-pointer) :type cffi:foreign-pointer)
  (sample-instance (cffi:null-pointer) :type cffi:foreign-pointer)
  (play-time 0d0 :type double-float)
  (length 0.0 :type single-float)
  (playing 0 :type bit)
  (play-through 0 :type bit)
  (gain 0.0 :type single-float)
  (pan 0.0 :type single-float))

(ecs:defsystem sound
  (:components-rw (sound)
   :arguments ((:dt double-float)))
  (let ((playing (plusp sound-playing)))
    (when playing
      (incf sound-play-time dt)
      (when (> sound-play-time sound-length)
        (setf sound-play-time 0d0)
        (unless (eq (al:get-sample-instance-playmode sound-sample-instance)
                  :loop)
          (setf sound-playing 0
                playing nil))))
    (unless (eq playing (al:get-sample-instance-playing sound-sample-instance))
      (al:set-sample-instance-playing sound-sample-instance playing))
    (when playing
      (al:set-sample-instance-gain sound-sample-instance sound-gain)
      (al:set-sample-instance-pan sound-sample-instance sound-pan))))

(declaim (inline distance))
(defun distance (x1 y1 x2 y2)
  (let ((dx (- x1 x2))
        (dy (- y1 y2)))
    (+ (* dx dx) (* dy dy))))

(ecs:defsystem position-sound
  (:components-rw (sound)
   :components-ro (position))
  (let ((x position-x)
        (y position-y)
        (px (position-x-aref *storage* *player-entity*))
        (py (position-y-aref *storage* *player-entity*)))
    (setf sound-gain
          (clamp
           (- 1.0 (* (distance px py x y)
                     (/ +scale+ (* 0.5 +window-width+ +window-height+))))
           0.0 1.0)

          sound-pan
          (clamp
           (* (- x px)
              (/ +scale+ +window-width+))
           -1.0 1.0))))

(defun add-sound (storage entity name &key (oncep t) (playingp t) throughp)
  (let ((sample-entity (sound-entity storage name)))
    (with-sound-sample (_ source-sample) storage sample-entity
      (unless (has-sound-p storage entity)
        (make-sound storage entity))
      (with-sound () storage entity
        (when (or (zerop playing)
                  (and (zerop play-through)
                       (not (eq source-sample sample))))
          (unless (cffi:null-pointer-p sample-instance)
            (al:stop-sample-instance sample-instance))
          (let ((instance (al:create-sample-instance source-sample)))
            (setf sample source-sample
                  sample-instance instance
                  length (al:get-sample-instance-time instance)
                  playing (if playingp 1 0)
                  play-through (if throughp 1 0)
                  play-time 0d0)
            (al:attach-sample-instance-to-mixer instance (al:get-default-mixer))
            (al:set-sample-instance-playmode instance
                                             (if oncep :once :loop))))))))

(cffi:defcallback load-sound :int
    ((file (:pointer (:struct al::fs-entry))) (data :pointer))
  (declare (ignore data))
  (if (zerop (logand (al::get-fs-entry-mode file)
                     (cffi:foreign-bitfield-value 'al::file-mode '(:isdir))))
      (let ((filename (al::get-fs-entry-name file)))
        (ecs:make-object *storage*
                         `((:sound-sample
                            :name ,(make-keyword
                                    (string-upcase
                                     (pathname-name filename)))
                            :sample
                            ,(al:ensure-loaded #'al:load-sample filename))))
        (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok))
      (cffi:foreign-enum-value 'al::for-each-fs-entry-result :skip)))

(defun load-sounds ()
  (let ((sounds-dir (al:create-fs-entry "../Resources/sounds")))
    (unwind-protect
         (= (cffi:foreign-enum-value 'al::for-each-fs-entry-result :ok)
            (al::for-each-fs-entry sounds-dir
                                   (cffi:callback load-sound)
                                   (cffi:null-pointer)))
      (al:destroy-fs-entry sounds-dir))))
