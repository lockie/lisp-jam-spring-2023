(in-package #:thoughtbound)


(ecs:defcomponent ui
  (active 0 :type bit :index active-ui)
  (text "" :type string :documentation "Text, 4 rows by 94 chars max")
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8)))

(define-constant +window-background-path+ "../Resources/images/window.png"
  :test #'string=)
(defvar *window-background*)
(define-constant +button-background-path+ "../Resources/images/button.png"
  :test #'string=)
(defvar *button-background*)
(define-constant +button-background2-path+ "../Resources/images/button2.png"
  :test #'string=)
(defvar *button-background2*)

(ecs:defsystem ui
  (:components-rw (ui)
   :enable (and (not (cffi:null-pointer-p *ui-context*))
                (not *deathp*)
                (not *restart*))
   :when (plusp ui-active))
  (nk:with-color (text-color :r ui-r :g ui-g :b ui-b :a 255)
    (nk:with-styles *ui-context*
        ((:item nk:+style-window-fixed-background+
                (nk:style-item-image *window-background*))
         (:item nk:+style-button-normal+
                (nk:style-item-image *button-background*))
         (:item nk:+style-button-hover+
                (nk:style-item-image *button-background*))
         (:item nk:+style-button-active+
                (nk:style-item-image *button-background2*))
         (:color nk:+style-text-color+ text-color))
      (when (plusp
             (nk:begin
              *ui-context* (format nil "text ~a" entity)
              '(nk::x 5f0 nk::y 445f0 nk::w 1270f0 nk::h 270f0) 0))
        (nk:layout-row-static *ui-context* 64f0 1250 1)
        (nk:layout-space-begin *ui-context* 1 28f0 1)
        ;; HACK: nk:label-wrap is very slow for some reason, hence this
        (loop :for s :in (uiop:split-string ui-text :separator '(#\newline)
                                                    :max 4)
              :do (nk:layout-space-push
                   *ui-context* '(nk::x 66f0 nk::y 0f0 nk::w 1120f0 nk::h 30f0))
                  (nk:label *ui-context* s 17))
        (nk:layout-space-push *ui-context*
                              '(nk::x 1000f0 nk::y 2f0 nk::w 180f0 nk::h 36f0))
        (when (or (plusp (nk:button-label *ui-context* "Continue"))
                  (al:with-current-keyboard-state keyboard-state
                    (al:key-down keyboard-state :space)))
          (setf ui-active 0))
        (nk:layout-space-end *ui-context*))
      (nk:end *ui-context*))))

(ecs:defsystem you-died
  (:components-ro (player)
   :enable  (and (not (cffi:null-pointer-p *ui-context*))
                 *deathp*))
  (nk:with-styles *ui-context*
      ((:item nk:+style-window-fixed-background+
              (nk:style-item-image *window-background*))
       (:item nk:+style-button-normal+
              (nk:style-item-image *button-background*))
       (:item nk:+style-button-hover+
              (nk:style-item-image *button-background*))
       (:item nk:+style-button-active+
              (nk:style-item-image *button-background2*)))
    (when (plusp
           (nk:begin
            *ui-context* "death"
            '(nk::x 5f0 nk::y 445f0 nk::w 1270f0 nk::h 270f0) 0))
      (nk:layout-row-static *ui-context* 64f0 1250 1)
      (nk:layout-space-begin *ui-context* 1 28f0 1)
      (nk:layout-space-push
       *ui-context* '(nk::x 66f0 nk::y 0f0 nk::w 1120f0 nk::h 30f0))
      (nk:label *ui-context* "You died." 17)
      (nk:layout-space-end *ui-context*)
      (nk:layout-row-static *ui-context* 90f0 1250 1)
      (nk:layout-space-begin *ui-context* 1 36f0 2)
      (nk:layout-space-push *ui-context*
                            '(nk::x 800f0 nk::y 2f0 nk::w 180f0 nk::h 36f0))
      (when (plusp (nk:button-label *ui-context* "RAGEQUIT!"))
        (uiop:quit))
      (nk:layout-space-push *ui-context*
                            '(nk::x 1000f0 nk::y 2f0 nk::w 180f0 nk::h 36f0))
      (when (or (plusp (nk:button-label *ui-context* "Continue"))
                (al:with-current-keyboard-state keyboard-state
                  (al:key-down keyboard-state :space)))
        (setf *restart* t))
      (nk:layout-space-end *ui-context*))
    (nk:end *ui-context*)))

(define-constant +lore-position-threshold+ 3000)

(ecs:defsystem lore
  (:components-rw (ui)
   :components-ro (position)
   :with ((player-x player-y)
          :of-type (single-float single-float)
          := (with-position () *storage* *player-entity*
               (values x y))))
  (when (< (distance position-x position-y player-x player-y)
           +lore-position-threshold+)
    (setf ui-active 1)
    (delete-position *storage* entity)))
