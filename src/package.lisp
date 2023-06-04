(defpackage #:thoughtbound
  (:use #:cl)
  (:nicknames #:game)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria #:clamp #:define-constant #:format-symbol
                #:make-keyword #:non-negative-fixnum)
  (:export #:main))
