(uiop:run-program "curl -LOs https://gitlab.com/lockie/cl-fast-ecs/-/archive/724a/cl-fast-ecs-724a.zip")
(uiop:run-program
 (format nil "7z x -o~a cl-fast-ecs-724a.zip" (merge-pathnames
                                               #P"quicklisp/local-projects/"
                                               (user-homedir-pathname))))
(ql:update-all-dists :prompt nil)
(setf (ql-dist:preference (ql-dist:find-dist "ultralisp")) most-positive-fixnum)
(push :ecs-unsafe *features*)
(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(ql:quickload '(#:lisp-jam-spring-2023 #:deploy))
(asdf:make :lisp-jam-spring-2023)
