(push :ecs-unsafe *features*)
(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(ql:quickload '(:deploy :thoughtbound))
(asdf:make :thoughtbound)
