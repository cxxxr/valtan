(in-package :cl-user)
(unless (member :valtan-resumable *features*)
  (error "Bordeaux Threads requires :target-features (:valtan-resumable)."))
(pushnew :thread-support *features*)
