(in-package :cl-test)
(deftest never-finishes (do () (nil)) t)
