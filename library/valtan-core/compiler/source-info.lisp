(in-package :compiler)

(defstruct (source-info-object (:constructor make-si-object (position value))
			       (:conc-name si-object))
  position
  value)
