(unless (find-package "SYSTEM")
  (make-package "SYSTEM"))

(unless (find-package "FFI")
  (make-package "FFI"))
