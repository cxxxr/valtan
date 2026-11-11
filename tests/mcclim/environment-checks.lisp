(in-package #:cl-user)

#+node
(let* ((name "VALTAN_MCCLIM_ENV_CHECK")
       (key (ffi:cl->js name))
       (previous (system:environment-op :getenv key)))
  (unwind-protect
       (progn
         (system:environment-op :setenv key (ffi:cl->js "日本語の値"))
         (assert (string= (clim-internals::get-environment-variable name)
                         "日本語の値"))
         (system:environment-op :setenv key nil)
         (assert (null (clim-internals::get-environment-variable name))))
    (system:environment-op :setenv key previous)))
#-node
(assert (null (clim-internals::get-environment-variable "VALTAN_MCCLIM_ENV_CHECK")))
(write-line "MCCLIM-ENVIRONMENT-CHECKS-PASS")
