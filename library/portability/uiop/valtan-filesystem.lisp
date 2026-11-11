(in-package :uiop/filesystem)
(defun delete-empty-directory (path)
  (let ((path (ensure-directory-pathname path)))
    (system:file-op :delete-directory (ffi:cl->js (native-namestring path)) path)))
