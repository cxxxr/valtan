(let ((scanner (cl-ppcre:create-scanner "([a-z]+)-([0-9]+)"
                                        :case-insensitive-mode t)))
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings scanner "Order: ITEM-42")
    (assert (string= match "ITEM-42"))
    (assert (equalp groups #("ITEM" "42")))
    (format t "Match: ~A, captures: ~S~%" match groups)))

(let ((parts (cl-ppcre:split "[,;]+" "東京,大阪;;京都"))
      (replaced (cl-ppcre:regex-replace-all "[0-9]+" "item-42 / item-7" "#")))
  (assert (equal parts '("東京" "大阪" "京都")))
  (assert (string= replaced "item-# / item-#"))
  (format t "Split: ~S~%Replace: ~A~%" parts replaced))
