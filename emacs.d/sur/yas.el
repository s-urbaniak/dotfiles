(provide 'yas)

(defun su/yas/camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(defun su/yas/g/functify (s)
  (replace-regexp-in-string "-" "_" s))

(defun su/yas/g/namespace (s)
  (let* ((parts (split-string s "_")))
    (car parts)))

(defun su/yas/g/class (s)
  (let* ((parts (split-string s "_")))
    (mapconcat 'identity (cdr parts) "_")))

(defun su/yas/space (s)
  (make-string (length s) ?\s))
