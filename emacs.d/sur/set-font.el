(provide 'set-font)

(defun su/setfont ()
  (interactive)
  (setq su/font (concat "Monospace-" (read-string "Size: ")))
  (set-frame-font su/font)
  (add-to-list 'default-frame-alist
               `(font . ,su/font))
  (spaceline-install))
