(provide 'clear-theme)

(defun su/clear-theme ()
  (interactive)
  (mapc 'disable-theme custom-enabled-themes)
  (spaceline-emacs-theme))
