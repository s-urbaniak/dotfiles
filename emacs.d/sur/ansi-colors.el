(provide 'display-ansi-colors)
(require 'ansi-color)
(defun su/display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
