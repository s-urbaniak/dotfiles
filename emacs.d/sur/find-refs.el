(provide 'find-refs)

(defun find-refs ()
  (interactive)
  (let ((cmd (concat "go-find-references "
                     "-file " (buffer-file-name) " "
                     ;; "-root " (getenv "GOPATH") " "
                     "-offset " (number-to-string (1- (byte-to-position (point))))
                     ))
        (buf-name "*find-refs-output*"))
    (get-buffer-create buf-name)
    (let ((message-log-max nil))
      (shell-command cmd buf-name))
    (switch-to-buffer-other-window buf-name)
    (special-mode)
    (compilation-mode)
    ))
