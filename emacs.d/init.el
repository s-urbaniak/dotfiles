;; (package-initialize)
;; kill scratch buffer at the very beginning, I barely need it and it prevents auto-loading of packages
(kill-buffer "*scratch*")

(defun su/load-user-file (file)
  (interactive "f")
  (load-file (expand-file-name file user-emacs-directory)))

(su/load-user-file "package-init.el")
(su/load-user-file "global-settings.el")
(su/load-user-file "use-package.el")
(su/load-user-file "hooks.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
