(add-hook 'emacs-lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'su/cxx-mode-hook)
(add-hook 'c-mode-hook 'su/cxx-mode-hook)
(add-hook 'isearch-mode-end-hook 'su/goto-match-beginning)

(defun su/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defun su/toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun su/lisp-mode-hook()
  (company-mode)
  (turn-on-eldoc-mode))

(defun su/js-mode-hook()
  (tern-mode)
  (company-mode)
  (subword-mode)
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

(defun su/cxx-mode-hook()
  (eldoc-mode)
  (company-mode)
  (setq eldoc-documentation-function 'rtags-eldoc)
  (setq indent-tabs-mode nil))

(defun su/html-mode-hook()
  (set (make-local-variable 'sgml-basic-offset) 4))

(defun su/terraform-mode-hook()
  (terraform-format-on-save-mode))
