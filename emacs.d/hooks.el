(add-hook 'html-mode-hook 'su/html-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'su/cxx-mode-hook)
(add-hook 'c-mode-hook 'su/cxx-mode-hook)
(add-hook 'js-mode-hook 'su/js-mode-hook)
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

(defun su/tide-mode-hook()
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode t))

(defun su/go-mode-hook ()
  (subword-mode)
  (eldoc-mode)
  (company-mode)
  (go-eldoc-setup)
  (go-guru-hl-identifier-mode)
  (lambda () (local-set-key (kbd "C-h f") #'godoc-at-point))
  (lambda () (local-set-key (kbd "M-.") #'go-guru-definition)))

(defun su/js-mode-hook()
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
  (company-mode)
  (terraform-format-on-save-mode))
