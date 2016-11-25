(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package is the only package which has to be bootstrapped
(dolist (package '(use-package))
  (unless (package-installed-p package)
	(package-install package)))

(setq use-package-always-ensure t)

(set-frame-font "Monospace-10")
(tool-bar-mode -1)
(menu-bar-mode -1)

(show-paren-mode 1)
(blink-cursor-mode 0)

;; disable fringes and scrollbars
(set-fringe-mode 0)
(scroll-bar-mode -1)

(global-font-lock-mode nil)
(global-auto-revert-mode t)

;; configure mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(global-set-key (kbd "<mouse-7>") '(lambda ()
                                     (interactive)
                                     (scroll-left 4)))

(global-set-key (kbd "<mouse-6>") '(lambda ()
                                     (interactive)
                                     (scroll-right 4)))

(setq mouse-autoselect-window t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq locate-command "mdfind")
(setq ring-bell-function 'ignore)
(setq set-mark-command-repeat-pop t)
(setq frame-title-format '("" " %b"))

;; (setq indent-line-function 'tab-to-tab-stop) ;; indent using tab stops
;; (setq tab-stop-list (number-sequence 8 200 8)) ;; define tab stops

(setq-default indent-tabs-mode nil) ;; don't insert tabs (only whitespace)
(setq-default tab-width 4) ;; show <TAB> characters as 4 whitespaces
;; (setq electric-indent-mode nil) ;; disable electric indent mode
(set-default 'truncate-lines t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(bind-key "<M-up>" 'windmove-up)
(bind-key "<M-down>" 'windmove-down)
(bind-key "<M-left>" 'windmove-left)
(bind-key "<M-right>" 'windmove-right)

(global-set-key (kbd "C-x C-b") 'buffer-menu)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(use-package sr-speedbar
  :init
  (setq helm-alive-p nil)
  :config
  (global-set-key (kbd "C-c b") 'sr-speedbar-toggle)
  (setq sr-speedbar-right-side nil
        sr-speedbar-auto-refresh nil
        speedbar-hide-button-brackets-flag t
        speedbar-show-unknown-files t
        speedbar-use-images nil
        speedbar-smart-directory-expand-flag t))

(use-package danneskjold-theme)
(load-theme 'danneskjold t)

(use-package markdown-mode+)

(use-package yaml-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package yasnippet)

(use-package ido
  :config
  (ido-mode t))

(use-package linum
  :init
  (setq linum-format "%d "))

(use-package magit
  :config
  (bind-key "C-x g" 'magit-status)
  (bind-key "C-c p" 'magit-find-file-completing-read))

(use-package find-file-in-project
  :config
  (setq ffip-find-options "-not -regex '.*Godeps.*' -not -regex '.*build-rkt.*'")
  (bind-key "C-x f" 'find-file-in-project))

(use-package expand-region
  :config
  (bind-key "C-=" 'er/expand-region))

(use-package go-rename)

(use-package go-eldoc
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package go-mode
  :defer t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "gofmt")
  :config
  (add-hook 'go-mode-hook 'su/go-mode-hook))

(use-package go-guru
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package company
  :init
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (bind-key "M-/" 'company-complete-common)
  :config
  (add-hook 'lisp-mode-hook 'company-mode))

(use-package company-go
  :init
  (add-to-list 'company-backends 'company-go))

(use-package rtags
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (add-to-list 'company-backends 'company-rtags)
  :config
  (rtags-diagnostics))

(use-package zoom-frm
  :config
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

(add-hook 'html-mode-hook 'su/html-mode-hook)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'su/cxx-mode-hook)
(add-hook 'c-mode-hook 'su/cxx-mode-hook)
(add-hook 'js-mode-hook 'su/js-mode-hook)

(defun su/go-mode-hook ()
  (subword-mode)
  (eldoc-mode)
  (company-mode)
  (go-guru-hl-identifier-mode)
  (local-set-key (kbd "C-c C-r") 'go-guru-referrers)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-h f") 'godoc-at-point))

(defun su/js-mode-hook()
  (subword-mode)
  (setq js-indent-level 4)
  (setq indent-tabs-mode t))

(defun su/cxx-mode-hook()
  (bind-key "M-." 'rtags-find-symbol-at-point)
  (bind-key "C-c C-r" 'rtags-find-references)
  (eldoc-mode)
  (company-mode)
  (setq indent-tabs-mode nil)
  (setq eldoc-documentation-function 'rtags-eldoc))

(defun su/html-mode-hook()
  (set (make-local-variable 'sgml-basic-offset) 4))

(add-to-list 'load-path (expand-file-name "sur" user-emacs-directory))
(require 'direnv)

(load (expand-file-name "local.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
