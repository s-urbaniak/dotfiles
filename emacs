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
(set-frame-font "Monospace-9")
(tool-bar-mode -1)
(menu-bar-mode -1)

(show-paren-mode 1)
(blink-cursor-mode 0)

;; disable fringes and scrollbars
(set-fringe-mode 0)
(scroll-bar-mode -1)

(global-font-lock-mode nil)
(global-auto-revert-mode t)

(setq js-indent-level 4)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq locate-command "mdfind")
(setq ring-bell-function 'ignore)

(custom-set-variables '(speedbar-show-unknown-files t))

(use-package colorsarenice-theme)

;; (load-theme 'colorsarenice-light t)
(load-theme 'colorsarenice-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'noctilux t)
;; (load-theme 'ample-light t)
;; (load-theme 'hydandata-light t)

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

(use-package go-eldoc
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :defer t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  :config
  (bind-key "M-." 'godef-jump)
  (add-hook 'go-mode-hook '(lambda()
							 (load-file "~/src/go/src/golang.org/x/tools/cmd/guru/go-guru.el")
							 (subword-mode)
							 (eldoc-mode)
							 (company-mode))))

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

(add-hook 'html-mode-hook 
		  '(lambda() 
			 (set (make-local-variable 'sgml-basic-offset) 4)))

(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(defun su/cxx-mode-hook()
  (bind-key "M-." 'rtags-find-symbol-at-point)
  (company-mode)
  (setq indent-tabs-mode nil))

(add-hook 'c++-mode-hook 'su/cxx-mode-hook)
(add-hook 'c-mode-hook 'su/cxx-mode-hook)
(add-hook 'js-mode-hook '(lambda()
                           (setq indent-tabs-mode t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
