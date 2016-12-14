(require 'package)
(setq package-user-dir "~/.cache/emacs/elpa")

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
(blink-cursor-mode -1)

;; disable fringes and scrollbars
(set-fringe-mode 0)
(scroll-bar-mode -1)

(global-font-lock-mode nil)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

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

(setq-default indent-tabs-mode nil) ;; don't insert tabs (only whitespace)
(setq-default tab-width 4) ;; show <TAB> characters as 4 whitespaces
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

(add-to-list 'load-path (expand-file-name "sur" user-emacs-directory))
(require 'direnv)

(use-package danneskjold-theme)
(load-theme 'danneskjold t)

(use-package neotree
  :config
  (setq neo-window-width 40)
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'arrow)
  (global-set-key (kbd "C-c b") 'neotree-toggle))

(use-package markdown-mode+)

(use-package yaml-mode)

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package yasnippet)

(use-package linum
  :config
  (setq linum-format "%d "))

(use-package find-file-in-project
  :config
  (setq ffip-find-options "-not -regex '.*Godeps.*' -not -regex '.*build-rkt.*'")
  (bind-key "C-x f" 'find-file-in-project))

(use-package expand-region
  :config
  (bind-key "C-=" 'er/expand-region))

(use-package go-rename)

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'su/go-mode-hook))

(use-package go-guru
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (set-face-attribute 'go-guru-hl-identifier-face nil
                      :inherit 'custom-comment))

(use-package company
  :config
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (bind-key "M-/" 'company-complete-common)
  (add-hook 'lisp-mode-hook 'company-mode))

(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go))

(use-package rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (add-to-list 'company-backends 'company-rtags)
  (rtags-diagnostics))

(use-package zoom-frm
  :config
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

(use-package tide
  :config
  (add-hook 'typescript-mode-hook 'su/tide-mode-hook))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

(use-package magit)

(add-hook 'html-mode-hook 'su/html-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'su/lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'su/cxx-mode-hook)
(add-hook 'c-mode-hook 'su/cxx-mode-hook)
(add-hook 'js-mode-hook 'su/js-mode-hook)
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load (expand-file-name "local.el" user-emacs-directory))
