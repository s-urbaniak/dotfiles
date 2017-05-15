;; kill scratch buffer at the very beginning, I barely need it and it prevents auto-loading of packages
(kill-buffer "*scratch*")

;; use the Emacs' package infrastructure, like melpa
(require 'package)

;; store downloaded packages in the cache directory rather than in ~/.emacs.d/elpa
(setq package-user-dir "~/.cache/emacs/elpa")

;; use https rather than the default http locations
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package is the only package which has to be bootstrapped
(dolist (package '(use-package))
  (unless (package-installed-p package)
	(package-install package)))

(setq use-package-always-ensure t)
(setq use-package-verbose nil)

(set-frame-font "Monospace-10")
(menu-bar-mode -1)

;; highlight matching parens
(show-paren-mode 1)
;; no blinking cursor
(blink-cursor-mode -1)

;; this is only applicable if started in a graphical environment
(tool-bar-mode -1)
(set-fringe-mode 0)
(scroll-bar-mode -1)

;; if a file changed on disk, load (revert) its content
(global-auto-revert-mode t)

;; typed text replaces selected text
(delete-selection-mode t)

;; configure mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(global-set-key (kbd "<mouse-7>") '(lambda ()
                                     (interactive)
                                     (scroll-left 4)))

(global-set-key (kbd "<mouse-6>") '(lambda ()
                                     (interactive)
                                     (scroll-right 4)))

;; select on focus for buffers
(setq mouse-autoselect-window t)

;; disable startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-buffer-choice "*notes*")

;; disable automatic backups
(setq make-backup-files nil)
(setq locate-command "mdfind")
(setq ring-bell-function 'ignore)
(setq set-mark-command-repeat-pop t)
(setq frame-title-format '("" " %b"))

(setq-default indent-tabs-mode nil) ;; don't insert tabs (only whitespace)
(setq-default tab-width 4) ;; show <TAB> characters as 4 whitespaces
(setq global-visual-line-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(bind-key "<M-up>" 'windmove-up)
(bind-key "<M-down>" 'windmove-down)
(bind-key "<M-left>" 'windmove-left)
(bind-key "<M-right>" 'windmove-right)

(global-set-key (kbd "C-x C-b") 'buffer-menu)

(add-to-list 'load-path (expand-file-name "sur" user-emacs-directory))
(require 'direnv)
(require 'yas)
(autoload 'gtk-lookup-symbol "gtk-look" nil t)

(use-package esup
  :defer t)

(use-package spacemacs-theme)
(load-theme 'spacemacs-light t)

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package list-register
  :bind
  ("C-x r v" . list-register))

(use-package neotree
  :bind
  ("C-c b" . neotree-toggle)
  :config
  (setq neo-window-width 40)
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'arrow)
  (setq neo-force-change-root t))

(use-package markdown-mode+
  :mode ("\\.markdown\\'" "\\.md\\'"))

(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package linum
  :defer t
  :config
  (setq linum-format "%d "))

(use-package find-file-in-project
  :bind
  ("C-x f" . find-file-in-project)  
  :config
  (setq ffip-find-options "-not -regex '.*Godeps.*' -not -regex '.*build-rkt.*'"))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-v" . er/expand-region))

(use-package go-rename
  :defer t)

(use-package go-eldoc
  :after go-mode)

(use-package go-guru
  :after go-mode
  :config
  (set-face-attribute 'go-guru-hl-identifier-face nil
                      :inherit 'isearch))

(use-package go-mode
  :mode "\\.go\\'"
  :bind
  ("C-h f" . godoc-at-point)
  ("M-." . godef-jump)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'su/go-mode-hook))

(use-package company
  :bind
  ("M-/" . company-complete-common)
  :config
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (add-hook 'lisp-mode-hook 'company-mode))

(use-package company-go
  :after go-mode
  :config
  (add-to-list 'company-backends 'company-go))

(use-package zoom-frm
  :bind
  ("C-x C--" . zoom-in/out)
  ("C-x C-=" . zoom-in/out))

(use-package tide
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (add-hook 'typescript-mode-hook 'su/tide-mode-hook))

(use-package ivy
  :bind
  ("C-x b" . ivy-switch-buffer)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package smex
  :defer t)

(use-package ag
  :defer t)

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  :config
  (counsel-mode 1)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper
  :bind
  ("\C-s" . swiper))

(use-package magit
  :defer t)

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package systemd
  :mode
  ("/systemd/\\(?:.\\|\n\\)+?\\.d/[^/]+?\\.conf\\'" . systemd-mode))

(use-package terraform-mode
  :mode "\\.tf\\(vars\\)?\\'"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package js2-mode
  :mode "\\.jsm?\\'"
  :interpreter "node"
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

;; installed locally
(when (require 'rtags nil 'noerror)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (setq rtags-rc-log-enanabled t))

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
  (go-guru-hl-identifier-mode))

(defun su/js-mode-hook()
  (subword-mode)
  (setq js-indent-level 2)
  (setq indent-tabs-mode t))

(defun su/cxx-mode-hook()
  (eldoc-mode)
  (company-mode)
  (gnome-c-style-mode)
  (setq indent-tabs-mode nil))

(defun su/html-mode-hook()
  (set (make-local-variable 'sgml-basic-offset) 4))

(global-set-key
 "\C-n"
 (lambda ()
   (interactive)
   (scroll-up 4)))

(global-set-key
 "\C-p"
 (lambda ()
   (interactive)
   (scroll-down 4)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load (expand-file-name "local.el" user-emacs-directory))

(require 'server)
(if (not (server-running-p))
    (server-start))
