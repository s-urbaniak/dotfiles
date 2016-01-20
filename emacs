(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-list
	  '(find-file-in-project go-autocomplete auto-complete go-rename go-mode gotest go-mode magit yasnippet zenburn-theme))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'ido)
(require 'yasnippet)

(set-frame-font "Monospace-9")
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'zenburn t)
;; (load-theme 'faff t)
(yas-global-mode 1)
(ido-mode t)
(show-paren-mode 1)

;; disable fringes and scrollbars
(set-fringe-mode 0)
(scroll-bar-mode -1)

(global-auto-complete-mode t)
(global-font-lock-mode 1)
(global-auto-revert-mode t)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c p") 'magit-find-file-completing-read)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key [f8] 'neotree-toggle)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq gofmt-command "goimports")
(setq locate-command "mdfind")
(setq ring-bell-function 'ignore)

(add-hook 'before-save-hook 'gofmt-before-save)

(load-file "~/src/go/src/golang.org/x/tools/cmd/oracle/oracle.el")
(server-start)
