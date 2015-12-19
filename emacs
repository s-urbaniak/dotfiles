(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-list
	  '(find-file-in-project swiper go-autocomplete auto-complete popup go-rename go-mode gotest go-mode f dash s s magit-find-file dash magit magit-popup dash async git-commit with-editor dash async dash with-editor dash async dash async magit-popup dash async multiple-cursors neotree popup s swiper tango-plus-theme with-editor dash async yasnippet zenburn-theme))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'multiple-cursors)
(require 'ido)
(require 'magit-find-file)
(require 'yasnippet)

(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'zenburn t)
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
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq gofmt-command "goimports")
(setq locate-command "mdfind")
(setq ring-bell-function 'ignore)

(add-hook 'before-save-hook 'gofmt-before-save)

(load-file "~/src/go/src/golang.org/x/tools/cmd/oracle/oracle.el")
