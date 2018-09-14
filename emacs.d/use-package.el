(use-package toml-mode)

(use-package elixir-mode)

(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package elpy
  :config
  (elpy-enable))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package pinentry
  :config
  (pinentry-start))

(use-package esup
  :defer t)

(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

(use-package direnv)

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package neotree
  :bind
  ("C-c b" . neotree-toggle)
  :config
  (setq neo-window-width 40)
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'arrow)
  (setq neo-force-change-root t))

(use-package markdown-mode+)

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
  ("M-." . go-guru-definition)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'su/go-mode-hook))

(use-package company
  :bind
  ("M-/" . company-complete-common)
  :config
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-everywhere t)
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (add-hook 'lisp-mode-hook 'company-mode))

(use-package company-go
  :after go-mode
  :config
  (add-to-list 'company-backends 'company-go))

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

(use-package rg
  :defer t
  :config
  (setq rg-custom-type-aliases
        '(("tf" . "*.tf") ("jsonnet" . "*.jsonnet *.libsonnet"))))

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
  (add-hook 'terraform-mode-hook 'su/terraform-mode-hook))

(use-package json-mode)

(use-package prettier-js
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (setq prettier-js-args
        '("--trailing-comma" "all"
          "--bracket-spacing" "false"
          "--single-quote" "true"
          "--bracket-spacing" "true")))

(use-package go-fill-struct)

(use-package flycheck
  :config
  (setq flycheck-go-build-install-deps t))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'su/rust-mode-hook))

(use-package racer
  :after rust-mode)

(use-package jsonnet-mode)
