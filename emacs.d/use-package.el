(use-package toml-mode
  :defer t)

(use-package elixir-mode
  :defer t)

(use-package robe
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

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
  ("C-b" . neotree-find)
  :config
  (setq neo-window-width 40)
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'arrow)
  (setq neo-force-change-root t))

(use-package markdown-mode+
  :defer t)

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
  (setq ffip-find-options "-not -iwholename './vendor/*' -not -regex '.*Godeps.*' -not -regex '.*build-rkt.*'"))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-v" . er/expand-region))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")

  (add-hook 'before-save-hook 'gofmt-before-save)

  (add-hook 'go-mode-hook
            (lambda ()
              (subword-mode)
              (eldoc-mode)
              (company-mode)
              (local-set-key (kbd "C-h f") #'godoc-at-point))))

(use-package go-guru
  :after go-mode)
  
(use-package go-rename
  :defer t
  :after go-mode)

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

(use-package ivy
  :bind
  ("C-x b" . ivy-switch-buffer)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package rg
  :defer t
  :config
  (setq rg-custom-type-aliases
        '(("tf" . "*.tf")
          ("jsonnet" . "*.jsonnet *.libsonnet"))))

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

(use-package systemd
  :mode "/systemd/\\(?:.\\|\n\\)+?\\.d/[^/]+?\\.conf\\'")

(use-package terraform-mode
  :mode "\\.tf\\(vars\\)?\\'"
  :config
  (add-hook 'terraform-mode-hook 'su/terraform-mode-hook))

(use-package prettier-js
  :defer t
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'ts-mode-hook 'prettier-js-mode)
  (setq prettier-js-args
        '("--trailing-comma" "all"
          "--bracket-spacing" "false"
          "--single-quote" "false"
          "--bracket-spacing" "true")))

(use-package go-fill-struct
  :defer t)

(use-package flycheck
  :config
  (setq flycheck-go-build-install-deps t))

(use-package rust-mode
  :defer t)

(use-package racer
  :defer t
  :after rust-mode)

(use-package json-mode
  :defer t
  :config
  (setq js-indent-level 2))

(use-package jsonnet-mode
  :mode ("jsonnet" "libsonnet"))

(use-package lsp-mode
  :config
  (require 'lsp-imenu)

  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (lsp-define-stdio-client lsp-python "python"
                           #'ffip-project-root
                           '("pyls"))

  (add-hook 'python-mode-hook
            (lambda () (lsp-python-enable))))

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends))

(use-package lsp-go
  :after lsp-mode
  :config
  (add-hook 'go-mode-hook 'lsp-go-enable))

(use-package lsp-javascript-typescript
  :after lsp-mode
  :config
  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
  (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
  )

(use-package typescript-mode
  :defer t
  :config
  (add-hook 'typescript-mode-hook 'company-mode)
  (setq typescript-indent-level 2))

(use-package smex
  :defer t)

(use-package lsp-rust
  :after lsp-mode
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'flycheck-mode))
