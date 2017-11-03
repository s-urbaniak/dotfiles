(setq su/font "Monospace-10")

;; set default font
(set-frame-font su/font)

;; set default font in newly opened frames
(add-to-list 'default-frame-alist
             `(font . ,su/font))

;; disable menu bar
(menu-bar-mode -1)
;; highlight matching parens
(show-paren-mode t)
;; no blinking cursor
(blink-cursor-mode -1)
;; use xterm mouse
(xterm-mouse-mode t)
;; saves the location of the point when you kill a buffer and returns to it next time you visit the associated file.
(save-place-mode t)

;; these settings is only applicable if started in a graphical environment
(when (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (set-fringe-mode 0)
    (scroll-bar-mode -1)))

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

;; disable select on focus for buffers
(setq mouse-autoselect-window nil)

;; disable startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

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
(require 'yas)
(require 'clear-theme)
(autoload 'gtk-lookup-symbol "gtk-look" nil t)

(require 'cl)
(require 'cl-macs)

;; installed locally
(when (require 'rtags nil 'noerror)
  (require 'company)
  (push 'company-rtags company-backends)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (setq rtags-rc-log-enanabled t))

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

(global-set-key
 (kbd "C-,")
 'switch-to-prev-buffer)

(global-set-key
 (kbd "C-.")
 'switch-to-next-buffer)

(defface egoge-display-time
  '((((type x w32 mac))
     (:inherit bold)))
  "Face used to display the time in the mode line.")

(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                    'face 'egoge-display-time)))

(display-time-mode t)

(require 'server)
(if (not (server-running-p))
    (server-start))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
