(setq inhibit-startup-screen t
      frame-title-format "%b"
      visible-bell 1
      inhibit-x-resources 't)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(if (display-graphic-p)
    (menu-bar-mode 1)  ; Show menu bar in GUI
  (menu-bar-mode -1))  ; Hide menu bar in terminal
(column-number-mode)
(show-paren-mode)

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi :no-confirm)

  (let ((font-height
         (if (eq system-type 'darwin)
             150
           118)))

    (set-face-attribute 'default nil :height font-height))
  )

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'automatic)
  (sml/name-width 32)
  (sml/shorten-modes nil)
  (sml/replacer-regexp-list nil)
  :config (sml/setup))

(provide 'ui-config)
