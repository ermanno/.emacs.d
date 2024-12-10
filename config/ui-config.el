(setq inhibit-startup-screen t
      frame-title-format "%b"
      visible-bell 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(show-paren-mode)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  (set-face-attribute 'default nil :height 150))

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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(provide 'ui-config)
