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

(load-theme 'wombat t)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 150))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(provide 'ui-config)
