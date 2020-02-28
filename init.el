;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(package-initialize)
(setq use-package-always-ensure t)

;; (setq custom-file "~/.emacs.d/custom-settings.el")
;; (load custom-file t)

;; GNU ELPA is already part of package-archives, add melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)

;; Install use-package if it is not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
	     :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; UI
(setq inhibit-startup-screen t
      frame-title-format "%b"
      visible-bell 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode)
(show-paren-mode)

;; UX
(global-auto-revert-mode)
(winner-mode)
(windmove-default-keybindings)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

;; Projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))
