;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(package-initialize)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Add config directory to load path
(setq config-dir
      (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path config-dir)

;; GNU ELPA is already part of package-archives, add melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it is not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(require 'ui-config)
(require 'ux-config)
(require 'mac-config)
(require 'git-config)
(require 'tree-sitter-config)
(require 'rust-config)
(require 'web-config)
(require 'lsp-config)
(require 'plantuml-config)
(require 'org-config)
(require 'shell-config)
(require 'go-config)
(require 'tramp-config)
(require 'elfeed-config)
