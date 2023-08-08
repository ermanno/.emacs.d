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

;; YAML support
(use-package yaml-mode
  :mode "\\.yaml\\'")

;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Perspective -- https://systemcrafters.net/effective-emacs-workflow/declutter-your-buffers-perspective-el/
(use-package perspective
  :custom (persp-mode-prefix-key (kbd "C-c s"))
  :bind ("C-x k" . persp-kill-buffer*)
  :init (persp-mode))

;; Show parens, not Lisp specific, but most useful for lisps
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; Smartparens
(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config))

;; Scheme
(use-package geiser-mit
  :init (progn
          (setq geiser-mit-binary "/usr/bin/scheme"
                geiser-active-implementations '(mit))
          (add-hook 'scheme-mode-hook 'geiser-mode))
  :commands geiser-mode)

;; yasnippet
(use-package yasnippet
  :config
  (progn (setq yas-verbosity 1
               yas-wrap-around-region t)
         (yas-reload-all)
         (yas-global-mode)))

(use-package yasnippet-snippets)

;; JavaScript
(setq js-indent-level 2)

;;;;; Web mode
(use-package web-mode
  :mode (("\\.php$" . web-mode)
         ("\\.html$" . web-mode)))

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t)
  :diminish company-mode)

;; TypeScript (https://github.com/ananthakumaran/tide)
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(require 'mac-config)
(require 'git-config)
(require 'plantuml-config)
(require 'org-config)
(require 'shell-config)
