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

;; Mac settings

(defun ermann/attached-to-external-monitor-p ()
  "Check whether the laptop is attached to an external monitor"
  (string-match-p
   "[Kk]eyboard"
   (shell-command-to-string "ioreg -p IOUSB -w0")))

(when (and
       (eq system-type 'darwin)
       (not (ermann/attached-to-external-monitor-p)))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

;; UI
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

;; UX
(global-auto-revert-mode)
(winner-mode)
(windmove-default-keybindings)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

;; Use only spaces
(setq-default indent-tabs-mode nil)

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Undo tree
(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1))

;; Transpose frame
(use-package transpose-frame
  :bind ("C-c t" . transpose-frame))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c C-l" . mc/mark-all-like-this)))

;; Ivy/Swiper/Counsel/Smex
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)))

(use-package swiper
  :bind ("C-c o" . swiper))

(use-package smex)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c a" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-c k" . counsel-compile)
         ("C-c r" . counsel-recentf)
         ("C-c h" . counsel-imenu))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (defun ermann/magit-log-edit-mode-hook ()
      (setq fill-column 72)
      (flyspell-mode t)
      (turn-on-auto-fill))
    (add-hook 'magit-log-edit-mode-hook 'ermann/magit-log-edit-mode-hook)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))))

;; Git gutter (or fringe if we are in graphics mode)
(let ((package-name (if (display-graphic-p)
                        "git-gutter-fringe"
                      "git-gutter")))
  (eval `(use-package ,package-name
           :init (global-git-gutter-mode))))

;; Neotree
(use-package neotree
  :bind ("<f5>" . neotree-toggle))

;; Expand region
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; Bash
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(defun shell-indentation-settings ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook #'shell-indentation-settings)

(require 'ansi-color)
(defun ermann/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'ermann/colorize-compilation-buffer)

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

;; Unfill
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

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

(use-package flycheck)

;; TypeScript (https://github.com/ananthakumaran/tide)
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(require 'plantuml-config)
(require 'org-config)
