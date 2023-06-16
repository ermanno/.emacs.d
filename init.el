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
(when (and
       (eq system-type 'darwin)
       (not (string-match-p
             "[Kk]eyboard"
             (shell-command-to-string "ioreg -p IOUSB -w0"))))
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
(defun shell-indentation-settings ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook #'shell-indentation-settings)

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

;; Perspectives -- https://github.com/nex3/perspective-el
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c s"))
  :init
  (persp-mode))

;; Show parens, not Lisp specific, but most useful for lisps
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; Smartparens
(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

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

;; PlantUML mode
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :mode "\\.plu\\'"
  :custom
  (plantuml-jar-path "~/bin/plantuml-1.2022.0.jar")
  (plantuml-default-exec-mode 'jar)
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml-1.2022.0.jar"))
  (org-startup-with-inline-images t)
  )

(use-package flycheck-plantuml
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

;; JavaScript
(setq js-indent-level 2)

;;;;; Web mode
(use-package web-mode
  :mode (("\\.php$" .  web-mode)
         ("\\.html$" .  web-mode)))

;; TODO company and flycheck are generic packages whose configuration
;; should live somewhere else than tide's

(use-package tide)
(use-package company)
(use-package flycheck)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;;;; Make Tide aware of tsx files
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; Org mode
(require 'org-mouse)

(setq org-use-speed-commands t
      org-src-fontify-natively t
      org-adapt-indentation nil
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-agenda-files '("~/Documents/org"))

(defun my-org-mode-hook ()
  "My hook for org mode, setting up spell checking, word wrapping and other niceties."
  (ispell-change-dictionary "english")
  (flyspell-mode)
  (visual-line-mode))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(use-package org
  :preface
  (defun ermann/org-link-copy (&optional arg)
    "Extract URL from org-mode link and add it to kill ring."
    (interactive "P")
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (url (org-element-property :path link))
           (url (concat type ":" url)))
      (kill-new url)
      (message (concat "Copied URL: " url))))
  :config (global-set-key (kbd "C-c c") 'org-capture)
  :bind (:map org-mode-map
              ("C-c b" . org-insert-structure-template)
              ("C-c l" . ermann/org-link-copy)))

;; https://github.com/abo-abo/org-download
(use-package org-download
  :defer t
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable)))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (org . t)
   (plantuml . t)
   ))

;; Redisplay images after evaluating with C-c C-c
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

;; Don't ask for confirmation when evaluating PlantUML code
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
