;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(package-initialize)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; GNU ELPA is already part of package-archives, add melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it is not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
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
  :ensure t
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
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1))

;; Transpose frame
(use-package transpose-frame
  :ensure t
  :bind ("C-c t" . transpose-frame))

;; Multiple cursors
(use-package multiple-cursors
  :init
  (progn
    ;; these need to be defined here - if they're lazily loaded with
    ;; :bind they don't work.
    (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-l") 'c/mark-all-like-this)))

;; Ivy/Swiper/Counsel/Smex
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'swiper))

(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h l") 'counsel-find-library)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c k") 'counsel-compile)
  (global-set-key (kbd "C-c r") 'counsel-recentf)
  (global-set-key (kbd "C-c h") 'counsel-imenu)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
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
           :ensure t
           :init (global-git-gutter-mode))))

;; Neotree
(use-package neotree
  :ensure t
  :bind ("<f5>" . neotree-toggle))

;; Expand region
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; YAML support
(use-package yaml-mode
  :mode "\\.yaml\\'")

;; Markdown support
(use-package markdown-mode
  :ensure t
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
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

;; Scheme
(use-package geiser-mit
  :ensure t
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
  :ensure t
  :config
  (progn (setq yas-verbosity 1
               yas-wrap-around-region t)
         (yas-reload-all)
         (yas-global-mode)))

(use-package yasnippet-snippets
  :ensure t)

;; PlantUML mode
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :ensure t
  :mode "\\.plu\\'"
  :custom
  (plantuml-jar-path "~/bin/plantuml-1.2022.0.jar")
  (plantuml-default-exec-mode 'jar)
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml-1.2022.0.jar"))
  (org-startup-with-inline-images t)
  )

(use-package flycheck-plantuml
  :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

;; JavaScript
(setq js-indent-level 2)

;;;;; Web mode
(use-package web-mode
  :ensure t
  :mode (("\\.php$" .  web-mode)
         ("\\.html$" .  web-mode)))

;; Typescript
;; Source: https://willschenk.com/articles/2021/setting_up_emacs_for_typescript_development/

;;;;; Install Tide
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
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
  :ensure t
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

;; bash
(defun shell-indentation-settings ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook #'shell-indentation-settings)

;; rust
;; https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun ermann/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
