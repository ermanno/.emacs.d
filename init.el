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

;; UI
(setq inhibit-startup-screen t
      frame-title-format "%b"
      visible-bell 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default cursor-type 'bar)
(column-number-mode)
(show-paren-mode)

(use-package vs-light-theme
  :config (load-theme 'vs-light t))

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
  :config
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

;; Org mode
(require 'org-mouse)

(setq org-use-speed-commands t
      org-src-fontify-natively t
      org-adapt-indentation nil
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0)

(defun my-org-mode-hook ()
  "My hook for org mode, setting up spell checking, word wrapping and other niceties."
  (ispell-change-dictionary "english")
  (flyspell-mode)
  (visual-line-mode))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(use-package org
  :config (global-set-key (kbd "C-c c") 'org-capture)
  :bind (:map org-mode-map
              ("C-c b" . org-insert-structure-template)))

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

;; From https://yiufung.net/post/anki-org/
;; For the moment I am leaving out the cloze stuff, as I don't use it.
(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f9>" . anki-editor-push-tree))
  :config
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4)))
  )

;; Org-capture templates
(setq org-my-anki-file "~/Documents/anki/learning.org")

(setq org-capture-templates
      '(
        ("t" "Anki TCP/IP stack"
         entry
         (file+headline org-my-anki-file "TCP/IP")
         "* %<%H:%M>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: TCP/IP\n:END:\n** Front\n%?\n** Back\n")
	("s" "Anki Security stack"
         entry
         (file+headline org-my-anki-file "Security")
         "* %<%H:%M>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Security\n:END:\n** Front\n%?\n** Back\n")))

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
