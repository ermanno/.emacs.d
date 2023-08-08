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

;; Neotree
(use-package neotree
  :bind ("<f5>" . neotree-toggle))

;; Expand region
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; Unfill
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package flycheck)

(provide 'ux-config)
