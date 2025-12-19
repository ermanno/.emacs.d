;; UX
(global-auto-revert-mode)
(winner-mode)
(windmove-default-keybindings)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 0)

;; Use only spaces
(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; Don't give warning with native compilation
(setq native-comp-async-report-warnings-errors 'silent)

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1))

(use-package transpose-frame
  :bind ("C-c t" . transpose-frame))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c C-l" . mc/mark-all-like-this)))

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

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

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

;; Smartparens
(use-package smartparens
  :hook (prog-mode)
  :config
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

(provide 'ux-config)
