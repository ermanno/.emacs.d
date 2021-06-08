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
(blink-cursor-mode -1)
(load-theme 'dichromacy t)
(column-number-mode)
(show-paren-mode)

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'dark)
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

;; Unfill
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

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

;; Org mode Hugo support
(use-package ox-hugo
  :ensure t
  :after ox)

;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "~/blog/blog.org" "Posts")
                 (function org-hugo-new-subtree-post-capture-template))))


(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
