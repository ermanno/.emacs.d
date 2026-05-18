;; Magit - pinned to v3.3.0 (last version supporting Emacs 27)
;; All dependencies pinned to avoid MELPA pulling newer Emacs 28+ versions
(use-package transient
  :ensure nil
  :quelpa (transient :fetcher github :repo "magit/transient"
                     :commit "1fae04a6346ea09a15ab5c842ea5aae718a2d18a"
                     :files ("lisp/transient.el")))

(use-package with-editor
  :ensure nil
  :quelpa (with-editor :fetcher github :repo "magit/with-editor"
                       :commit "5d39abc5c28299c02b75c687bce5d7d1d7786b49"
                       :files ("with-editor.el")))

(use-package magit-section
  :ensure nil
  :quelpa (magit-section :fetcher github :repo "magit/magit"
                         :commit "7fcd837f38c5ee11e5c95b2e64e8612cd6947d96"
                         :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el")))

(use-package git-commit
  :ensure nil
  :quelpa (git-commit :fetcher github :repo "magit/magit"
                      :commit "7fcd837f38c5ee11e5c95b2e64e8612cd6947d96"
                      :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el")))

(use-package magit
  :ensure nil
  :quelpa (magit :fetcher github :repo "magit/magit"
                 :commit "7fcd837f38c5ee11e5c95b2e64e8612cd6947d96"
                 :files ("lisp/magit.el" "lisp/magit-*.el" "lisp/git-rebase.el"
                         (:exclude "lisp/magit-section.el" "lisp/magit-section-pkg.el"
                                   "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el")))
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

(provide 'git-config)
