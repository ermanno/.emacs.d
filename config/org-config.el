;; Org mode
(require 'org-mouse)

;; setup org and org roam directories
(defcustom ermann/org-files-directory
  (expand-file-name (if (file-exists-p "~/Dropbox") "~/Dropbox/org" "~/Documents/org"))
  "Directory for storing org files."
  :type 'directory
  :group 'org)

(defcustom ermann/org-roam-files-directory (file-name-concat ermann/org-files-directory "roam")
  "Directory for storing org roam files."
  :type 'directory
  :group 'org)

(dolist (dir (list ermann/org-files-directory ermann/org-roam-files-directory))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; needed to export Anki
(use-package htmlize)

;; Configure org-mode settings
(with-eval-after-load 'org
  (setq org-use-speed-commands t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-adapt-indentation nil
        org-hide-emphasis-markers t
        org-edit-src-content-indentation 0
        org-preview-latex-default-process 'dvisvgm
        org-agenda-files (list ermann/org-files-directory)
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t))

;; https://github.com/eyeinsky/org-anki
;; install anki connect plug in (id: 2055492159)
(use-package org-anki
  :after org
  :config (setq org-anki-default-deck "Default"))

(defun ermann/org-mode-hook ()
  "My hook for org mode, setting up spell checking, word wrapping and other niceties."
  (ispell-change-dictionary "english")
  (flyspell-mode)
  (visual-line-mode))
(add-hook 'org-mode-hook 'ermann/org-mode-hook)

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
(use-package ob-rust)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (python . t)
   (org . t)
   (plantuml . t)
   (rust . t)
   (java . t)))

;; Redisplay images after evaluating with C-c C-c
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

;; Don't ask for confirmation when evaluating PlantUML code
(defun ermann/org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))
(setq org-confirm-babel-evaluate #'ermann/org-confirm-babel-evaluate)

(defun ermann/org-count-chars-in-subtree ()
  "Counts the number of characters in an Org mode subtree"
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (forward-line 1)
    (exchange-point-and-mark)
    (forward-line -1)
    (let ((nchars (- (point) (mark))))
      (deactivate-mark)
      (message "%d" nchars))))


(use-package org-pomodoro
  :after (org alert)
  :config
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15
        org-pomodoro-format "⏱ %s"
        org-pomodoro-short-break-format "☕ %s"
        org-pomodoro-long-break-format "🌴 %s"
        org-pomodoro-time-format "%m:%s"
        )
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'org-config)
