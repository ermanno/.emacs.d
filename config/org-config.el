;; Org mode
(require 'org-mouse)

(defcustom ermann/org-files-directory (expand-file-name "~/Documents/org")
  "Directory for storing org files."
  :type 'directory
  :group 'org)

;; Ensure org directory exists
(unless (file-exists-p ermann/org-files-directory)
  (make-directory ermann/org-files-directory t))

;; Configure org-mode settings
(with-eval-after-load 'org
  (setq org-use-speed-commands t
        org-src-fontify-natively t
        org-adapt-indentation nil
        org-hide-emphasis-markers t
        org-edit-src-content-indentation 0
        org-preview-latex-default-process 'dvisvgm
        org-agenda-files (list ermann/org-files-directory)))

;; https://github.com/eyeinsky/org-anki
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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (org . t)
   (plantuml . t)))

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

(provide 'org-config)
