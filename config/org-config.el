;; Org mode
(require 'org-mouse)

(defcustom ermann/org-files-directory
  (expand-file-name (if (file-exists-p "~/Dropbox") "~/Dropbox/org" "~/Documents/org"))
  "Directory for storing org files."
  :type 'directory
  :group 'org)

;; needed to export Anki
(use-package htmlize)

(use-package org
  :preface
  (defun ermann/org-link-copy (&optional arg)
    "Extract URL from org-mode link and add it to kill ring."
    (interactive "P")
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (url  (concat type ":" (org-element-property :path link))))
      (kill-new url)
      (message "Copied URL: %s" url)))

  (defun ermann/org-count-chars-in-subtree ()
    "Count the number of characters in an Org mode subtree."
    (interactive)
    (save-excursion
      (org-mark-subtree)
      (message "%d" (- (region-end) (region-beginning)))))

  :hook (org-mode . (lambda ()
                      (ispell-change-dictionary "english")
                      (flyspell-mode t)
                      (visual-line-mode)))

  :config
  (setq org-use-speed-commands               t
        org-src-fontify-natively             t
        org-src-preserve-indentation         t
        org-adapt-indentation                nil
        org-hide-emphasis-markers            t
        org-edit-src-content-indentation     0
        org-preview-latex-default-process    'dvisvgm
        org-agenda-files                     (list ermann/org-files-directory)
        org-startup-with-inline-images       t
        org-confirm-babel-evaluate           (lambda (lang _body)
                                               (not (string= lang "plantuml"))))
  (global-set-key (kbd "C-c c") 'org-capture)

  :bind (:map org-mode-map
              ("C-c b" . org-insert-structure-template)
              ("C-c l" . ermann/org-link-copy)))

;; https://github.com/eyeinsky/org-anki — requires AnkiConnect plugin (id: 2055492159)
(use-package org-anki
  :after org
  :config (setq org-anki-default-deck "Default"))

;; https://github.com/abo-abo/org-download
(use-package org-download
  :after org
  :config (org-download-enable))

;; org-babel languages
(use-package ob-rust)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell      . t)
   (emacs-lisp . t)
   (python     . t)
   (org        . t)
   (plantuml   . t)
   (rust       . t)
   (java       . t)))

;; Redisplay inline images after babel evaluation
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

(provide 'org-config)
