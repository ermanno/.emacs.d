;; Tree-sitter configuration for Go
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

;; Install the grammar if needed
(dolist (lang '(go gomod))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

;; Use tree-sitter modes for Go
(add-to-list 'major-mode-remap-alist
             '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(go-mod-mode . go-mod-ts-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'go-config)
