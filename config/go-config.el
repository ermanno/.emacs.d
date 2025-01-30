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
