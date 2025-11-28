;; rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'rust-config)
