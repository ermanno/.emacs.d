(setq js-indent-level 2)

(use-package web-mode
  :mode (("\\.php$" . web-mode)
         ("\\.html$" . web-mode)))

;; TypeScript (https://github.com/ananthakumaran/tide)
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'web-config)
