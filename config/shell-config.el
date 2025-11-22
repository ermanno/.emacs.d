(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(defun shell-indentation-settings ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook #'shell-indentation-settings)

(require 'ansi-color)
(defun ermann/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'ermann/colorize-compilation-buffer)

(use-package vterm)

(provide 'shell-config)
