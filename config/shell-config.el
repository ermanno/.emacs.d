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

(use-package vterm
  :if (and (executable-find "cmake")
           (version<= "3.11" (string-trim (shell-command-to-string "cmake --version | head -n1 | awk '{print $3}'")))))

(provide 'shell-config)
