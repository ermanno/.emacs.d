(defun ermann/attached-to-external-monitor-p ()
  "Check whether the laptop is attached to an external monitor"
  (string-match-p
   "[Kk]eyboard"
   (shell-command-to-string "ioreg -p IOUSB -w0")))

(when (and
       (eq system-type 'darwin)
       (not (ermann/attached-to-external-monitor-p)))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

(provide 'mac-config)
