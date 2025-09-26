;; From: https://www.reddit.com/r/emacs/comments/1jatdse/im_trying_to_troubleshoot_extremely_slow_tramp/
(use-package tramp
  :config
  ;; To speed up connections
  (setq tramp-verbose 0
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil
        tramp-default-method "ssh"
        tramp-verbose 1
        tramp-default-remote-shell "/bin/sh"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch . "-c")))
  ;; For ~/.local/bin etc. to work when SSHing
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'tramp-config)
