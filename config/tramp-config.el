;; From: https://www.reddit.com/r/emacs/comments/1jatdse/im_trying_to_troubleshoot_extremely_slow_tramp/
(use-package tramp
  :config
  ;; To speed up connections
  (setq tramp-verbose 10 ; https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil
        tramp-default-method "ssh"
        tramp-verbose 1
        tramp-default-remote-shell "/bin/sh"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch . "-c"))
        tramp-auto-save-directory temporary-file-directory)
  ;; Backup (file~) disabled and auto-save (#file#) locally to prevent delays in editing remote files
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; For ~/.local/bin etc. to work when SSHing
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'tramp-config)
