;; PlantUML mode
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :mode "\\.plu\\'"
  :custom
  (plantuml-jar-path "~/bin/plantuml-1.2022.0.jar")
  (plantuml-default-exec-mode 'jar)
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml-1.2025.10.jar"))
  (org-startup-with-inline-images t))

(use-package flycheck-plantuml
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

(provide 'plantuml-config)
