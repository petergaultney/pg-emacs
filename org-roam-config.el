(require 'org)
(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (expand-file-name "~/org/roam"))
	  (org-roam-buffer-width 0.2)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
(provide 'org-roam-config)
