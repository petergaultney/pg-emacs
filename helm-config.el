(use-package helm)
(use-package helm-rg)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-p") 'helm-projectile-find-file-dwim)
