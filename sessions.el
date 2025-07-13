(use-package
  perspective
  :ensure t
  :custom (persp-mode-prefix-key (kbd "C-q"))
  :config
  (setq persp-modestring-short t)
  (setq persp-show-modestring 'header)
  :init (persp-mode 1))

(use-package consult
  :after perspective

  :config
  (message "Configuring Consult-Perspective integration...")
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))
