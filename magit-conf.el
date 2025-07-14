(use-package transient :ensure (:host github :repo "magit/transient"))

(use-package
  magit
  :after transient
  :ensure (:host github :repo "magit/magit")
  :bind
  (("C-c g" . magit-status) ;; (global-set-key (kbd "C-c g") 'magit-status)
    :map magit-file-section-map ("RET" . magit-diff-visit-file-other-window)
    :map magit-hunk-section-map ("RET" . magit-diff-visit-file-other-window))

  :config (global-unset-key (kbd "C-x g"))
  ;; i do not like magit activating
  ;; itself when i'm just trying to
  ;; Ctrl-g cancel something.
  )

(use-package forge :ensure t :after magit)
