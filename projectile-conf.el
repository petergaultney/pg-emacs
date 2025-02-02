(use-package projectile
  :ensure t)
(use-package projectile-ripgrep
  :after projectile
  :ensure (projectile-ripgrep
	       :host github
		   :repo "nlamirault/ripgrep.el")
  )
(use-package projectile-codesearch
  :ensure t)
  ;; Recommended keymap prefix on macOS
  ;; :bind (:map projectile-mode-map
  ;;             ((kbd "C-c p") . 'projectile-command-map)))
;; this keymapping is not working, and i can't figure out how to translate
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; into a use-package setup...
