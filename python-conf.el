(defvar myPackages
  '(;; elpy
    material-theme
    better-defaults
	color-identifiers-mode
	highlight-numbers
;;	jedi  ;; i can't remember why this is not enabled?
    ))

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(add-hook 'python-mode-hook 'yas-minor-mode)

;; (add-hook 'python-mode-hook #'tree-sitter-mode)
;; (tree-sitter-require 'python)

(add-hook 'python-mode-hook 'highlight-numbers-mode)

;; (with-eval-after-load 'lsp-mode  ; try this or similar
;;   (lsp-register-custom-settings
;;    '(("pylsp.plugins.pylsp_mypy.enabled" t t)
;;      ("pylsp.plugins.pylsp_mypy.live_mode" t t)
;;      ("pylsp.plugins.black.enabled" t t)
;;      ("pylsp.plugins.pylsp_isort.enabled" t t))))
