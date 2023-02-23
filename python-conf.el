(defvar myPackages
  '(;; elpy
    flycheck
    flycheck-flow
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

(defun my-python-mode-before-save-hook ()
  (when (eq major-mode 'python-mode)
    (blacken-buffer)))

;; (add-hook 'python-mode-hook 'anaconda-mode)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

;; (use-package elpy)
;; (elpy-enable)
;; auto-format hook for when black is standard
;; (add-hook 'elpy-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook
;;                                       'elpy-black-fix-code nil t)))
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook #'tree-sitter-mode)
(tree-sitter-require 'python)
(add-hook 'python-mode-hook 'highlight-numbers-mode)
;; (add-hook 'before-save-hook 'my-python-mode-before-save-hook)
;; (setq read-process-output-max (* 1024 1024)) ;; 1m - for lsp mode

;; (with-eval-after-load 'lsp-mode  ; try this or similar
;;   (lsp-register-custom-settings
;;    '(("pylsp.plugins.pylsp_mypy.enabled" t t)
;;      ("pylsp.plugins.pylsp_mypy.live_mode" t t)
;;      ("pylsp.plugins.black.enabled" t t)
;;      ("pylsp.plugins.pylsp_isort.enabled" t t))))

;; (with-eval-after-load 'lsp-mode
;;   ;; :global/:workspace/:file
;;   (setq lsp-modeline-diagnostics-scope :workspace))
