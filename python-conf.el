(defvar myPackages
  '(elpy
    flycheck
    flycheck-flow
    material-theme
    better-defaults
	color-identifiers-mode
	blacken
;;	jedi  ;; i can't remember why this is not enabled?
    ))

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

(use-package elpy)
(elpy-enable)
;; auto-format hook for when black is standard
;; (add-hook 'elpy-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook
;;                                       'elpy-black-fix-code nil t)))
