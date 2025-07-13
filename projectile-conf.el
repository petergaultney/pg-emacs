(defun my-projectile-modeline ()
  "Customize the Projectile modeline."
  (when (and (projectile-project-p)
             (projectile-project-name)
             (not (string= (projectile-project-name) "-")))
    (let* ((project-name (projectile-project-name))
           (colors (my-pick-fg-bg-color-from-hsl project-name 0.4))
           (bg-color (nth 0 colors))
           (text-color (nth 1 colors)))
      (propertize (format " %s " project-name)
                  'face `(:background ,bg-color :foreground ,text-color)))))


;; warning! this next section is manually overriding the mode-line-format.

(setq old-mode-line-format mode-line-format)
;; (setq mode-line-format old-mode-line-format)

(defun insert-after-mode-line-modes (new-element)
  "Insert NEW-ELEMENT after mode-line-modes in mode-line-format."
  (let ((new-format '()))
    (dolist (element mode-line-format)
      (push element new-format)
      (when (equal element 'mode-line-modes)
        (push new-element new-format)))
    (reverse new-format)))

(setq-default mode-line-format
  (insert-after-mode-line-modes '(:eval (my-projectile-modeline))))

;; (force-mode-line-update t)

;; And disable projectile's built-in modeline
(setq projectile-mode-line-function '(lambda () ""))

(defun my/force-mode-line-update ()
  "Force an update of the mode line."
  (interactive)
  (force-mode-line-update))

(use-package
  projectile
  :ensure t
  :config (setq projectile-mode-line-function 'my-projectile-modeline))

(use-package
  projectile-ripgrep
  :after projectile
  :ensure (projectile-ripgrep :host github :repo "nlamirault/ripgrep.el"))

(use-package projectile-codesearch :ensure t)

;; Recommended keymap prefix on macOS
;; :bind (:map projectile-mode-map
;;             ((kbd "C-c p") . 'projectile-command-map)))
;; this keymapping is not working, and i can't figure out how to translate
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; into a use-package setup...
