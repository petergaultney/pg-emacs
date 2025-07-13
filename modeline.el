(use-package
  dim
  :ensure t
  :config
  (dim-minor-names
    '
    ((visual-line-mode " ↩")
      (abbrev-mode " ⌨")
      (auto-fill-function " ↵")
      (eldoc-mode "" eldoc)
      (color-identifiers-mode " 🍭")
	  (which-key-mode "")
      (whitespace-mode " _" whitespace)
      (paredit-mode " ()" paredit))))


(defun my/force-mode-line-update ()
  "Force an update of the mode line."
  (interactive)
  (force-mode-line-update))


(defun my-modeline ()
  "Display colored project or directory name in the modeline."
  (let ((place-name (if (and (projectile-project-p)
                       (projectile-project-name)
                       (not (string= (projectile-project-name) "-")))
						(projectile-project-name)
					  (file-name-nondirectory (directory-file-name default-directory)))))
    (when (and place-name (not (string-empty-p place-name)))
      (let* ((colors (my-pick-fg-bg-color-from-hsl place-name 0.4))
             (bg-color (nth 0 colors))
             (text-color (nth 1 colors)))
        (propertize (format " %s " place-name)
                    'face `(:background ,bg-color :foreground ,text-color))))))

;; warning! this next section is manually overriding the mode-line-format.

(setq old-mode-line-format mode-line-format)
;; (setq mode-line-format old-mode-line-format) ;; fix the modeline

(defun insert-after-mode-line-modes (new-element)
  "Insert NEW-ELEMENT after mode-line-modes in mode-line-format."
  (let ((new-format '()))
    (dolist (element mode-line-format)
      (push element new-format)
      (when (equal element 'mode-line-modes)
        (push new-element new-format)))
    (reverse new-format)))

(setq-default mode-line-format
  (insert-after-mode-line-modes '(:eval (my-modeline))))
