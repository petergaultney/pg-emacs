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
      ;; (which-key-mode "") ;; use (setq which-key-lighter nil) instead ??
      (whitespace-mode " _" whitespace)
      (paredit-mode " ()" paredit))))


(defun my/force-mode-line-update ()
  "Force an update of the mode line."
  (interactive)
  (force-mode-line-update))


(defun my--get-modeline-place-info ()
  "Return the project or directory info as a cons cell (NAME . DIR).
Returns nil if no suitable name can be found."
  (let*
    (
      (project-name
        (and (fboundp 'projectile-project-p)
          (projectile-project-p)
          (projectile-project-name)))
      (is-real-project (and project-name (not (string= project-name "-")))))
    (cond
      (is-real-project
        (cons project-name (projectile-project-root)))
      ((buffer-file-name)
        (cons
          (file-name-nondirectory (directory-file-name default-directory))
          default-directory))
      (t
        nil))))

(defun my--propertize-for-dired (text dir)
  "Propertize TEXT to open DIR in dired on click."
  (let ((map (make-sparse-keymap)))
    (define-key
      map [mode-line mouse-1]
      `
      (lambda ()
        (interactive)
        (dired ,dir)))
    (propertize text 'keymap map 'help-echo (format "mouse-1: Dired %s" dir))))


(defun my-modeline-place ()
  "Display a clickable, colored project or directory name."
  (when-let*
    (
      (place-info (my--get-modeline-place-info))
      (name (car place-info))
      (dir (cdr place-info))
      (colors (my-pick-fg-bg-color-from-hsl name 0.4))
      (bg-color (nth 0 colors))
      (text-color (nth 1 colors)))
    (let*
      (
        (text (format "%s " name))
        (colored-text
          (propertize text 'face `(:background ,bg-color :foreground ,text-color))))
      (my--propertize-for-dired colored-text dir))))


(defun my-modeline-subproject ()
  "Display a clickable, colored subproject name, if one is found.
Relies on `my-cached-subproject-root` being set."
  (when-let* ((dir my-cached-subproject-root))
    (let*
      (
        (name (file-name-nondirectory (directory-file-name dir)))
        (colors (my-pick-fg-bg-color-from-hsl name 0.4))
        (bg-color (nth 0 colors))
        (text-color (nth 1 colors))
        (text (format "/%s " name))
        (colored-text
          (propertize text 'face `(:background ,bg-color :foreground ,text-color))))
      (my--propertize-for-dired colored-text dir))))


;; warning! this next section is manually overriding the mode-line-format.

(setq old-mode-line-format mode-line-format)
;; (setq-default mode-line-format old-mode-line-format)

(setq-default mode-line-format
  '
  ("%e"
    mode-line-front-space
    (:propertize
      (""
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-window-dedicated)
      display (min-width (6.0)))
    (:eval (my-modeline-place))
    (:eval (my-modeline-subproject))
    " "
    mode-line-buffer-identification
    "   "
    mode-line-position
    (project-mode-line project-mode-line-format)
    (vc-mode vc-mode)
    "  "
    mode-line-modes
    mode-line-misc-info
    mode-line-end-spaces))
