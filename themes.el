(use-package ef-themes :ensure (:host github :repo "protesilaos/ef-themes"))
;; (use-package doom-themes :ensure t) ;; doom themes take over too much stuff and do magic which I do not like.
;; (use-package nord-theme :ensure t)
(use-package spacemacs-theme :ensure t)

(defun fix-markdown-headers ()
  "Fix markdown headers to be readable in dark themes -
   apparently markdown mode customizes its face backgrounds, but i never want that."
  (interactive)
  (dolist (level '(1 2 3 4 5 6))
    (let ((md-face (intern (format "markdown-header-face-%d" level)))
          (outline-face (intern (format "outline-%d" level))))
      (set-face-attribute md-face nil :inherit outline-face))))

(defun remove-all-backgrounds ()
  "Remove background colors from all faces except crucial UI elements."
  (interactive)
  (let ((preserve-bg-faces
          '(
			 ;; Selection and completion UI
			 vertico-current           ; Currently selected item in vertico
			 consult-preview-match     ; Preview matches in consult
			 completions-highlight     ; Standard completion highlighting

			 ;; Region and search
			 region                    ; Selected text
			 isearch                   ; Incremental search matches
			 lazy-highlight            ; Other search matches

			 ;; Diff/magit
			 diff-added
			 diff-removed
			 magit-diff-added-highlight
			 magit-diff-removed-highlight

			 show-paren-match          ; The matching parenthesis
			 show-paren-mismatch       ; Mismatched parenthesis

			 ;; Org mode
			 org-block                 ; Code blocks need backgrounds
			 org-code                  ; Inline code
			 org-table                 ; Tables

			 ;; Markdown
			 markdown-header-face
			 markdown-header-face-1
			 markdown-header-face-2
			 markdown-header-face-3
			 markdown-header-face-4
			 markdown-header-face-5
			 markdown-header-face-6

			 ;; UI elements
			 header-line

			 ;; Others
			 hl-line                   ; Line highlighting
			 highlight                 ; Generic highlighting
			 highlight-numbers-number  ;Number highlighting
			 secondary-selection       ; Secondary selection
			 which-key-highlighted-command-face ; Which-key highlighting

			 ;; avy
			 avy-lead-face            ; Avy lead face
			 avy-lead-face-0          ; Avy lead face for first candidate
			 avy-lead-face-1          ; Avy lead face for second candidate
			 avy-lead-face-2          ; Avy lead face for third candidate
			 )))

    (mapc (lambda (face)
            (when (and (face-background face)
                    (not (memq face preserve-bg-faces)))
              (set-face-background face "unspecified-bg")))
      (face-list))))


(defun invert-modeline ()
  "Invert modeline colors using foreground as background and vice versa."
  (interactive)
  ;; Get foreground and set as modeline background
  (let ((fg (face-foreground 'default))
         (bg (face-background 'default nil t))) ;; fallback to terminal bg if needed

    ;; Set mode-line and mode-line-inactive with inverted colors
    (set-face-attribute 'mode-line nil
      :background fg
      :foreground bg
      :box nil)

    ;; Slightly dimmer for inactive modelines
    (set-face-attribute 'mode-line-inactive nil
      :background fg
      :foreground bg
      :box nil)))


(defun load-theme-tweaks (theme &optional no-bg invert-mode)
  "Load THEME with tweaks.
When NO-BG is non-nil (default t), remove all background colors.
When INVERT-MODE is non-nil (default t), invert modeline colors."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                   (mapcar 'symbol-name (custom-available-themes))))))
  (load-theme theme t)
  ;; Default both to t when not specified
  (when (or (eq no-bg t) (null no-bg))
    (remove-all-backgrounds))
  (when (or (eq invert-mode t) (null invert-mode))
    (invert-modeline))
  (fix-markdown-headers) ;; always do this last
  )



(defun print-names-of-visible-faces ()
  "Print all unique faces currently visible on screen to a buffer."
  (interactive)
  (let ((visible-faces '())
        (line-count (count-lines (window-start) (window-end))))

    ;; Collect faces from visible text
    (save-excursion
      (goto-char (window-start))
      (dotimes (_ line-count)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (while (< line-start line-end)
            (let* ((face-prop (get-text-property line-start 'face))
                   (faces (if (listp face-prop) face-prop (list face-prop))))
              (dolist (face faces)
                (when (and face (facep face))
                  (cl-pushnew face visible-faces :test #'eq))))
            (setq line-start (next-property-change line-start nil line-end))))
        (forward-line 1)))

    ;; Add mode-line and header-line faces
    (when (get-buffer-window)
      (cl-pushnew 'mode-line visible-faces :test #'eq)
      (cl-pushnew 'mode-line-inactive visible-faces :test #'eq)
      (cl-pushnew 'header-line visible-faces :test #'eq))

    ;; Collect faces from other visible windows
    (dolist (window (window-list))
      (when (not (eq window (selected-window)))
        (with-current-buffer (window-buffer window)
          (let ((line-count (count-lines (window-start window) (window-end window))))
            (save-excursion
              (goto-char (window-start window))
              (dotimes (_ line-count)
                (let ((line-start (line-beginning-position))
                      (line-end (line-end-position)))
                  (while (< line-start line-end)
                    (let* ((face-prop (get-text-property line-start 'face))
                           (faces (if (listp face-prop) face-prop (list face-prop))))
                      (dolist (face faces)
                        (when (and face (facep face))
                          (cl-pushnew face visible-faces :test #'eq))))
                    (setq line-start (next-property-change line-start nil line-end))))
                (forward-line 1)))))))

    ;; Display results
    (with-current-buffer (get-buffer-create "*visible-faces*")
      (erase-buffer)
      (dolist (face (sort (delq nil visible-faces) #'string-lessp))
        (insert (format "%s\n" face)))
      (goto-char (point-min))
      (switch-to-buffer (current-buffer))
      (message "Found %d visible faces" (length visible-faces)))))


(add-hook 'elpaca-after-init-hook
  (lambda ()

	(defvar my-preferred-dark-theme
	  'ef-autumn
	  ;; 'ef-dark
	  ;;'ef-night
	  ;; 'spacemacs-dark
	  "The dark theme to use when toggling themes.")

	(defvar my-preferred-light-theme
	  'ef-light
	  "The light theme to use when toggling themes.")

	(load-theme-tweaks my-preferred-dark-theme)))

(defun toggle-light-dark-theme ()
  "Toggle between preferred light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) my-preferred-dark-theme)
    (load-theme-tweaks my-preferred-light-theme)
    (load-theme-tweaks my-preferred-dark-theme)))

(defun theme-light ()
  "Load the preferred light theme."
  (interactive)
  (load-theme-tweaks my-preferred-light-theme))

(defun theme-dark ()
  "Load the preferred dark theme."
  (interactive)
  ;; log the name of the theme
  (message "Theme: %s" my-preferred-dark-theme)
  (load-theme-tweaks my-preferred-dark-theme))

;; Load the default theme (dark)
;;(load-theme-tweaks my-preferred-dark-theme)


(defun switch-theme-interactive (&optional use-defaults)
  "Switch themes interactively with customized tweaks.
When USE-DEFAULTS is non-nil, apply default tweaks without prompting."
  (interactive "P")
  (let* ((themes (custom-available-themes))
         (theme-name (completing-read "Choose theme: "
                                     (mapcar #'symbol-name themes)
                                     nil t))
         (theme-symbol (intern theme-name)))

    ;; Disable currently enabled themes
    (mapc #'disable-theme custom-enabled-themes)

    ;; Use your custom load-theme-tweaks function
    (load-theme-tweaks theme-symbol t t)

    ;; Provide feedback
    (message "Loaded theme %s" theme-name)))
