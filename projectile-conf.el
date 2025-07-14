(use-package
  projectile
  :ensure t
  :config
  ;; disable projectile's built-in modeline
  (setq projectile-mode-line-function '(lambda () "")) (projectile-mode +1))

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


(defcustom my-subproject-marker-files
  '("pyproject.toml" "package.json" "Cargo.toml" "go.mod")
  "List of filenames that identify the root of a subproject.
The search for these files will stop at the main project root."
  :type '(repeat string)
  :group 'my)

(defvar-local my-cached-subproject-root nil
  "Buffer-local cache for the calculated subproject root directory.
This is set by `my-set-subproject-root-maybe`.")


(defun my--find-subproject-root ()
  "Find the nearest subproject root directory, if any.
A subproject is a directory *within* the current projectile
project that contains one of the files listed in
`my-subproject-marker-files`. The project root itself cannot
be a subproject.

Returns the directory path as a string, or nil if not found."
  (when-let*
    (
      (proj-root (projectile-project-root))
      ;; Find the nearest directory with a marker file,
      ;; stopping the search when we hit the project root.
      (candidate-dir
        (locate-dominating-file
          default-directory
          (lambda (dir)
            (when (string= (expand-file-name dir) proj-root)
              'stop)
            (cl-some
              (lambda (marker) (file-exists-p (expand-file-name marker dir)))
              my-subproject-marker-files)))))
    ;; This `when` is the critical fix: ensure the candidate we found
    ;; is not the project root itself. This can happen if the search
    ;; starts in the project root.
    (when (and candidate-dir (not (string= (expand-file-name candidate-dir) proj-root)))
      candidate-dir)))

(defun my-set-subproject-root-maybe ()
  "Find and cache the subproject root in a buffer-local variable.
This is intended to be called from `projectile-mode-hook`."
  (setq my-cached-subproject-root (my--find-subproject-root)))

(add-hook 'projectile-mode-hook #'my-set-subproject-root-maybe)
(add-hook 'find-file-hook #'my-set-subproject-root-maybe)
(add-hook 'dired-after-readin-hook #'my-set-subproject-root-maybe)
