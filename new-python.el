;; -*- coding: utf-8; lexical-binding: t -*-

;; Python: pyrefly from the worktree venv via Eglot; Ruff formats on save.
;; Install the monorepo's pinned pyrefly with uv sync --group dev at its root.
;; Venv selection and Eglot-only rooting adapted from Grant Andersen:
;; https://github.com/gandersen101/minimal-emacs.d/tree/ecd268040ed3a00aa637a45ac07af81a6cc0da91/lisp/my-python-env
;; Pyrefly chooses each file's pyproject.toml and interpreter itself.

(defun my/ruff-format-buffer ()
  "Run `ruff format` and `ruff check --fix` (import sorting) on the current buffer."
  (when (eq major-mode 'python-mode)
    (let ((file (buffer-file-name)))
      (when file
        ;; ruff format for code formatting (replaces black)
        (call-process "ruff" nil nil nil "format" "--quiet" file)
        ;; ruff check --fix --select I for import sorting (replaces isort)
        (call-process "ruff" nil nil nil "check" "--fix" "--select" "I" "--quiet" file)
        (revert-buffer t t t)))))

(defun my/python-pyrefly-root (dir)
  "Find the nearest ancestor of DIR with pyrefly in a .venv or venv."
  (locate-dominating-file
   dir (lambda (parent)
         (seq-some
          (lambda (name)
            (let ((venv (expand-file-name name parent)))
              (and (file-exists-p (expand-file-name "pyvenv.cfg" venv))
                   (file-executable-p (expand-file-name "bin/pyrefly" venv)))))
          '(".venv" "venv")))))

(defun my/python-pyrefly-command (dir)
  "Resolve pyrefly from DIR's nearest venv, then exec-path."
  (or (when-let* ((root (my/python-pyrefly-root dir)))
        (seq-some
         (lambda (name)
           (let ((venv (expand-file-name name root)))
             (when (and (file-exists-p (expand-file-name "pyvenv.cfg" venv))
                        (file-executable-p (expand-file-name "bin/pyrefly" venv)))
               (expand-file-name "bin/pyrefly" venv))))
         '(".venv" "venv")))
      (executable-find "pyrefly")))

(defun my/python-eglot-project (dir)
  "Use the pyrefly venv's parent as the project only for Eglot."
  (when (and (bound-and-true-p eglot-lsp-context)
             (derived-mode-p 'python-base-mode)
             buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (when-let* ((root (my/python-pyrefly-root dir)))
      (project-try-vc root))))

(defun my/python-pyrefly-contact (_interactive project)
  "Start pyrefly for PROJECT without indexing unrelated workspace files."
  (list (or (my/python-pyrefly-command (project-root project))
            (user-error "No pyrefly found; run uv sync --group dev at the worktree root"))
        "lsp" "--workspace-indexing-limit" "0"))

(defun my/python-eglot-ensure ()
  "Start Eglot only for a local Python file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (eglot-ensure)))

(use-package
  eglot
  :ensure nil
  :hook
  ((python-mode . my/python-eglot-ensure)
    (python-ts-mode . my/python-eglot-ensure)
    (python-mode . highlight-numbers-mode)
    (eglot-managed-mode
      .
      (lambda ()
        (flymake-mode t)
        (set-window-margins nil 0 nil))) ;; eglot adds a margin in 30.1 for some reason
    (after-save . my/ruff-format-buffer))

  :config

  (remove-hook 'project-find-functions #'my/find-pyproject-root)
  (add-hook 'project-find-functions #'my/python-eglot-project -90)
  (add-to-list 'eglot-server-programs
    '((python-mode python-ts-mode) . my/python-pyrefly-contact))

  ;; Avoid recursively watching monorepo venvs and exhausting macOS kqueue.
  ;; Reconnect Eglot after creating or deleting files in another process.
  (cl-defmethod eglot-client-capabilities :around (_server)
    (let ((caps (cl-call-next-method)))
      (setf (plist-get (plist-get caps :workspace) :didChangeWatchedFiles)
        '(:dynamicRegistration :json-false))
      caps))


  (setq eglot-events-buffer-config '(:size 20000000 :format full)))

;; requires https://github.com/blahgeek/emacs-lsp-booster
;; to be built and on the PATH
(use-package
  eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))
