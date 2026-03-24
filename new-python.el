;; -*- coding: utf-8; lexical-binding: t -*-

;; Python LSP setup: basedpyright (via eglot) + ruff (standalone formatter)
;;
;; External dependencies (install via `uv tool install`):
;;   uv tool install basedpyright    # type checking, completions, go-to-def, hover, references
;;   uv tool install ruff            # linting, formatting, import sorting
;;
;; Also requires https://github.com/blahgeek/emacs-lsp-booster built and on PATH.
;;
;; basedpyright runs as the sole eglot LSP server - it handles type checking,
;; completions, go-to-definition, hover, and references. Reads pyproject.toml.
;;
;; ruff runs outside eglot as a standalone formatter on save (ruff format + ruff
;; check --fix for import sorting). This avoids needing rass (LSP multiplexer),
;; which has compatibility issues with basedpyright 1.38+ as of rass 0.3.3.
;;
;; Previous setup used pylsp + jedi + black + isort + pylsp-mypy. That's all gone now.

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

(defun my/find-pyproject-root (dir)
  "Find a project root by looking for a `pyproject.toml` file."
  (when-let ((root (locate-dominating-file dir "pyproject.toml")))
    (cons 'transient root)))

(use-package
  eglot
  :ensure nil
  :hook
  ((python-mode . eglot-ensure)
    (python-mode . highlight-numbers-mode)
    (eglot-managed-mode
      .
      (lambda ()
        (flymake-mode t)
        (set-window-margins nil 0 nil))) ;; eglot adds a margin in 30.1 for some reason
    (after-save . my/ruff-format-buffer))

  :config

  ;; Find the nearest pyproject.toml as the project root. Must be first
  ;; in project-find-functions so it wins over VC-based detection, which
  ;; would pick the git worktree root instead of the subproject root.
  ;; This matters because eglot starts one basedpyright per project, and
  ;; basedpyright uses the project root to find pyproject.toml and .venv.
  (add-to-list 'project-find-functions #'my/find-pyproject-root)

  (add-to-list 'eglot-server-programs
    '((python-mode) . ("basedpyright-langserver" "--stdio")))

  ;; Disable dynamic file watching. basedpyright sends
  ;; client/registerCapability for workspace/didChangeWatchedFiles on
  ;; init, and eglot 30.1's handler calls (project-files) to enumerate
  ;; the entire project tree, which exhausts macOS kqueue file
  ;; descriptors on the monorepo. The error response kills basedpyright.
  ;; This tells basedpyright we don't support dynamic registration so it
  ;; never sends the request. Tradeoff: basedpyright won't notice when
  ;; other files are created/deleted — use M-x eglot-reconnect if needed.
  ;; Eglot master (unreleased 1.22+) has eglot-max-file-watches which
  ;; handles this more gracefully; switch to that when emacs 30.2 ships.
  ;; See: https://github.com/joaotavora/eglot/issues/1568
  (cl-defmethod eglot-client-capabilities :around (_server)
    (let ((caps (cl-call-next-method)))
      (setf (plist-get (plist-get caps :workspace) :didChangeWatchedFiles)
        '(:dynamicRegistration :json-false))
      caps))


  ;; basedpyright defaults to "recommended" typeCheckingMode which is very
  ;; strict (reportUnknown* everywhere). "standard" is closer to what you'd
  ;; expect coming from mypy defaults. Adjust as needed.
  (setq-default eglot-workspace-configuration
    '(:basedpyright.analysis (:typeCheckingMode "standard")))

  (setq eglot-events-buffer-config '(:size 20000000 :format full)))

;; requires https://github.com/blahgeek/emacs-lsp-booster
;; to be built and on the PATH
(use-package
  eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(provide 'new-python)
