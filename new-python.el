;; -*- coding: utf-8; lexical-binding: t -*-

;; Configuration

;; how to set up pylsp server to work with this:
;; 1. figure out which python eglot is looking for. it's usually just the 'default python'.
;; 2. in that python, `pip install "python-language-server[flake8]"` or whatever extras you want
;; 3. then `pip install "pylsp-mypy @ git+https://github.com/petergaultney/pylsp-mypy.git@master"
;; 4. then `pip install python-lsp-black pyls-isort`

(defun format-python-with-eglot-on-save ()
  (when (eq major-mode 'python-mode)
    (eglot-format-buffer)))

;; (use-package json-rpc)

;; (use-package eldoc
;;   :config
;;   (provide 'upgraded-eldoc))
;; elpaca seems unable to install eglot without eldoc 1.14... and won't resolve that on its own for whatever reason.
(defun my/find-pyproject-root (dir)
  "Find a project root by looking for a `pyproject.toml` file."
  (when-let ((root (locate-dominating-file dir "pyproject.toml")))
    (cons 'transient root))) ; <-- Use 'transient' instead of 'vc'

(use-package
  eglot
  :ensure nil ;;(:host github :repo "joaotavora/eglot" :files (:defaults) :inherit nil)
  ;; :defer t
  :hook
  ((python-mode . eglot-ensure)
    (python-mode . highlight-numbers-mode)
    (eglot-managed-mode
      .
      (lambda ()
        (flymake-mode t)
        (set-window-margins nil 0 nil))) ;; i have no idea why eglot is adding this margin all of a sudden in 30.1
    (before-save . format-python-with-eglot-on-save))

  :init (add-hook 'before-save-hook 'format-python-with-eglot-on-save)
  :config

  ;; i do not know why this is necessary, but without it, eglot suddenly isn't finding
  ;; the actual pyproject.toml and subproject root that we want it to find.
  (add-to-list 'project-find-functions #'my/find-pyproject-root 'append) ; Ensures it's at the front

  (setq eglot-events-buffer-config '(:size 20000000 :format full))
  ;; (setq eglot-show-diagnostics-indicators nil) ;; not, apparently, necessary
  (setq-default eglot-workspace-configuration
    '
    (:pylsp
      (:plugins
        (:pycodestyle
          (:enabled nil)

          :jedi_completion (:enabled t)
          :jedi_definition (:enabled t)
          :jedi_hover (:enabled t)
          :jedi_references (:enabled t)
          :jedi_signature_help (:enabled t)
          :jedi_symbols (:enabled t)
          :rope (:enabled :json-false) ; Disable rope - it conflicts with jedi

          :pyflakes (:enabled nil)
          :isort (:enabled t)
          :flake8 (:enabled nil)
          :pylsp-mypy (:enabled t :live_mode t)
          :black (:enabled t :line_length 105))))))

;; requires https://github.com/blahgeek/emacs-lsp-booster
;; to be built and on the PATH
(use-package
  eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(provide 'new-python)
