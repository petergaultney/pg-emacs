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

(use-package eglot
  :ensure t
  :defer t
  :straight (:type built-in)
  :hook ((python-mode . eglot-ensure))
  :init (add-hook 'before-save-hook 'format-python-with-eglot-on-save)
  :config (setq-default eglot-workspace-configuration
                '(:pylsp
                   (:plugins
                    (:pycodestyle (:enabled nil)
						          :pyflakes (:enabled nil)
                                  :isort (:enabled t)
								  :flake8 (:enabled nil)
								  :pylsp-mypy (:enabled t)
								  :black (:enabled t :line_length 105)
                     ))))
  ;; (setq-default project-vc-extra-root-markers '("pyproject.toml"))

  )

;; (add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode t)))
(add-hook 'python-mode-hook 'highlight-numbers-mode)
(provide 'new-python)
