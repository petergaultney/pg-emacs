;; -*- coding: utf-8; lexical-binding: t -*-

	;; Configuration

(defun new-configure-company-anaconda ()
  (if (not (member 'company-anaconda company-backends))
      (push 'company-anaconda company-backends)))

;; (defun new-configure-company-jedi ()
;;   (if (not (member 'company-jedi company-backends))
;;       (push 'company-jedi company-backends)))

;; Packages

;; (use-package anaconda-mode
;;   :ensure t
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode))
;;   :bind (:map python-mode-map (("C-x C-d" . anaconda-mode-show-doc)
;;                                ("C-x C-w" . anaconda-mode-find-definitions))))

;; (use-package company-anaconda
;;   :ensure t
;;   :hook ((python-mode . new-configure-company-anaconda)))

;; (use-package company-jedi
;;   :ensure t
;;   :hook ((python-mode . new-configure-company-jedi)))

;; (use-package elpy
;;   :ensure t
;;   :init (elpy-enable))

;; (use-package jedi
;;   :ensure t
;;   :defer t
;;   :hook ((python-mode . jedi-mode)
;;          (python-mode . eglot-ensure)))

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
								  :flake8 (:enabled nil)
								  :pylsp-mypy (:enabled t)
								  :black (:enabled t :line_length 105)
                     ))))
  ;; (setq-default project-vc-extra-root-markers '("pyproject.toml"))

  )

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

(provide 'new-python)
