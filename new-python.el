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

(use-package
  eglot
  :ensure nil ;;(:host github :repo "joaotavora/eglot" :files (:defaults) :inherit nil)
  ;; :defer t
  :hook ((python-mode . eglot-ensure)
          (python-mode . highlight-numbers-mode)
          (eglot-managed-mode .
            (lambda ()
              (flymake-mode t)
              (set-window-margins nil 0 nil)))  ;; i have no idea why eglot is adding this margin all of a sudden in 30.1
          (before-save . format-python-with-eglot-on-save))

  :init (add-hook 'before-save-hook 'format-python-with-eglot-on-save)
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full))
  ;; (setq eglot-show-diagnostics-indicators nil) ;; not, apparently, necessary
  (setq-default eglot-workspace-configuration
    '(:pylsp
       (:plugins
         (:pycodestyle (:enabled nil)
           :pyflakes (:enabled nil)
           :isort (:enabled t)
           :flake8 (:enabled nil)
           :pylsp-mypy (:enabled t)
           :black (:enabled t :line_length 105))))))

;; requires https://github.com/blahgeek/emacs-lsp-booster
;; to be built and on the PATH
(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config    (eglot-booster-mode))

(provide 'new-python)
