(defun pipenv-venv ()
  (interactive)
  (let ((venv-result (substring (shell-command-to-string "pipenv --venv") 0 -1) ))
	(print venv-result debugbuff)
	(if
	  (string-prefix-p "No" venv-result)
	  nil
	  venv-result)))

(defun pipenv-bin ()
  (interactive)
  (when (pipenv-venv)
	(concat (pipenv-venv) "/bin")))

(defun set-flychecker-executables ()
  "Configure virtualenv for flake8 and lint."
  (interactive)
  (let (pipenv-bin-dir (pipenv-bin))
	(print pipenv-bin-dir debugbuff)
	(when pipenv-bin-dir
	  (flycheck-set-checker-executable (quote python-mypy)
									   (concat (pipenv-bin-dir) "/mypy"))
	  (flycheck-set-checker-executable (quote python-pylint)
									   (concat (pipenv-bin-dir) "/mypy")))))

(use-package flycheck
  :ensure t)
  ;; :init (global-flycheck-mode))
(use-package flycheck-flow)

;;(when (require 'flycheck nil t)
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;;(add-hook 'elpy-mode-hook 'flycheck-mode))

;; (flycheck-define-checker
;;     python-mypy ""
;;     :command ("mypy"
;;               "--ignore-missing-imports"
;;               "--python-version" "3.7"
;;               source-original)
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
;;     :modes python-mode)

;; (add-to-list 'flycheck-checkers 'python-mypy t)
;; (flycheck-add-next-checker 'python-pylint 'python-mypy t)

(add-hook 'flycheck-mode-hook
          #'set-flychecker-executables 'local)
