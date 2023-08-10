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

