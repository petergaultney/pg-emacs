(defun obsidian-open-deep-link (vault-name file-name)
  "Open an Obsidian deep link for FILE-NAME in VAULT-NAME."
  (interactive "sVault name: \nsFile name: ")
  (let ((deep-link (format "obsidian://open?vault=%s&file=%s"
                          (url-hexify-string vault-name)
                          (url-hexify-string file-name))))
    (shell-command (format "open '%s'" deep-link))))

(defun obsidian-find-vault (file-path)
  "Find if FILE-PATH is in an Obsidian vault. Return (vault-name . relative-path)."
  (let ((dir (file-name-directory file-path)))
    (while (and dir (not (file-exists-p (expand-file-name ".obsidian" dir))))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    (when dir
      (cons (file-name-nondirectory (directory-file-name dir))
            (file-relative-name file-path dir)))))

(defun obsidian-open-current-file ()
  "Open current file in Obsidian if it's in a vault."
  (interactive)
  (if-let* ((file-path (buffer-file-name))
            (real-path (file-truename file-path))
            (vault-info (obsidian-find-vault real-path)))
      (let ((vault-name (car vault-info))
            (relative-path (cdr vault-info)))
        (obsidian-open-deep-link vault-name relative-path)
        (message "Opened %s in Obsidian vault '%s'" relative-path vault-name))
    (message "Current file is not in an Obsidian vault")))
