(defun backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(defun enter-insert-and-backward-delete-one ()
  (interactive)
  (evil-insert-state)
  (backward-delete-char 1))

(setq debugbuff (get-buffer-create "*PETER-LISP-DEBUG*"))

(defun delete-line ()
  (interactive)
  (kill-line)
  (setq kill-ring (cdr kill-ring)))


(defun untabify-directory (dir)
  "Untabify all files in the given DIRECTORY."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively dir "\\."))
    (with-temp-buffer
      (insert-file-contents file)
      (untabify (point-min) (point-max))
      (write-file file))))
