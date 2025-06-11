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

(defun copy-identifier-at-point ()
  "Copy the identifier at point to the kill ring."
  (interactive)
  (when-let ((ident (thing-at-point 'symbol t))) ; 'symbol often means identifier
    (kill-new ident)
    (message "Copied: %s" ident)))

(define-key global-map (kbd "C-c <down>") 'copy-identifier-at-point)

(defun rename-current-file (new-name)
  "Rename the current file and buffer."
  (interactive
   (list (read-file-name "New name: "
                         (file-name-directory (buffer-file-name))
                         nil nil
                         (file-name-nondirectory (buffer-file-name)))))
  (let ((old-name (buffer-file-name)))
    (if old-name
        (progn
          (rename-file old-name new-name)
          (set-visited-file-name new-name)
          (rename-buffer (file-name-nondirectory new-name))
          (set-buffer-modified-p nil)
          (message "File renamed to %s" new-name))
      (error "Buffer is not visiting a file"))))

(global-set-key (kbd "C-c r") 'rename-current-file)

(defun delete-current-file ()
  "Delete the current file and kill the buffer, with confirmation."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
      (when (yes-or-no-p (format "Really delete %s? " (file-name-nondirectory file)))
        (delete-file file t)  ; t means "move to trash if possible"
        (kill-buffer)
        (message "File %s deleted" (file-name-nondirectory file)))
    (error "Buffer is not visiting a file"))))

(global-set-key (kbd "C-c d") 'delete-current-file)
