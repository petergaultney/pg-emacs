(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(defun open-dotemacs ()
  (interactive)
  (find-file pg-emacs-dir))
(defun open-lclcfg ()
  (interactive)
  (find-file pg-emacs-config-file))

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


(define-prefix-command 'file-prefix-map)
(define-key global-map (kbd "C-c f") 'file-prefix-map)

(define-key file-prefix-map (kbd "d") 'delete-current-file)
(define-key file-prefix-map (kbd "r") 'rename-current-file)
(define-key file-prefix-map (kbd "e") 'open-dotemacs)  ; f-e for "file-edit-dotemacs"
(define-key file-prefix-map (kbd "l") 'reload-dotemacs)
