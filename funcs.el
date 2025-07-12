;; non-interactive

;; The helper function that performs the actual file system mutation.
(defun my/mkdir-and-move (source destination)
  "Move SOURCE to DESTINATION, creating parent directory of DESTINATION if it doesn't exist."
  (let ((target-dir (file-name-directory destination)))
	(make-directory target-dir t))  ; t makes it idempotent
  (rename-file source destination t) ;; t means overwrite
  (message "Moved %s -> %s" (file-name-nondirectory source) destination))

;; The helper function that performs the actual file system mutation.
(defun my/fake-mkdir-and-move (source destination)
  "Pretend to move SOURCE to DESTINATION, creating parent directory of DESTINATION if it doesn't exist."
  (let ((target-dir (file-name-directory destination)))
	(message "Would create directory %s" target-dir))
  (message "Would moved %s -> %s" source destination))


;; INTERACTIVE FUNCTIONS


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
