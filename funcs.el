;; non-interactive


;; INTERACTIVE FUNCTIONS

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive
    (progn
      (barf-if-buffer-read-only)
      '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))


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
(global-set-key "\M-Q" 'unfill-paragraph)
