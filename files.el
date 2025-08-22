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
    (list
      (read-file-name "New name: "
        (file-name-directory (buffer-file-name))
        nil
        nil
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
        (delete-file file t) ; t means "move to trash if possible"
        (kill-buffer)
        (message "File %s deleted" (file-name-nondirectory file)))
      (error "Buffer is not visiting a file"))))

;; file organization functions

;; The helper function that performs the actual file system mutation.
(defun my/mkdir-and-move (source destination)
  "Move SOURCE to DESTINATION, creating parent directory of DESTINATION if it doesn't exist."
  (let ((target-dir (file-name-directory destination)))
    (make-directory target-dir t)) ; t makes it idempotent
  (rename-file source destination t) ;; t means overwrite
  (message "Moved %s -> %s" (file-name-nondirectory source) destination))

;; a substitute for `my/mkdir-and-move` that allows for testing without actual file operations.
(defun my/fake-mkdir-and-move (source destination)
  "Pretend to move SOURCE to DESTINATION, creating parent directory of DESTINATION if it doesn't exist."
  (let ((target-dir (file-name-directory destination)))
    (message "Would create directory %s" target-dir))
  (message "Would move %s -> %s" source destination))

(defun my/organize-files-by-date (directory days &optional files-to-ignore move-fn)
  "Organize and rename files in DIRECTORY based on date.

This is a generic, non-interactive function.

- Renames files from 'YYYYMMDD...' format to 'YY-MM-DD...'.
- Adds a 'YY-MM-DD_HHMM_' prefix to files that lack one,
  based on the file's modification time.
- Moves files older than DAYS into 'YYYY-MM' subdirectories.
- Skips any file whose basename is in FILES-TO-IGNORE.

The MOVE-FN must be a function that accepts two arguments,
SOURCE and DESTINATION, and handles the file operation."
  (let*
    (
      (dir directory)
      (long-format-re
        "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[_-]\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]\\{2\\}[@_-]\\(.*\\)\\.md$")
      (short-format-re
        "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[_-]\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[_-]\\(.*\\)\\.md$")
      (files (directory-files dir t "\\.md$" t))
      (move-function (or move-fn #'my/mkdir-and-move))
      (cutoff-time (time-subtract (current-time) (seconds-to-time (* days 24 3600)))))

    (dolist (file files)
      (when (file-regular-p file)
        (let*
          (
            (basename (file-name-nondirectory file))
            file-time
            new-basename)
          (unless (member basename files-to-ignore)
            (cond
              ;; Case 1: Long format (YYYYMMDD_HHMMSS...)
              ((string-match long-format-re basename)
                (let*
                  (
                    (year (string-to-number (match-string 1 basename)))
                    (month (string-to-number (match-string 2 basename)))
                    (day (string-to-number (match-string 3 basename)))
                    (hour (string-to-number (match-string 4 basename)))
                    (min (string-to-number (match-string 5 basename)))
                    (title
                      (replace-regexp-in-string "[@_]" "-" (match-string 6 basename))))
                  (setq file-time (encode-time 0 min hour day month year))
                  (setq new-basename
                    (format "%02d-%02d-%02d_%02d%02d-%s.md"
                      (- year 2000)
                      month
                      day
                      hour
                      min
                      title))))

              ;; Case 2: Short format (YY-MM-DD_HHMM...)
              ((string-match short-format-re basename)
                (let*
                  (
                    (yy (string-to-number (match-string 1 basename)))
                    (month (string-to-number (match-string 2 basename)))
                    (day (string-to-number (match-string 3 basename)))
                    (hour (string-to-number (match-string 4 basename)))
                    (min (string-to-number (match-string 5 basename))))
                  (setq file-time (encode-time 0 min hour day month (+ 2000 yy)))
                  (setq new-basename basename)))

              ;; Case 3: No date prefix
              (t
                (let*
                  (
                    (attrs (file-attributes file))
                    (mtime (file-attribute-modification-time attrs))
                    (title (file-name-sans-extension basename)))
                  (setq file-time mtime)
                  (setq new-basename
                    (format "%s_%s.md"
                      (format-time-string "%y-%m-%d_%H%M" file-time)
                      title)))))

            ;; --- Take Action ---
            (when file-time
              (let*
                (
                  (is-old (time-less-p file-time cutoff-time))
                  (target-dir
                    (if is-old
                      (expand-file-name (format-time-string "%Y-%m" file-time) dir)
                      dir))
                  (target-file (expand-file-name new-basename target-dir)))
                (unless (string= file target-file)
                  (funcall move-function file target-file))))))))))


(defun my/kill-unmodified-buffers-in-dir (dir)
  "Kill unmodified buffers visiting files under DIR.
Shows a list of buffers and asks for confirmation before killing."
  (interactive "DDirectory: ")
  (let*
    (
      (dir (expand-file-name dir))
      (candidates
        (cl-loop
          for buf in (buffer-list) when
          (and (buffer-file-name buf)
            (not (buffer-modified-p buf))
            (string-prefix-p dir (buffer-file-name buf)))
          collect buf)))
    (if (not candidates)
      (message "No unmodified file-visiting buffers found under %s" dir)
      (when
        (y-or-n-p
          (format "Kill these %d buffers?\n%s"
            (length candidates)
            (mapconcat #'buffer-name candidates "\n")))
        (mapc #'kill-buffer candidates)
        (message "Killed %d buffers." (length candidates))))))


(define-prefix-command 'file-prefix-map)
(define-key global-map (kbd "C-c f") 'file-prefix-map)

(define-key file-prefix-map (kbd "d") 'delete-current-file)
(define-key file-prefix-map (kbd "r") 'rename-current-file)
(define-key file-prefix-map (kbd "e") 'open-dotemacs) ; f-e for "file-edit-dotemacs"
(define-key file-prefix-map (kbd "l") 'reload-dotemacs)

(provide 'files)
