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
  "Move SOURCE to DESTINATION, creating parent directory of DESTINATION if it doesn't exist.
Updates any buffers visiting the source file to visit the destination instead."
  (let
    (
      (target-dir (file-name-directory destination))
      (absolute-source (expand-file-name source)))

    ;; Create target directory if needed
    (make-directory target-dir t)

    ;; Find all buffers visiting this file
    (let
      (
        (buffers-to-update
          (cl-remove-if-not
            (lambda (buf)
              (and (buffer-file-name buf)
                (string= (expand-file-name (buffer-file-name buf)) absolute-source)))
            (buffer-list))))

      ;; Move the file
      (rename-file source destination t)

      ;; Update all buffers visiting this file
      (dolist (buf buffers-to-update)
        (with-current-buffer buf
          (set-visited-file-name destination t t))))

    (message "Moved %s -> %s" (file-name-nondirectory source) destination)))


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


(defun my/get-file-creation-time (file)
  "Get file creation time using stat, falling back to modification time if unavailable."
  (condition-case nil
    (let
      (
        (stat-output
          (shell-command-to-string
            (format "stat -f %%B %s" (shell-quote-argument file)))))
      (if (string-match "^[0-9]+$" (string-trim stat-output))
        (seconds-to-time (string-to-number (string-trim stat-output)))
        ;; stat failed, use modification time
        (let ((attrs (file-attributes file)))
          (file-attribute-modification-time attrs))))
    (error
      ;; Any error with stat command, fall back to modification time
      (let ((attrs (file-attributes file)))
        (file-attribute-modification-time attrs)))))


(defun my/rename-file-by-date (file &optional move-fn)
  "Rename a single FILE based on date formatting rules.

- Renames files from 'YYYYMMDD...' format to 'YY-MM-DD...'.
- Adds a 'YY-MM-DD_HHMM_' prefix to files that lack one,
  based on the file's modification time.

Returns the new file path after renaming, or the original path if no change.
The MOVE-FN defaults to `my/mkdir-and-move'."
  (interactive (list (read-file-name "File to rename: " nil (buffer-file-name) t)))

  (let*
    (
      (move-function (or move-fn #'my/mkdir-and-move))
      (basename (file-name-nondirectory file))
      (dir (file-name-directory file))
      (long-format-re
        "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[_-]\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]\\{2\\}[@_-]\\(.*\\)\\.md$")
      (short-format-re
        "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[_-]\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[_-]\\(.*\\)\\.md$")
      new-basename)

    (when (and (file-regular-p file) (string-suffix-p ".md" basename))
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
              (title (replace-regexp-in-string "[@_]" "-" (match-string 6 basename))))
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
          (setq new-basename basename))

        ;; Case 3: No date prefix
        (t
          (let*
            (
              (ctime (my/get-file-creation-time file))
              (title (file-name-sans-extension basename)))
            (setq new-basename
              (format "%s_%s.md" (format-time-string "%y-%m-%d_%H%M" ctime) title)))))

      ;; Rename if needed
      (if (and new-basename (not (string= basename new-basename)))
        (let ((new-file (expand-file-name new-basename dir)))
          (funcall move-function file new-file)
          new-file)
        file))))

(defun my/move-file-to-date-subdir (file days &optional move-fn)
  "Move FILE to a YYYY-MM subdirectory if it's older than DAYS.

Returns the new file path after moving, or the original path if not moved.
The MOVE-FN defaults to `my/mkdir-and-move'."
  (let*
    (
      (move-function (or move-fn #'my/mkdir-and-move))
      (cutoff-time (time-subtract (current-time) (seconds-to-time (* days 24 3600))))
      (basename (file-name-nondirectory file))
      (dir (file-name-directory file))
      (short-format-re
        "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[_-]\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[_-]\\(.*\\)\\.md$")
      file-time)

    (when (and (file-regular-p file) (string-suffix-p ".md" basename))
      ;; Extract date from filename or use file modification time
      (if (string-match short-format-re basename)
        (let*
          (
            (yy (string-to-number (match-string 1 basename)))
            (month (string-to-number (match-string 2 basename)))
            (day (string-to-number (match-string 3 basename)))
            (hour (string-to-number (match-string 4 basename)))
            (min (string-to-number (match-string 5 basename))))
          (setq file-time (encode-time 0 min hour day month (+ 2000 yy))))
        (let ((attrs (file-attributes file)))
          (setq file-time (file-attribute-modification-time attrs))))

      ;; Move if old
      (if (and file-time (time-less-p file-time cutoff-time))
        (let*
          (
            (target-dir (expand-file-name (format-time-string "%Y-%m" file-time) dir))
            (target-file (expand-file-name basename target-dir)))
          (unless (string= file target-file)
            (funcall move-function file target-file)
            target-file))
        file))))

(defun my/organize-files-by-date (directory days &optional files-to-ignore move-fn)
  "Organize and rename files in DIRECTORY based on date.

This is a generic, non-interactive function.

- First renames all files using `my/rename-file-by-date'.
- Then optionally moves old files to subdirectories using `my/move-file-to-date-subdir'.
- Skips any file whose basename is in FILES-TO-IGNORE.

If ORGANIZE-OLD-FILES is non-nil (default t), moves files older than DAYS
into 'YYYY-MM' subdirectories.

The MOVE-FN must be a function that accepts two arguments,
SOURCE and DESTINATION, and handles the file operation."
  (let ((files (directory-files directory t "\\.md$" t)))
    (dolist (file files)
      (when (file-regular-p file)
        (let ((basename (file-name-nondirectory file)))
          (unless (member basename files-to-ignore)
            ;; Step 1: Rename the file
            (let ((renamed-file (my/rename-file-by-date file move-fn)))
              ;; Step 2: Optionally move to subdir if old
              (when days
                (my/move-file-to-date-subdir renamed-file days move-fn)))))))))


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


(defun my/organize-notes-in-dir ()
  "Organize and rename markdown files in DIR based on date."
  (interactive "DDirectory: " "sDays to keep (default 2): ")
  (let*
    (
      (dir (expand-file-name dir))
      (days
        (if (or (null days-str) (string-empty-p days-str))
          2 ; Default to 2 if user hits Enter
          (string-to-number days-str))))
    (my/organize-files-by-date dir days
      '("index.md" "llm-chats.md" "14.51 LLM chats.md") ; Files to never rename or move
      #'my/mkdir-and-move) ;; change to my/fake-mkdir-and-move to test without actual file ops
    (message "Done organizing LLM chats.")))


(define-prefix-command 'file-prefix-map)
(define-key global-map (kbd "C-c f") 'file-prefix-map)

(define-key file-prefix-map (kbd "d") 'delete-current-file)
(define-key file-prefix-map (kbd "r") 'rename-current-file)
(define-key file-prefix-map (kbd "e") 'open-dotemacs) ; f-e for "file-edit-dotemacs"
(define-key file-prefix-map (kbd "l") 'reload-dotemacs)

(provide 'files)
