;; (defun my-log-error-to-file (error-data)
;;   "Log error information to a file with timestamps."
;;   (let ((log-file "~/.emacs.d/emacs-error-log.txt")) ; Change this path if needed
;;     (with-temp-buffer
;;       (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
;;         (insert (format "Timestamp start: %s\n" timestamp))
;;         (insert (format "Error: %s\n" error-data))
;;         (let ((debug-on-error t))
;;           (backtrace))
;;         (insert (format "Timestamp end: %s\n\n" timestamp)))
;;       (write-region (point-min) (point-max) log-file t 'silent)
;;       (message "Error logged to %s" log-file))))

(defvar my-log-error-in-progress nil
  "Non-nil if an error is currently being logged.")

(defun my-log-error-to-file (error-data)
  "Log error information to a file with timestamps, including stack traces."
  (unless my-log-error-in-progress
    (let ((my-log-error-in-progress t))
      (let ((log-file "~/.emacs.d/emacs-error-log.txt")) ; Change this path if needed
        (unless (or (and (consp error-data) (eq (car error-data) 'user-error))
                    (eq (car error-data) 'end-of-buffer))
          (with-temp-buffer
            (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
              (insert (format "Timestamp start: %s\n" timestamp))
              (insert (format "Error: %s\n" error-data))
              (insert "Backtrace:\n")
              (let ((backtrace (with-output-to-string
                                 (with-current-buffer standard-output
                                   (backtrace)))))
                (insert backtrace))
              (insert (format "Timestamp end: %s\n\n\n\n" timestamp)))
            (write-region (point-min) (point-max) log-file t 'silent)
            (message "Error logged to %s" log-file)))))))

(defun my-wrap-error-handler (orig-fun &rest args)
  "Wrap the original error handler to log errors selectively."
  (condition-case err
      (apply orig-fun args)
    ((debug error)
     (my-log-error-to-file err))))


(advice-add 'signal :around #'my-wrap-error-handler)
