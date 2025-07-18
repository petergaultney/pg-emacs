(defvar my-log-error-in-progress nil
  "Non-nil if an error is currently being logged.")
;; has to be buffer-local so that it will not get unset when a recursive error happens


(defun my-get-error-log-path ()
  "Return path to today's error log file, creating directory if needed."
  (let*
    (
      (log-dir "~/.emacs.d/error-logs-by-date")
      (date-stamp (format-time-string "%Y-%m-%d"))
      (log-path (expand-file-name (concat date-stamp ".txt") log-dir)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    log-path))


(defun my-ignore-window-resize-errors (error-data)
  "Return t if the error should be ignored due to 'window-resize' in the backtrace."
  (condition-case err
    (let
      (
        (n 0)
        (frame nil))
      (while (setq frame (backtrace-frame n))
        (when (eq (nth 1 frame) 'window-resize)
          (return t))
        (setq n (1+ n)))
      nil) ; Return nil if no window-resize found
    (error (message "Error while checking backtrace: %S" err) nil)))


(defun my-log-error-to-file (error-data)
  "Log error information to a file with timestamps, filtering specific error sources."
  (unless my-log-error-in-progress
    (setq my-log-error-in-progress t)
    (unwind-protect
      (progn
        (condition-case err
          (progn
            (let
              (
                (log-file (my-get-error-log-path))
                (error-name (car error-data))) ; Extract the error name
              ;; ignore some errors that i'm not interested in...
              (unless
                (or (and (consp error-data) (eq error-name 'user-error))
                  (eq error-name 'end-of-buffer)
                  (my-ignore-window-resize-errors error-data))
                ;; end error-ignore stuff
                (with-temp-buffer
                  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
                    (insert (format "Timestamp: %s\n" timestamp))
                    (insert (format "Error Name: %s\n" error-name))
                    (insert (format "Error: %s\n" error-data))
                    (insert "Filtered Backtrace:\n")
                    (let
                      (
                        (backtrace
                          (with-output-to-string
                            (with-current-buffer standard-output
                              (backtrace)))))
                      ;; Remove frames related to the error logger itself
                      (insert
                        (replace-regexp-in-string
                          "\\`\\(?:.*\n\\)*?  signal("
                          "  signal("
                          backtrace)))
                    (insert (format "Error Name: %s\n" error-name))
                    (insert (format "Timestamp: %s\n\n" timestamp)))
                  (write-region (point-min) (point-max) log-file t 'silent)
                  (message "Error %s logged to %s" error-name log-file)))))
          (error (message "Error in logger itself: %S" err))))
      (setq my-log-error-in-progress nil))))


(defun my-wrap-error-handler (orig-fun &rest args)
  "Wrap the original error handler to log errors selectively."
  (if my-log-error-in-progress
    (apply orig-fun args)

    (condition-case err
      (apply orig-fun args)
      ((debug error) (my-log-error-to-file err)))))


(defun register-error-logger ()
  (interactive)
  (advice-mapc
    (lambda (fun props)
      (message "Removing advice %S" fun)
      (advice-remove 'signal fun))
    'signal)
  (advice-add 'signal :around #'my-wrap-error-handler))


(defun trigger-error-for-logging ()
  "Function to deliberately trigger an error for testing the error logger."
  (interactive)
  (signal 'test-error "This is a test error"))

;; Define a dummy error symbol to use for testing
(if (not (get 'test-error 'error-conditions))
  (put 'test-error 'error-conditions '(error))
  (put 'test-error 'error-message "Test error for logging"))
;; Use M-x trigger-error-for-logging to test the logger
