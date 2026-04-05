;;; warnings.el --- Auto-dismiss the *Warnings* buffer window after a delay.

(defun pg/auto-dismiss-warnings-window ()
  "Auto-dismiss the *Warnings* buffer window after 5 seconds."
  (run-with-timer 5 nil
                  (lambda ()
                    (when-let ((win (get-buffer-window "*Warnings*")))
                      (delete-window win)))))

(advice-add 'display-warning :after
            (lambda (&rest _) (pg/auto-dismiss-warnings-window)))
