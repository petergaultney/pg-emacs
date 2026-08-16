;;; warnings-conf.el --- Auto-dismiss the *Warnings* buffer window after a delay.

;; NOTE: do not name this file warnings.el -- it would shadow Emacs' built-in
;; emacs-lisp/warnings.el on the load-path, and the autoload of `display-warning'
;; would find this file instead, breaking startup entirely.

(require 'warnings)

(defun pg/auto-dismiss-warnings-window ()
  "Auto-dismiss the *Warnings* buffer window after 5 seconds."
  (run-with-timer 5 nil
                  (lambda ()
                    (when-let ((win (get-buffer-window "*Warnings*")))
                      (delete-window win)))))

(advice-add 'display-warning :after
            (lambda (&rest _) (pg/auto-dismiss-warnings-window)))
