;; from Mastering EShell -- https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

(defun my-eshell-exit-kills-client (orig-fn &rest args)
  "Advise eshell/exit to kill the client frame correctly."
  (let ((frame (selected-frame))
        (client-param (frame-parameter (selected-frame) 'client)))
    (if client-param
        (progn
          (message "Client frame detected, calling save-buffers-kill-terminal")
          (save-buffers-kill-terminal)))
    (progn
      (apply orig-fn args))))

(advice-remove 'eshell/exit #'my-eshell-exit-kills-client)
(advice-add 'eshell/exit :around #'my-eshell-exit-kills-client)
