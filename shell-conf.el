;; Use bash for emacs subprocesses (shell-command, projectile, etc.)
;; xonsh calls stty on startup even non-interactively, which spams
;; "stty: stdin isn't a terminal" in subprocess output. Bash doesn't.
;; This doesn't affect vterm (which uses vterm-shell, set to "xonsh").
;; PATH is inherited from the emacs process, so everything is still found.
(setq shell-file-name "/bin/bash")

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
