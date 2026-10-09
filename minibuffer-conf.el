(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-c <") 'switch-to-minibuffer-window)

;; Backtrace on C-g out of a hang, but not on C-g out of a prompt.
(setq debug-on-quit t)
(add-to-list 'debug-ignored-errors 'minibuffer-quit)
