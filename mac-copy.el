;; copy straight to mac clipboard with a keystroke!!
(defun pbcopy (&optional b e)
  (interactive "r")
  (shell-command-on-region
   b
   e
   "pbcopy"))

(global-set-key (kbd "M-c") 'pbcopy)
