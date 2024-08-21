;; copy directly to mac clipboard with a keystroke!!
(defun pb (&optional b e)
  (interactive "r")
  (shell-command-on-region
   b
   e
   "pbcopy"))

(global-set-key (kbd "M-c") 'pb)
