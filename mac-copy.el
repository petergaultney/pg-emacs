;; copy directly to mac clipboard with a keystroke!!
(defun pb (&optional b e)
  (interactive "r")
  (shell-command-on-region
   b
   e
	"pbcopy"))

(defun pb-from-kill-ring ()
  "Select an item from kill-ring using consult and copy to Mac clipboard."
  (interactive)
  (let ((selected
         (if (and (require 'consult nil t)
                  (fboundp 'consult--read))
             ;; Use consult if available
             (consult--read kill-ring
                            :prompt "Copy to clipboard: "
                            :sort nil
                            :category 'kill-ring
                            :require-match t)
           ;; Fallback: just grab the most recent kill
           ;; Note: I don't know if this actually works because
           ;; I don't have a nice way of testing without consult :)
           (if kill-ring
               (car kill-ring)
             (user-error "Kill ring is empty")))))
    ;; The body of the let starts here
    (with-temp-buffer
      (insert selected)
      (shell-command-on-region (point-min) (point-max) "pbcopy"))))

(global-set-key (kbd "M-c") 'pb-from-kill-ring)
