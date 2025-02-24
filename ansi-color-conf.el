(require 'ansi-color)

(defun ansi-colors-display ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
