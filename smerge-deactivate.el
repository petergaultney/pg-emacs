(require 'smerge-mode)

(defun maybe-deactivate-smerge-mode ()
  "Deactivate `smerge-mode' if no conflict markers are found in the buffer."
  (when (and smerge-mode
             (not (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^<<<<<<< " nil t))))
    (smerge-mode -1)))

(add-hook 'after-save-hook 'maybe-deactivate-smerge-mode)
(add-hook 'after-revert-hook 'maybe-deactivate-smerge-mode)
