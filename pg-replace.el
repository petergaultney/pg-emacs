(defun toggle-case-fold-search ()
  "Toggle the value of `case-fold-search' between `nil' and non-nil."
  (interactive)
  ;; `case-fold-search' automatically becomes buffer-local when set
  (setq case-fold-search (not case-fold-search)))
(define-key query-replace-map (kbd "C") #'toggle-case-fold-search)
