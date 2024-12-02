;; elisp for custom window management

(defun delete-window-vertically-or-horizontally ()
  "Delete an adjacent window and expand the current window in that direction."
  ;; this one works but oddly gives an error when trying to call enlarge-window
  (interactive)

  (let* ((current-window (selected-window))
         (directions '(above below left right))
         (adjacent-window (cl-loop for dir in directions
                                   for win = (window-in-direction dir)
                                   when win return (cons dir win))))
    (when adjacent-window
      (let ((dir (car adjacent-window))
            (win (cdr adjacent-window)))
        (delete-window win)
        (cond
         ((eq dir 'above) (enlarge-window (window-height win)))
         ((eq dir 'below) (enlarge-window (window-height win)))
         ((eq dir 'left) (enlarge-window (window-width win) t))
         ((eq dir 'right) (enlarge-window (window-width win) t)))))))


(defun delete-adjacent-window-and-expand ()
  ;; this one seems not to work.
  "Delete an adjacent window and expand the current window in that direction."
  (interactive)
  (let* ((current-window (selected-window))
         (directions '(above below left right))
         (adjacent-window (cl-loop for dir in directions
                                   for win = (window-in-direction dir)
                                   when win return (cons dir win))))
    (when adjacent-window
      (let* ((dir (car adjacent-window))
             (win (cdr adjacent-window)))
        (window-resize current-window
                       (if (member dir '(left right))
                           (- (window-width win))
                           (- (window-height win)))
                       (member dir '(left right))
                      )
        (delete-window win)))))

  ;; (let* ((current-window (selected-window))
  ;;        (directions '(above below left right))
  ;;        (adjacent-window (cl-loop for dir in directions
  ;;                                  for win = (window-in-direction dir)
  ;;                                  when win return (cons dir win))))
  ;;   (when adjacent-window
  ;;     (let ((dir (car adjacent-window))
  ;;           (win (cdr adjacent-window)))
  ;;       (delete-window win)
  ;;       (maximize-window current-window dir)))))

  ;; "Delete the window above/below or to the left/right of the current window."
  ;; (interactive)
  ;; (let ((deleted nil)
  ;;       (window (selected-window)))
  ;;   (dolist (dir '(below above right left) deleted)
  ;;     (when (and (not deleted) (window-in-direction dir window))
  ;;       (setq deleted t)
  ;;       (delete-window (window-in-direction dir window))))
  ;;   (unless deleted
  ;;     (message "No adjacent window to delete!"))))

;; Optionally bind the function to a key:
(global-set-key (kbd "C-c C-w") 'delete-window-vertically-or-horizontally)
