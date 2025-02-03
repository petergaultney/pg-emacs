;; elisp for custom window management
(winner-mode 1)
(setq winner-dont-bind-my-keys 1)

;; these are my chosen shortcuts
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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


(defun close-most-recently-created-window ()
  "Close the most recently created window."
  (interactive)
  (let ((sorted-windows
         (sort (window-list)
               (lambda (w1 w2)
                 (> (window-parameter w1 'window-creation))
                    (window-parameter w2 'window-creation)))))
    (when sorted-windows
      (delete-window (car sorted-windows)))))

(defun close-warnings-buffer-window ()
  "Close the window displaying the *Warnings* buffer, if visible."
  (interactive)
  (let ((warnings-buffer (get-buffer "*Warnings*")))
    (when warnings-buffer
      (delete-windows-on warnings-buffer))))

(defvar my-window-commands-map (make-sparse-keymap)
  "A keymap for window management commands.")

;; Bind the keymap to C-c w
(global-set-key (kbd "C-v") my-window-commands-map) ;; this is normally scroll-up-command, but i _never_ use that.

;; Bind 'k' to close-most-recently-created-window
(define-key my-window-commands-map (kbd "k") 'close-most-recently-created-window)
(define-key my-window-commands-map (kbd "w") 'close-warnings-buffer-window)

;; Bind 'v' to your custom 'delete-adjacent-window-and-expand' function
(define-key my-window-commands-map (kbd "v") 'delete-window-vertically-or-horizontally)
(define-key my-window-commands-map (kbd "u") 'winner-undo)
(define-key my-window-commands-map (kbd "r") 'winner-undo)

(define-key my-window-commands-map (kbd "<left>") 'windmove-left)
(define-key my-window-commands-map (kbd "<right>") 'windmove-right)
(define-key my-window-commands-map (kbd "<up>") 'windmove-up)
(define-key my-window-commands-map (kbd "<down>") 'windmove-down)
