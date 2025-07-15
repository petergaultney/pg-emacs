;; elisp for custom window management
(winner-mode 1)
(setq winner-dont-bind-my-keys 1)
(setq windmove-wrap-around t)

(defun delete-window-vertically-or-horizontally ()
  "Delete an adjacent window and expand the current window in that direction."
  ;; this one works but oddly gives an error when trying to call enlarge-window
  (interactive)

  (let*
    (
      (current-window (selected-window))
      (directions '(above below left right))
      (adjacent-window
        (cl-loop
          for
          dir
          in
          directions
          for
          win
          =
          (window-in-direction dir)
          when
          win
          return
          (cons dir win))))
    (when adjacent-window
      (let
        (
          (dir (car adjacent-window))
          (win (cdr adjacent-window)))
        (delete-window win)
        (cond
          ((eq dir 'above)
            (enlarge-window (window-height win)))
          ((eq dir 'below)
            (enlarge-window (window-height win)))
          ((eq dir 'left)
            (enlarge-window (window-width win) t))
          ((eq dir 'right)
            (enlarge-window (window-width win) t)))))))


(defun close-most-recently-created-window ()
  "Close the most recently created window."
  (interactive)
  (let
    (
      (sorted-windows
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

;; Bind the keymap to C-v w
;; (global-set-key (kbd "C-v") my-window-commands-map) ;; this is normally scroll-up-command, but i _never_ use that.

;; Bind 'k' to close-most-recently-created-window
(define-key my-window-commands-map (kbd "w") 'close-warnings-buffer-window)

;; Bind 'v' to your custom 'delete-adjacent-window-and-expand' function
;; (define-key my-window-commands-map (kbd "v") 'delete-window-vertically-or-horizontally)
(define-key my-window-commands-map (kbd "u") 'winner-undo)
(define-key my-window-commands-map (kbd "r") 'winner-undo)

(define-key my-window-commands-map (kbd "<left>") 'windmove-left)
(define-key my-window-commands-map (kbd "a") 'windmove-left) ;; like beginning of line
(define-key my-window-commands-map (kbd "<right>") 'windmove-right)
(define-key my-window-commands-map (kbd "e") 'windmove-right) ;; like end of line
(define-key my-window-commands-map (kbd "<up>") 'windmove-up)
(define-key my-window-commands-map (kbd "<down>") 'windmove-down)
(define-key my-window-commands-map (kbd "C-<left>") 'windmove-left)
(define-key my-window-commands-map (kbd "C-<right>") 'windmove-right)
(define-key my-window-commands-map (kbd "C-<up>") 'windmove-up)
(define-key my-window-commands-map (kbd "C-<down>") 'windmove-down)

;; Use the `display-buffer-alist` in your `init.el` file to control how windows are split.
;; You can set `display-buffer-alist` to enforce horizontal splits by default. For example:
(setq display-buffer-alist
  '
  (
    (".*"
      (display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . nil))))
;; Adjust how Emacs splits windows based on buffer size and ensure it uses side-by-side splits:
(setq split-height-threshold nil)
(setq split-width-threshold 200)


(defun allow-more-windows ()
  (interactive)
  (setq split-width-threshold nil))

;; put everything i'm used to under this same C-v prefix (instead of C-x)
(define-key my-window-commands-map (kbd "0") 'delete-window)
;; (define-key my-window-commands-map (kbd "k") 'delete-window)
(define-key my-window-commands-map (kbd "d") 'delete-window) ;; probably better
(define-key my-window-commands-map (kbd "1") 'delete-other-windows)
(define-key my-window-commands-map (kbd "2") 'split-window-below)
(define-key my-window-commands-map (kbd "h") 'split-window-below) ;; h for horizontal
(define-key my-window-commands-map (kbd "3") 'split-window-right)
(define-key my-window-commands-map (kbd "v") 'split-window-right) ;; v for vertical
(define-key my-window-commands-map (kbd "4") 'allow-more-windows)
(define-key my-window-commands-map (kbd "b") 'balance-windows)

;; (use-package switch-window
;;   :ensure t
;;   :config
;;   (define-key my-window-commands-map (kbd "C-w") 'switch-window)
;;   (setq switch-window-shortcut-style 'qwerty)
;;   (setq switch-window-shortcut-appearance 'asciiart)
;;   (setq switch-window-qwerty-shortcuts
;;     '("a" "s" "e" "t" "n" "i" "o" "h"))
;;   )
;;
;; not as good as ace-window, because it blanks the whole buffer to show the window 'letters'

(use-package
  ace-window
  :ensure t
  :config
  (define-key my-window-commands-map (kbd "C-v") 'ace-window)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?e ?t ?q ?w ?d ?f)))

(defvar-local my/hydra-original-mode-line-bg nil
  "Original background color of the mode line before hydra activation.")

;; (set-face-background 'mode-line nil)  ;; fix the mode line
(message "defining window hydra")

(defhydra
  hydra-window
  (:timeout
    1
    :post
    (progn
      (set-face-background 'mode-line my/hydra-original-mode-line-bg)
      (force-mode-line-update)))
  "window"

  ("a" windmove-left "left")
  ("w" windmove-up "up")
  ("s" windmove-down "down")
  ("e" windmove-right "right")

  ("t" ace-window "ace-window")

  ("b" balance-windows "balance")
  ("u" winner-undo "undo")
  ("r" winner-redo "redo")

  ("<left>" windmove-left "left")
  ("<down>" windmove-down "down")
  ("<up>" windmove-up "up")
  ("<right>" windmove-right "right")

  ("o" windmove-right "right")
  ("n" windmove-left "left")
  ("i" windmove-down "down")
  ("r" windmove-up "up")

  ("v" split-window-right "vsplit")
  ("h" split-window-below "hsplit")
  ("d" delete-window "delete")
  ("q" delete-other-windows "delete other windows")
  ("1" delete-other-windows "delete other windows")

  ("g" nil "quit")
  ("RET" nil "quit"))

(message "defined window hydra")

(defun my-window-hydra-entry ()
  (interactive)
  (setq my/hydra-original-mode-line-bg (face-background 'mode-line))
  (set-face-background 'mode-line "#ff6666")
  (force-mode-line-update)
  (hydra-window/body))

(global-set-key (kbd "C-v") #'my-window-hydra-entry)
(message "done loading windows.el")
