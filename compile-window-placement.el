(setq compilation-window-height 20)

(defun my-select-bottom-window ()
  (let ((bottom-window (selected-window))
        window-below)
    (while (setq window-below (window-in-direction 'below bottom-window))
      (setq bottom-window window-below))
    (select-window bottom-window)))

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (my-select-bottom-window)
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)
