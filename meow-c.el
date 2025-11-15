(use-package
  meow
  :ensure t
  :config
  ;; define custom keybinding using Norman keyboard layout rather than QWERTY
  ;; qwdfkjurl;[]\
  ;; asetgynioh'
  ;; zxcvbpm,./
  (setq meow-expand-hint-remove-delay 3.0) ; default is 1.0
  (setq delete-active-region t)
  (setq meow--kbd-forward-char #'forward-char)
  (setq meow--kbd-backward-char #'backward-char)
  (defun meow-setup-norman ()

    (defun my/meow-enter-means-insert-mode ()
      (interactive)
      (insert-char ?\n)
      (meow-insert-mode 1))

    (meow-leader-define-key
      ;; '("SPC" . set-mark-command)

      '("RET" . my/meow-enter-means-insert-mode)
      '("e" . elisp-prefix-map)
      '("f" . find-file)
      '("b" . consult-buffer)
      '("k" . kill-buffer)
      '("1" . meow-digit-argument)
      '("2" . meow-digit-argument)
      '("3" . meow-digit-argument)
      '("4" . meow-digit-argument)
      '("5" . meow-digit-argument)
      '("6" . meow-digit-argument)
      '("7" . meow-digit-argument)
      '("8" . meow-digit-argument)
      '("9" . meow-digit-argument)
      '("0" . meow-digit-argument)
      '("/" . meow-keypad-describe-key)
      '("?" . meow-cheatsheet))
    (meow-normal-define-key
      ;; '("C-f" . forward-char)
      ;; '("C-b" . backward-char)

      ;; '("SPC" . set-mark-command)

      '("RET" . my/meow-enter-means-insert-mode)

      '("M-," . beginning-of-buffer)
      '("M-." . end-of-buffer)
      '("0" . meow-expand-0)
      '("9" . meow-expand-9)
      '("8" . meow-expand-8)
      '("7" . meow-expand-7)
      '("6" . meow-expand-6)
      '("5" . meow-expand-5)
      '("4" . meow-expand-4)
      '("3" . meow-expand-3)
      '("2" . meow-expand-2)
      '("1" . meow-expand-1)
      '("-" . negative-argument)
      '(";" . meow-reverse)
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing)
      '("(" . meow-beginning-of-thing)
      '(")" . meow-end-of-thing)
      '("/" . meow-undo)
      '("a" . smarter-move-beginning-of-line)
      '("A" . meow-open-below)
      '("b" . meow-back-word)
      '("B" . meow-back-symbol)
      '("c" . meow-change)
      '("d" . meow-delete)
      '("D" . meow-backward-delete)
      '("e" . end-of-line)
      '("E" . meow-next-symbol)
      '("f" . meow-find)
      '("g" . meow-cancel-selection)
      '("G" . meow-grab)
      '("h" . meow-insert) ;; mirrors my keyboard config
      '("i" . next-line)
      '("I" . meow-next-expand)
      '("j" . meow-join)
      '("k" . meow-kill)
      '("l" . meow-line)
      '("L" . meow-goto-line)
      '("m" . set-mark-command)
      '("M" . meow-mark-symbol)
      '("n" . backward-char)
      '("N" . meow-left-expand)
      '("o" . forward-char)
      '("O" . meow-right-expand)
      ;; '("p" . meow-yank)
      '("q" . meow-quit)
      '("Q" . meow-goto-line)
      '("r" . previous-line)
      '("R" . meow-prev-expand)
      ;; '("s" . meow-insert)
      '("S" . meow-open-above)
      '("t" . avy-goto-char-timer)
      '("u" . meow-save)
      '("U" . meow-undo-in-selection)
      '("v" . my-window-hydra-entry)
      '("w" . meow-save)
      '("x" . exchange-point-and-mark)
      '("X" . meow-backward-delete)
      '("y" . meow-yank)
      '("Y" . meow-sync-grab)
      '("z" . meow-pop-selection)
      '("'" . repeat)
      '("<escape>" . ignore))
    (meow-motion-overwrite-define-key
      ;; basic movement
      '("h" . meow-motion-mode) '("<escape>" . meow-normal-mode)))
  (meow-setup-norman)
  (meow-global-mode 1)

  (defun my/set-mark-and-normal-mode ()
    (interactive)
    (set-mark-command nil)
    (meow-normal-mode 1))

  (add-hook
    'meow-insert-mode-hook
    (lambda () (local-set-key (kbd "C-SPC") 'my/set-mark-and-normal-mode))))
