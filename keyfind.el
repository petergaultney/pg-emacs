;;; -*- lexical-binding: t; -*-

(defun my/list-unbound-prefix-keys (prefix)
  "List unbound character and symbol keys for a given PREFIX key.
Prompts for a prefix like \"C-c\" and shows available keys."
  (interactive "sPrefix key: ")
  (let* ((keymap (lookup-key global-map (kbd prefix)))
         (used-keys '())
         (candidate-keys
          (append
           (string-to-list "abcdefghijklmnopqrstuvwxyz0123456789-=[]\\;',./")
           '(tab return backspace up down left right
             f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12)))
         (unbound-keys '()))

    (unless (keymapp keymap)
      (error "'%s' is not a prefix key" prefix))

    (map-keymap (lambda (key _binding) (push key used-keys)) keymap)

    (setq unbound-keys
          (seq-filter (lambda (key) (not (memq key used-keys)))
                      candidate-keys))

    (if unbound-keys
        (message "Unbound keys under %s: %s"
                 prefix
                 ;; --- FIX IS HERE ---
                 ;; We must convert each item (char code or symbol) into a
                 ;; valid key sequence before passing to key-description.
                 (mapconcat
                  (lambda (key)
                    (key-description (if (characterp key)
                                         (string key)    ; 97 -> "a"
                                       (vector key))))   ; 'tab -> [tab]
                  unbound-keys
                  " "))
      (message "No common unbound keys found under %s." prefix))))
