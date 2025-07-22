(setq ibuffer-formats
  '
  (
    (mark
      modified
      read-only
      locked
      " "
      (name 18 -1 :left :elide)
      " "
      (size 9 9 :right)
      " "
      (mode 16 16 :left :elide)
      " "
      filename-and-process)
    (mark " " (name 16 -1) " " filename)))

(setq ibuffer-formats
  '
  (
    (mark
      modified read-only locked " "
      (size 9 9 :right) " " ; All others must have fixed widths
      (mode 16 16 :left :elide) " " ;; ??
      (filename-and-process 20 20 :left :elide) " "
      (name 18 -1 :left :elide) ; The only stretchy column
      )
    ;; Also fix the format for buffers without files (e.g., *Messages*)
    (mark
      " "
      (name 16 -1) ; Stretchy column
      " " filename)))

(setq ibuffer-saved-filter-groups
  '
  (
    ("default"
      ("vterm" (mode . vterm-mode))
      ("dired" (mode . dired-mode))
      ("magit" (name . "^magit"))
      ("org" (mode . org-mode))
      ("chats"
        (and (mode . gfm-mode)
          (name
            .
            "^[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}_[0-9]\\{2\\}[0-9]\\{2\\}.*\\.md$")))
      ("emacs"
        (or (name . "^\\*scratch\\*$")
          (name . "^\\*Messages\\*$")
          (name . "^\\*Warnings\\*$")
          (name . "^\\*Help\\*$")))
      ("logs" (or (name . "^\\*PETER-LISP-DEBUG\\*$") (name . "^\\*copilot"))))))
