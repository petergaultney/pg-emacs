(require 'whitespace)

(defun change-indent ()
  (interactive)
  (setq spaces (read-number "How many whitespaces: " 2))
    (if (y-or-n-p "Use tabs?")
      (setq indent-tabs-mode t
        c-indent-tabs-mode t
        c-tab-always-indent t
        tab-width spaces
      )
      (setq indent-tabs-mode nil
        c-indent-tabs-mode nil
        c-tab-always-indent nil
      )
    )
    (setq c-basic-indent spaces
      c-basic-offset spaces
      c-indent-level spaces
      c-argdecl-indent 0
      backward-delete-function nil
    )
    (message "Changed indentation.")
)

(global-set-key "\C-x\C-o" 'whitespace-mode)
(global-set-key "\C-x\C-i" 'change-indent)
