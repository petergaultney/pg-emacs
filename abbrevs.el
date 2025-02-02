(setq abbrev-file-name "~/.emacs.d/pg-emacs/abbrev_defs") ; specify your custom path
(setq save-abbrevs 'silently) ; save abbrevs without asking
(setq-default abbrev-mode t) ; enable abbrev-mode by default
(read-abbrev-file abbrev-file-name) ; load abbrevs at startup
;; use `C-x a -` to correct a typo now and forever.
