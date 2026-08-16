;; local files that need to be loaded
(pg-load "colors.el") ;; i might use this various places.
(pg-load "temp-files.el")
(pg-load "funcs.el")
(pg-load "files.el")
(pg-load "datetimes.el")
(pg-load "minibuffer-conf.el")
(pg-load "smarter_move_beginning_of_line.el")

(pg-load "window-half-scroll.el")
(pg-load "compile-window-placement.el")
(pg-load "mac-copy.el")
(pg-load "gptel-conf.el")

(pg-load "windows.el") ;; this ABSOLUTELY MUST remain above consult-config.el, because it does a setq on display-buffer-alist

;; (pg-load "find-file-in-project.el")

(pg-load "coding-standards.el")
(pg-load "unfill-paragraph.el")
;; (pg-load "god-mode-config.el")
(pg-load "indentation.el")

(pg-load "header2.el")

(pg-load "Highlight-Indentation-for-Emacs/highlight-indentation.el")
;; (pg-load "vtl.el")
(pg-load "dos-eol.el")


(pg-load "avy-de.el")
; (add-to-list 'custom-theme-load-path "~/.emacs.d/local_config/themes/")
; (load-theme 'zenburn t)

(pg-load "auto-correct.el")

(pg-load "themes.el")

(pg-load "github-copilot.el")
(pg-load "eca-conf.el")
(pg-load "new-python.el")

(pg-load "magit-conf.el")
;; (pg-load "org-roam-config.el")
(pg-load "org-custom.el") ;; sometimes this used to fail so it's at the very end

(pg-load "consult-config.el")
(pg-load "better-defaults.el") ;; needs to see that helm/consult/whatever is loaded

(pg-load "z-helpful.el")
(pg-load "projectile-conf.el")
(pg-load "javascript.el")
(pg-load "ansi-color-conf.el")
(pg-load "obsidian.el")
(pg-load "shell-conf.el")
(pg-load "keyfind.el")
(pg-load "elisp-conf.el")
(pg-load "modeline.el")
(pg-load "vterm-conf.el")
(pg-load "pg-replace.el")
(pg-load "abbrevs.el")
(pg-load "smerge-deactivate.el")
(pg-load "meow-c.el")
(pg-load "override-shortcuts.el")
(pg-load "warnings-conf.el")
