;; local files that need to be loaded
(load "colors.el") ;; i might use this various places.
(load "temp-files.el")
(load "funcs.el")
(load "files.el")
(load "datetimes.el")
(load "minibuffer-conf.el")
(load "smarter_move_beginning_of_line")

(load "window-half-scroll.el")
(load "compile-window-placement.el")
(load "mac-copy.el")
(load "gptel-conf.el")

(load "windows.el") ;; this ABSOLUTELY MUST remain above consult-config.el, because it does a setq on display-buffer-alist

;; (load "find-file-in-project.el")

(load "coding-standards.el")
(load "unfill-paragraph.el")
;; (load "god-mode-config.el")
(load "indentation.el")

(require 'header2)

(load "Highlight-Indentation-for-Emacs/highlight-indentation.el")
;; (load "vtl")
(load "dos-eol")


(load "avy-de.el")
; (add-to-list 'custom-theme-load-path "~/.emacs.d/local_config/themes/")
; (load-theme 'zenburn t)

(load "auto-correct.el")

(load "themes.el")

(load "github-copilot.el")
(load "new-python.el")

(load "magit-conf.el")
;; (require 'org-roam-config)
(load "org-custom") ;; sometimes this used to fail so it's at the very end

(load "consult-config.el")
(load "better-defaults.el") ;; needs to see that helm/consult/whatever is loaded

(load "z-helpful.el")
(load "projectile-conf.el")
(load "javascript.el")
(load "ansi-color-conf.el")
(load "obsidian.el")
(load "shell-conf.el")
(load "keyfind.el")
(load "elisp-conf.el")
(load "modeline.el")
(load "vterm-conf.el")
(load "pg-replace.el")
(load "abbrevs.el")
(load "smerge-deactivate.el")
(load "meow-c.el")
