(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(ido-mode (quote both) nil (ido))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(org-agenda-files (quote ("~/workspace/work.org")))
 '(org-hide-leading-stars t)
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-startup-indented t)
 '(org-todo-keywords (quote ((sequence "TODO" "DONE" "CANCELED"))))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color) (background light)) (:background "#500"))))
 '(flymake-infoline ((((class color) (background light)) (:background "DarkGreen" :foreground "White"))))
 '(font-lock-comment-face ((nil (:foreground "red"))))
 '(hi-pink ((t (:background "pink" :foreground "black"))))
 '(highlight ((((class color) (min-colors 8) (background light)) (:background "#080"))))
 '(org-hide ((nil (:foreground "black"))))
 '(org-table ((t (:foreground "Blue"))))
 '(region ((((class color) (min-colors 8) (supports :inverse-video t)) (:background "blue" :foreground "white"))))
 '(secondary-selection ((((class color) (min-colors 8) (background light)) (:background "yellow1" :foreground "black")))))
