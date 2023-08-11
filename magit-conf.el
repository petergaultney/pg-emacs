(global-unset-key (kbd "C-x g"))  ;; i do not like magit activating
								  ;; itself when i'm just trying to
								  ;; Ctrl-g cancel something.
(global-set-key (kbd "C-c g") 'magit-status)
