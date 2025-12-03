(define-prefix-command 'file-prefix-map)
(define-key global-map (kbd "C-c f") 'file-prefix-map)

(define-key file-prefix-map (kbd "d") 'delete-current-file)
(define-key file-prefix-map (kbd "r") 'rename-current-file)
(define-key file-prefix-map (kbd "e") 'open-dotemacs) ; f-e for "file-edit-dotemacs"
(define-key file-prefix-map (kbd "l") 'reload-dotemacs)
