(use-package markdown-mode
  :ensure t)

(defun read-claude-api-key ()
  "Read the contents of my Claude API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.claude-api-key"))
    (string-trim (buffer-string))))

(defun gptel-rename-chat ()
  (interactive)
  (unless gptel-mode
    (user-error "This command is intended to be used in gptel chat buffers."))
  (let ((gptel-model 'gpt-4o-mini))
    (gptel-request
        (list nil                                    ;user
              "What is the chat content?"            ;llm
              (concat "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
                      (buffer-substring-no-properties (point-min) (point-max))
                      "\n```"))                      ;user
      :system
      (list (format                                  ;system message
             "I will provide a transcript of a chat with an LLM.  \
Suggest a short and informative name for a file to store this chat in.  \
Use the following guidelines:
- be very concise, one very short sentence at most
- no spaces, use underscores if required
- return ONLY the title, no explanation or summary
- append the extension .%s"
             (if (eq major-mode 'org-mode) "org" "md")))
      :callback
      (lambda (resp info)                           ;callback called with response and request info
        (if (stringp resp)
            (let ((buf (plist-get info :buffer)))
              (when (and (buffer-live-p buf)
                         (y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) resp)))
                (with-current-buffer buf (rename-visited-file resp))))
          (message "Error(%s): did not receive a response from the LLM."
                   (plist-get info :status)))))))


(defvar gptel-default-directory "~/docs/llm-chats")

(defun gptel-set-default-directory ()
  (unless (buffer-file-name)
    (setq default-directory gptel-default-directory)))


(defvar gptel-global-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'gptel) ; Bind for starting a new session
    (define-key map (kbd "t") 'gptel-quick)             ; Bind for gptel-quick
    (define-key map (kbd "r") 'gptel-rename-chat)       ; Bind for renaming chat
    (define-key map (kbd "m") 'gptel-menu)              ; Bind for GPTel menu
    (define-key map (kbd "d") 'gptel-mode) ; activates the mode on existing buffer
    map)
  "Global keymap for all GPTel commands.")

;; Set M-o as the global prefix for GPTel commands
(global-set-key (kbd "M-o") gptel-global-prefix-map)

(use-package gptel
  :ensure (:host github :repo "karthink/gptel" )
  :config
  (setq gptel-default-mode 'markdown-mode)
  (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key)
  ;; (add-hook 'gptel-mode-hook #'gptel-set-default-directory)
  (add-hook 'gptel-mode-hook #'gptel-set-default-directory nil t)
  )

(use-package posframe
  :ensure (:host github :repo "tumashu/posframe"))
(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick"))

;; all of the below comes from
;; https://github.com/benthamite/dotfiles/blob/master/emacs/extras/simple-extras.el
;; and
;; https://github.com/benthamite/dotfiles/blob/master/emacs/extras/gptel-extras.el
;; but i don't want to use the entire config for fear i'll get myself into waters that are far too deep.
;;;;; Slugify

;; mostly borrowed from Prot
(defun simple-extras-slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*/" "" str))

(defun simple-extras-slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun simple-extras-slugify (string)
  "Convert STRING into slug."
  (downcase (simple-extras-slug-hyphenate (simple-extras-slug-no-punct string))))

(defun gptel-extras-save-buffer (name _ _ interactivep)
  "Save the `gptel' buffer with NAME right after it is created.
The buffer is saved to a file in `gptel-extras-dir'. INTERACTIVEP is t when
gptel is called interactively.

This function is meant to be an `:after' advice to `gptel'."
  (when interactivep
    ;; do not run if the buffer is visiting a file, because that means the user
    ;; selected an existing buffer
    (unless (buffer-file-name (get-buffer name))
      (switch-to-buffer name)
      (let* ((extension (pcase major-mode
			  ('org-mode "org")
			  ('markdown-mode "md")
			  (_ (user-error "Unsupported major mode"))))
	     (filename (file-name-concat gptel-default-directory
					 (file-name-with-extension (simple-extras-slugify name) extension))))
	(when (derived-mode-p 'org-mode)
	  (goto-char (point-min))
	  (org-insert-heading nil nil 1)
	  (insert name)
	  (org-next-visible-heading 1)
	  (end-of-line))
	;; we temporarily remove the hook because `gptel--save-state' throws an
	;; error if called at this early stage
	(remove-hook 'before-save-hook #'gptel--save-state t)
	(write-file filename 'confirm)
	(add-hook 'before-save-hook #'gptel--save-state nil t)))))

(advice-add 'gptel :after #'gptel-extras-save-buffer)
