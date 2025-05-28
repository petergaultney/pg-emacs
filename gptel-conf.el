(use-package markdown-mode
  :ensure t)

(defun read-claude-api-key ()
  "Read the contents of my Claude API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.keys/claude-api"))
    (string-trim (buffer-string))))

(defun read-gemini-api-key ()
  "Read the contents of my Gemini API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.keys/gemini-api"))
    (string-trim (buffer-string))))

(defvar gptel-file-datetime-fmt "%y-%m-%d_%H%M_")
(defvar gptel-default-directory (expand-file-name "~/llm-chats"))

(defun gptel-rename-chat ()
  (interactive)
  (unless gptel-mode
    (user-error "This command is intended to be used in gptel chat buffers."))
  (let ((gptel-model 'claude-3-haiku-latest))
    (gptel-request
      (list nil                                    ;user
        "What is the chat content?"            ;llm
        (concat "```" (pcase major-mode
                        ('org-mode "org")
                        ('adoc-mode "asciidoc")
                        (_ "markdown"))
          "\n"
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
- append the extension .%s
- never give me a generic description like 'a transcript of a chat with an LLM'"
              (pcase major-mode
                ('org-mode "org")
                ('adoc-mode "adoc")
                (_ "md"))))
      :callback
      (lambda (resp info)                           ;callback called with response and request info

        (if (stringp resp)
            (let ((buf (plist-get info :buffer)))
              (when (and (buffer-live-p buf))
                (let* ((date-prefix (format-time-string gptel-file-datetime-fmt))
                      (new-name (concat date-prefix resp)))
                  (when (y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) new-name))
                    (with-current-buffer buf (rename-visited-file new-name))))))
          (message "Error(%s): did not receive a response from the LLM."
                   (plist-get info :status)))))))


(defun my-gptel-path-match-p (filepath)
  "Test if FILEPATH matches LLM chats criteria.
Returns t if the path is a markdown/adoc file in an LLM chats directory."
  (and filepath
    (string-match-p "\\.\\(?:md\\|adoc\\)$" filepath)
    (string-match-p "\\bllm[-[:space:]]chats\\b"
      (downcase (file-truename filepath)))))

(defun my-gptel-test-file (filepath)
  "Test if FILEPATH would activate gptel-mode."
  (interactive "fFile to test: ")
  (message "File %s: gptel would %sbe activated"
           filepath
           (if (my-gptel-path-match-p filepath) "" "NOT ")))

(defun my-gptel-activate ()
  "Activate `gptel-mode` for Markdown/AsciiDoc files in LLM chats directories."
  (when (my-gptel-path-match-p buffer-file-name)
    (message "Activating gptel-mode for %s" buffer-file-name)
    (gptel-mode 1)))

(defun gptel-set-default-directory ()
  (unless (buffer-file-name)
    (setq default-directory gptel-default-directory)))


(defvar gptel-global-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'gptel) ; start a new chat
    (define-key map (kbd "M-t") 'gptel-quick)
    (define-key map (kbd "M-q") 'gptel-quick)
    (define-key map (kbd "M-r") 'gptel-rename-chat)
    (define-key map (kbd "M-m") 'gptel-menu)
    (define-key map (kbd "M-d") 'gptel-mode) ; activates the mode on existing buffer
    map)
  "Global keymap for all GPTel commands.")

;; Set M-o as the global prefix for GPTel commands
(global-set-key (kbd "M-o") gptel-global-prefix-map)


(use-package gptel
  :ensure (:host github :repo "karthink/gptel" )
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key)
  (gptel-make-gemini "Gemini" :stream t :key #'read-gemini-api-key)
  (unless (alist-get 'claude-3-7-sonnet-20250219 gptel--anthropic-models)
    (add-to-list 'gptel--anthropic-models
      '(claude-3-7-sonnet-20250219
         :description "Highest level of intelligence and capability"
		 :capabilities (media tool-use cache)
         :mime-types
         ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
         :context-window 200 :input-cost 3 :output-cost 15 :cutoff-date "2024-11")))
  (unless (alist-get 'claude-sonnet-4-20250514 gptel--anthropic-models)
    (add-to-list 'gptel--anthropic-models
      '(claude-sonnet-4-20250514
         :description "High intelligence and balanced performance"
		 :capabilities (media tool-use cache)
         :mime-types
         ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
         :context-window 200 :input-cost 3 :output-cost 15 :cutoff-date "2025-03")))

  (setq
    gptel-model 'claude-sonnet-4-20250514
    gptel-backend (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key))
  :hook (gptel-mode . gptel-set-default-directory)
  :hook (markdown-mode . my-gptel-activate)
  :hook (gptel-mode . visual-line-mode))


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
      (let* ((datetime-prefix (format-time-string gptel-file-datetime-fmt))
             (extension (pcase major-mode
                          ('org-mode "org")
                          ('markdown-mode "md")
                          ('adoc-mode "adoc")
                          (_ (user-error "Unsupported major mode"))))
             (filename (file-name-concat gptel-default-directory
                                         (file-name-with-extension (concat datetime-prefix
                                                                           (simple-extras-slugify name))
                                                                   extension))))
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
