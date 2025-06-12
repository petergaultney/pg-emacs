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
      (list nil
        "What is the chat content?"
        (concat "```" (pcase major-mode
                        ('org-mode "org")
                        ('adoc-mode "asciidoc")
                        (_ "markdown"))
          "\n"
          (buffer-substring-no-properties (point-min) (point-max))
          "\n```"))
      :system
      (list (format
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
      (lambda (resp info)
        (if (stringp resp)
            (let ((buf (plist-get info :buffer)))
              (when (and (buffer-live-p buf))
                (let* ((current-name (buffer-file-name buf))
                       (current-basename (when current-name (file-name-nondirectory current-name)))
                       ;; Extract existing timestamp with debug message
                       (existing-timestamp
                        (progn
                          (when current-basename
                            (message "Current filename: %s" current-basename)
                            (if (string-match "^\\([0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}_[0-9]\\{4\\}\\)" current-basename)
                                (match-string 1 current-basename)
                              (message "Regex didn't match timestamp in: %s" current-basename)
                              nil))))
                       ;; Use existing timestamp or generate new one
                       (date-prefix (or existing-timestamp
                                        (format-time-string gptel-file-datetime-fmt)))
                       (new-name (concat date-prefix "_" resp)))
                  (message "Using timestamp: %s" date-prefix)
                  (when (y-or-n-p (format "Rename buffer %s to %s? "
                                         (or current-basename (buffer-name buf))
                                         new-name))
                    (with-current-buffer buf (rename-visited-file new-name))))))
          (message "Error(%s): did not receive a response from the LLM."
            (plist-get info :status)))))))


(defun mkdir-and-move (source-file target-dir target-file)
  "Create TARGET-DIR if needed and move SOURCE-FILE to TARGET-FILE."
  (make-directory target-dir t)  ; t makes it idempotent
  (rename-file source-file target-file)
  (message "Moved %s -> %s" source-file target-file))

(defun llm-chat-mkdir-and-move-stub (source-file target-dir target-file)
  "Stub that prints what would be done instead of actually doing it."
  (message "Would create dir: %s" target-dir)
  (message "Would move: %s -> %s" source-file target-file))

(defun my/organize-llm-chats (&optional directory move-fn)
  "Organize LLM chat files by moving old months into YYYY-MM directories.
Files from the current month stay in place.
MOVE-FN defaults to the stub for testing."
  (interactive)
  (let* ((dir (or directory gptel-default-directory))
         (current-month (format-time-string "%Y-%m"))
         (files (directory-files dir t "^[0-9][0-9]-[0-9][0-9]-[0-9][0-9]_.*"))
         (move-function (or move-fn #'mkdir-and-move)))

    (dolist (file files)
      (when (file-regular-p file)
        (let* ((basename (file-name-nondirectory file))
               (date-part (substring basename 0 8)) ; YY-MM-DD
               (year (+ 2000 (string-to-number (substring date-part 0 2))))
               (month (substring date-part 3 5))
               (file-month (format "%04d-%s" year month)))

          (unless (string= file-month current-month)
            (let* ((target-dir (expand-file-name file-month dir))
                   (target-file (expand-file-name basename target-dir)))

              (funcall move-function file target-dir target-file))))))
    (message "Done organizing LLM chats")))


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
  (push 'gemini-2.5-pro-preview-06-05 (gptel-backend-models (gptel-get-backend "Gemini")))
  (gptel-make-anthropic "claude-4-sonnet-thinking"
	:key #'read-claude-api-key
    :stream t
	:models '(claude-4-sonnet-20250514)
	:request-params '(:thinking (:type "enabled" :budget_tokens 2048) :max_tokens 4096))
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
