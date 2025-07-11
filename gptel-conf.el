(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)  ;; prefer gfm-mode because it supports underscores that aren't italics.
  :custom (markdown-fontify-code-blocks-natively t) (markdown-fontify-whole-heading t))

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
  (let ((gptel-model 'claude-3-5-haiku-20241022)
        (file-ext (pcase major-mode
                    ('org-mode "org")
                    ('adoc-mode "adoc")
                    (_ "md")))
        (code-lang (pcase major-mode
                     ('org-mode "org")
                     ('adoc-mode "asciidoc")
                     (_ "markdown"))))

    (gptel-request
      (concat "What is the chat content?\n\n"
              "```" code-lang "\n"
              (buffer-substring-no-properties (point-min) (point-max))
              "\n```")
      :system
      (format "Suggest a short filename for this chat transcript.
- Return ONLY the filename
- It must be very short - no more than 7 words!!
- Be specific about the topic discussed
- No generic names: avoid 'chat', 'LLM', 'conversation', 'transcript', 'summary'
- Use dashes, no spaces
- End with .%s" file-ext)

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
          (message "Error(%s): %s"
             (plist-get info :status)
             (plist-get info :error)))))))


(defun my/organize-llm-chats (&optional days-str directory move-fn)
  "Organize and rename LLM chat files.

Renames files from 'YYYYMMDD...' format to 'YY-MM-DD...'.
Moves files older than a specified number of days into 'YYYY-MM'
subdirectories. Interactively prompts for days, defaulting to 8.

The MOVE-FN must be a function that accepts two arguments,
SOURCE and DESTINATION, and handles the file operation."
  (interactive "sDays to keep (default 8): ")
  (let* ((days (if (or (null days-str) (string-empty-p days-str))
                   8 ; Default to 8 if user hits Enter
                 (string-to-number days-str)))
         (dir (or directory gptel-default-directory))
         ;; Define two separate regexes for clarity, as you suggested.
         (long-format-re "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)_\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]\\{2\\}[@_]\\(.*\\)\\.md$")
         (short-format-re "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)_\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)_\\(.*\\)\\.md$")
         (files (directory-files dir t "\\.md$" t)) ; Get all markdown files with full paths
         (move-function (or move-fn #'my/mkdir-and-move))
         (cutoff-time (time-subtract (current-time) (seconds-to-time (* days 24 3600)))))

    (dolist (file files)
      (when (file-regular-p file)
        (let (file-time new-basename ; These will be set by the cond block
              (basename (file-name-nondirectory file)))
          (cond
           ;; Case 1: Long format (YYYYMMDD_HHMMSS...)
           ((string-match long-format-re basename)
            (let* ((year  (string-to-number (match-string 1 basename)))
                   (month (string-to-number (match-string 2 basename)))
                   (day   (string-to-number (match-string 3 basename)))
                   (hour  (string-to-number (match-string 4 basename)))
                   (min   (string-to-number (match-string 5 basename)))
                   (title (replace-regexp-in-string "[@_]" "-" (match-string 6 basename))))
              (setq file-time (encode-time 0 min hour day month year))
              (setq new-basename (format "%02d-%02d-%02d_%02d%02d_%s.md"
                                         (- year 2000) month day hour min title))))

           ;; Case 2: Short format (YY-MM-DD_HHMM...) -- now with robust parsing
           ((string-match short-format-re basename)
            (let* ((yy    (string-to-number (match-string 1 basename)))
                   (month (string-to-number (match-string 2 basename)))
                   (day   (string-to-number (match-string 3 basename)))
                   (hour  (string-to-number (match-string 4 basename)))
                   (min   (string-to-number (match-string 5 basename))))
              (setq file-time (encode-time 0 min hour day month (+ 2000 yy)))
              (setq new-basename basename)))) ; No rename needed for this format

          ;; --- Take Action ---
          ;; This block runs only if one of the above cond clauses matched.
          (when file-time
            (let* ((is-old (time-less-p file-time cutoff-time))
                   (target-dir (if is-old (expand-file-name (format-time-string "%Y-%m" file-time) dir) dir))
                   (target-file (expand-file-name new-basename target-dir)))
              ;; Only call the move function if the path will change.
              (unless (string= file target-file)
                (funcall move-function file target-file)))))))
    (message "Done organizing LLM chats.")))



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
	:request-params '(:thinking (:type "enabled" :budget_tokens 2048)))
  (gptel-make-gemini "gemini-2.5-pro-thinking"
	:key #'read-gemini-api-key
    :stream t
	:models '(gemini-2.5-pro-preview-06-05)
	:request-params '(:generationConfig (:thinkingConfig (:thinkingBudget 2048))))

  ;; defaults
  (setq
    gptel-model 'claude-sonnet-4-20250514
    gptel-backend (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key))

  :hook (markdown-mode . my-gptel-activate)
  :hook (gfm-mode . my-gptel-activate)
  :hook (gptel-mode . gptel-set-default-directory)
  :hook (gptel-mode . visual-line-mode)
  :hook (gptel-mode . visual-fill-column-mode))


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
                          ('gfm-mode "md")
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


(defun my/gptel-save-state-wrapper (orig-fun &rest args)
  "Wrap gptel--save-state to prevent modification prompts."
  (let ((modified-p (buffer-modified-p)))
    (apply orig-fun args)  ; This calls the ORIGINAL gptel--save-state
    (set-buffer-modified-p modified-p)))

(defvar-local gptel-anthropic-use-web-search nil
  "When non-nil, enable the Anthropic web search tool for the current buffer.")

(with-eval-after-load 'gptel

  (advice-add 'gptel--save-state :around #'my/gptel-save-state-wrapper)
  (advice-add 'gptel :after #'gptel-extras-save-buffer)

  ;; (load-file "gptel-pricing.el")
  ;; (add-hook 'gptel-post-response-functions #'gptel-track-cost)

  (advice-add
    'gptel--request-data :around
    (lambda (orig-fn &rest args)
      (let ((result (apply orig-fn args)))
        (if (and gptel-anthropic-use-web-search
              (cl-typep (car args) 'gptel-anthropic))
		  (cons :tools
			(cons [(:type "web_search_20250305"
					 :name "web_search"
					 :max_uses 5)]
              result))
		  result)))))
