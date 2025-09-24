(require 'files)

(use-package
  markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode) ;; prefer gfm-mode because it supports underscores that aren't italics.
  :bind
  ((:map markdown-mode-map ("C-c m b" . markdown-insert-code-block))
    (:map gfm-mode-map ("C-c m b" . markdown-insert-code-block)))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-whole-heading t))

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

(defun read-openai-api-key ()
  "Read the contents of my OpenAI API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.keys/openai-api"))
    (string-trim (buffer-string))))

(defun read-openrouter-api-key ()
  "Read the contents of my OpenRouter API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.keys/openrouter-api"))
    (string-trim (buffer-string))))

(defvar gptel-file-datetime-fmt "%y-%m-%d_%H%M_")
(defvar gptel-default-directory (expand-file-name "~/llm-chats"))


(require 'datetimes)


(defun gptel-rename-chat ()
  (interactive)
  (when
    (and (not gptel-mode) (not (yes-or-no-p "Not a gptel chat. Continue with rename? ")))
    (user-error "Aborted"))
  (let
    (
      (gptel-model 'claude-3-5-haiku-20241022)
      (file-ext
        (pcase major-mode
          ('org-mode "org")
          ('adoc-mode "adoc")
          (_ "md")))
      (code-lang
        (pcase major-mode
          ('org-mode "org")
          ('adoc-mode "asciidoc")
          (_ "markdown"))))

    (gptel-request
      (concat
        "What is the chat content?\n\n"
        "```"
        code-lang
        "\n"
        (buffer-substring-no-properties (point-min) (point-max))
        "\n```")
      :system
      (format
        "Suggest a short filename for this chat transcript.
- Return ONLY the filename
- It must be very short - no more than 7 words!!
- Be specific about the topic discussed
- No generic names: avoid 'chat', 'LLM', 'conversation', 'transcript', 'summary'
- Use dashes, no spaces
- End with .%s"
        file-ext)

      :callback
      (lambda (resp info)
        (if (stringp resp)
          (let ((buf (plist-get info :buffer)))
            (when (and buf (buffer-live-p buf))
              (let*
                (
                  (current-name (buffer-file-name buf))
                  (current-basename
                    (when current-name
                      (file-name-nondirectory current-name)))
                  (file-ext
                    (when current-basename
                      (file-name-extension current-basename)))
                  (prefix-info
                    (when current-basename
                      (my/parse-filename-prefix current-basename)))
                  ;; 1. Get the date prefix, either parsed or brand new
                  (date-prefix
                    (if prefix-info
                      (nth 0 prefix-info) ; Use parsed/reformatted prefix
                      (format-time-string gptel-file-datetime-fmt))) ; Generate new
                  ;; 2. Construct the full new name
                  (new-name (concat date-prefix "_" resp)))
                (when
                  (and new-name
                    (y-or-n-p
                      (format "Rename buffer %s to %s? "
                        (or current-basename (buffer-name buf))
                        new-name)))
                  (with-current-buffer buf
                    (rename-visited-file new-name))))))
          ;; Handle GPT error case
          (message "Error(%s): %s" (plist-get info :status) (plist-get info :error)))))))


;;;; #### User-Facing Wrapper Function
(defun my/organize-llm-chats (&optional days-str)
  "Interactively organize chats in `gptel-default-directory`.

This is a wrapper around `my/organize-files-by-date`. It
prompts for the number of days of recent files to keep in the
main directory and ignores 'index.md'."
  (interactive "sDays to keep (default 2): ")
  (let
    (
      (days
        (if (or (null days-str) (string-empty-p days-str))
          2 ; Default to 2 if user hits Enter
          (string-to-number days-str))))
    (my/organize-files-by-date
      gptel-default-directory days
      '("index.md" "llm-chats.md" "14.51 LLM chats.md") ; Files to never rename or move
      #'my/mkdir-and-move) ;; change to my/fake-mkdir-and-move to test without actual file ops
    (message "Done organizing LLM chats.")))


(defun my-gptel-path-match-p (filepath)
  "Test if FILEPATH matches LLM chats criteria.
Returns t if the path is a markdown/adoc file in an LLM chats directory."
  (and filepath
    (string-match-p "\\.\\(?:md\\|adoc\\)$" filepath)
    (string-match-p "\\bllm[-[:space:]]chats\\b" (downcase (file-truename filepath)))))

(defun my-gptel-test-file (filepath)
  "Test if FILEPATH would activate gptel-mode."
  (interactive "fFile to test: ")
  (message "File %s: gptel would %sbe activated"
    filepath
    (if (my-gptel-path-match-p filepath)
      ""
      "NOT ")))

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


(use-package
  gptel
  :ensure (:host github :repo "karthink/gptel")
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key)
  (gptel-make-gemini "Gemini" :stream t :key #'read-gemini-api-key)

  (push 'gemini-2.5-pro-preview-06-05 (gptel-backend-models (gptel-get-backend "Gemini")))

  (gptel-make-anthropic
    "claude-4-sonnet-thinking"
    :key #'read-claude-api-key
    :stream t
    :models '(claude-4-sonnet-20250514)
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)))
  (gptel-make-gemini
    "gemini-2.5-pro-thinking"
    :key #'read-gemini-api-key
    :stream t
    :models '(gemini-2.5-pro-preview-06-05)
    :request-params '(:generationConfig (:thinkingConfig (:thinkingBudget 2048))))

  (gptel-make-openai
    "gpt-5"
    :stream t
    :key #'read-openai-api-key
    :models '(gpt-5 gpt-5-mini gpt-5-nano))

  (gptel-make-openai
    "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'read-openrouter-api-key
    :models '(perplexity/sonar-pro perplexity/sonar-pro:online perplexity/r1-1776))

  ;; defaults
  (setq
    gptel-model 'claude-sonnet-4-20250514
    gptel-backend (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key)
    gptel--system-message
    (concat
      "You are a large language model living in Emacs and a helpful assistant. Respond concisely.\n\n"
      "When using markdown formatting in responses, start headers at level 4 (####) "
      "and use deeper levels as needed (##### for subsections, etc.). Never use # or ## or ### headers."
      "Never put Markdown of any kind on the first line of your response - if necessary, start answering after a single newline."))
  (add-to-list gptel-post-response-functions 'save-buffer)

  :hook (markdown-mode . my-gptel-activate)
  :hook (gfm-mode . my-gptel-activate)
  :hook (gptel-mode . gptel-set-default-directory)
  :hook (gptel-mode . visual-line-mode)
  :hook (gptel-mode . visual-fill-column-mode)

  :custom
  (gptel-log-level 'info)
  (gptel-default-mode 'gfm-mode)
  (gptel-prompt-prefix-alist
    '
    ((adoc-mode . "== ")
      (markdown-mode . "## ")
      (org-mode . "** ")
      (text-mode . "## ")
      (gfm-mode . "## ")))
  (gptel-response-prefix-alist
    '
    ((markdown-mode . "### ")
      (adoc-mode . "=== ")
      (org-mode . "*** ")
      (text-mode . "### ")
      (gfm-mode . "### "))))


(use-package posframe :ensure (:host github :repo "tumashu/posframe"))
(use-package gptel-quick :ensure (:host github :repo "karthink/gptel-quick"))

;; a handy script for calling gptel from the command line.
;; #!/bin/bash
;; # eclaude script
;;
;; query="$*"
;; temp_file="/tmp/gptel_response_$$"
;;
;; emacsclient --eval "(progn
;;   (require 'gptel)
;;   (gptel-request
;;     \"$query\"
;;     :callback (lambda (response info)
;;                 (with-temp-file \"$temp_file\"
;;                   (insert response)))))"  > /dev/null 2>&1
;;
;; # Wait for response file to appear and have content
;; while [ ! -s "$temp_file" ]; do
;;     sleep 0.1
;; done
;;
;; cat "$temp_file"
;; echo ""
;; rm "$temp_file"


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
      "-\\{2,\\}"
      "-"
      (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun simple-extras-slugify (string)
  "Convert STRING into slug."
  (downcase (simple-extras-slug-hyphenate (simple-extras-slug-no-punct string))))


(defun gptel-extras-save-buffer (name _ _ interactivep)
  "Save the `gptel' buffer with NAME right after it is created."
  (when interactivep
    (let ((buffer (get-buffer name)))
      (when (and buffer (not (buffer-file-name buffer)))
        (with-current-buffer buffer
          ;; 1. Prepare the buffer content (e.g., Org title)
          (when (derived-mode-p 'org-mode)
            (goto-char (point-min))
            (org-insert-heading nil nil 1)
            (insert name)
            (org-next-visible-heading 1)
            (end-of-line))

          ;; 2. Manually run gptel's state saving function.
          ;; This adds the metadata comment block to the buffer content.
          (gptel--save-state)

          ;; 3. Now, save the buffer, which already contains the metadata.
          (let*
            (
              (datetime-prefix (format-time-string gptel-file-datetime-fmt))
              (extension
                (pcase major-mode
                  ('org-mode "org")
                  ((or 'markdown-mode 'gfm-mode) "md")
                  ('adoc-mode "adoc")
                  (_ (user-error "Unsupported major mode"))))
              (filename
                (file-name-concat gptel-default-directory
                  (file-name-with-extension
                    (concat datetime-prefix (simple-extras-slugify name))
                    extension))))
            (write-file filename)
            ;; The buffer is now visiting a file and is not modified.
            (set-buffer-modified-p nil)))))))


(defvar-local gptel-anthropic-use-web-search nil
  "When non-nil, enable the Anthropic web search tool for the current buffer.")
;; how to set this for my current buffer?

(with-eval-after-load 'gptel

  (advice-add 'gptel :after #'gptel-extras-save-buffer)

  ;; (load-file "gptel-pricing.el")
  ;; (add-hook 'gptel-post-response-functions #'gptel-track-cost)

  (advice-add
    'gptel--request-data
    :around
    (lambda (orig-fn &rest args)
      (let ((result (apply orig-fn args)))
        (if (and gptel-anthropic-use-web-search (cl-typep (car args) 'gptel-anthropic))
          (cons
            :tools (cons [(:type "web_search_20250305" :name "web_search" :max_uses 5)] result))
          result)))))
