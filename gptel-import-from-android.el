;;; gptel-import-from-android.el --- Convert chat export to gptel format  -*- lexical-binding: t; -*-

(defun gptel-convert-chat-export (&optional filename)
  "Convert a chat export buffer or FILE to gptel-style transcript.

If called interactively with prefix or when visiting FILE, operates on
that file. When called non-interactively, if FILENAME is non-nil,
operate on that file; otherwise operate on current buffer.

Produces:

- User messages as lines starting with \"## \"
- Assistant messages as blocks starting with \"### \"
- Internal markdown headings inside assistant responses are shifted so
  the minimum heading level is `####`.
- Strips Chat Export / Exported on / Chat History / User/Assistant labels.
- Adds a Local Variables block at end with `gptel--bounds` describing
  assistant response regions."
  (interactive
    (list
      (when current-prefix-arg
        (read-file-name "Convert chat export file: " nil nil t))))
  (if filename
    (with-current-buffer (find-file-noselect filename)
      (gptel--convert-chat-export-in-buffer)
      (save-buffer))
    (gptel--convert-chat-export-in-buffer)))

(defun gptel--convert-chat-export-in-buffer ()
  "Do the conversion in the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (gptel--strip-export-header)
      (gptel--convert-chat-blocks)
      (gptel--insert-local-variables))))

(defun gptel--strip-export-header ()
  "Remove Chat Export metadata and anything before first **User:**."
  (goto-char (point-min))
  ;; Delete everything before the first **User:** marker.
  (let
    (
      (user-pos
        (save-excursion (re-search-forward "^[[:space:]]*\\*\\*User:\\*\\*" nil t))))
    (when user-pos
      (delete-region
        (point-min)
        (save-excursion
          (goto-char user-pos)
          (line-beginning-position))))))

(defun gptel--convert-chat-blocks ()
  "Convert user/assistant labelled blocks into gptel format.

Also computes and stores assistant response bounds as a buffer-local
variable `gptel--bounds` (list of (START . END))."
  (goto-char (point-min))
  (let
    (
      (bounds '())
      (case-fold-search nil))
    (while (re-search-forward "^[[:space:]]*\\*\\*\\(User\\|Assistant\\):\\*\\*" nil t)
      (let*
        (
          (role (match-string 1))
          (start (match-beginning 0)))
        (delete-region start (line-end-position))
        (delete-char 1) ; delete newline after label
        (cl-case
          (intern (downcase role)) (user (gptel--convert-user-block))
          (assistant
            (let ((resp-start (gptel--convert-assistant-block)))
              (push resp-start bounds))))))
    ;; Bounds list is currently ((START . END) ...) in reverse order
    (setq-local gptel--bounds (nreverse bounds))))

(defun gptel--convert-user-block ()
  "Convert a user block starting at point into a single-line '## ' entry.

Assumes point is at start of the first line of user content."
  (let
    (
      (msg-start (point))
      msg-end
      text-lines)
    (while
      (and (not (eobp))
        (not (looking-at "^[[:space:]]*\\*\\*\\(User\\|Assistant\\):\\*\\*")))
      (let
        (
          (line
            (string-trim-right
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))))
        (push line text-lines))
      (forward-line 1))
    (setq msg-end (point))
    (delete-region msg-start msg-end)
    (let*
      (
        (joined (string-join (nreverse text-lines) " "))
        (joined (string-trim joined)))
      (insert "## " joined "\n\n"))))

(defun gptel--convert-assistant-block ()
  "Convert an assistant block starting at point.

Returns a cons (START . END) indicating the region of the assistant
response after conversion."
  (let
    (
      (resp-start nil)
      (block-start (point)))
    ;; Read lines until next label or end of buffer
    (while
      (and (not (eobp))
        (not (looking-at "^[[:space:]]*\\*\\*\\(User\\|Assistant\\):\\*\\*")))
      (forward-line 1))
    (let*
      (
        (block-end (point))
        (raw (buffer-substring-no-properties block-start block-end)))
      ;; Replace region with transformed text
      (delete-region block-start block-end)
      (setq resp-start (point))
      (insert (gptel--transform-assistant-text raw))
      (let ((resp-end (point)))
        (insert "\n\n") ;; visual separation; outside bounds
        (cons resp-start resp-end)))))

(defun gptel--transform-assistant-text (text)
  "Transform assistant TEXT into gptel format.

- Remove leading/trailing blank lines.
- First non-empty line becomes '### <line>'.
- Subsequent lines:
  - Maintain content.
  - Headings have their level increased so minimum is '####'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Trim leading/trailing blank lines
    (gptel--trim-blank-lines)
    (goto-char (point-min))
    (let ((lines '()))
      (while (not (eobp))
        (push
          (buffer-substring-no-properties (line-beginning-position) (line-end-position))
          lines)
        (forward-line 1))
      (setq lines (nreverse lines))
      (let
        (
          (out-lines '())
          (first-line-done nil))
        (dolist (line lines)
          (let ((trim (string-trim-right line)))
            (cond
              (
                (and (not first-line-done)
                  (not (string-match-p "\\`[[:space:]]*\\'" trim)))
                ;; First non-empty line: prefix with ###
                (setq first-line-done t)
                (push (concat "### " trim) out-lines))
              (first-line-done
                ;; Body lines: adjust headings as needed
                (push (gptel--shift-heading-level trim) out-lines))
              (t
                ;; leading empty lines before first content: skip
                ))))
        (setq out-lines (nreverse out-lines))
        (mapconcat #'identity out-lines "\n")))))

(defun gptel--shift-heading-level (line)
  "If LINE is a markdown heading, shift its level so minimum is ####.

- '# ' → '#### '
- '## ' → '##### '
- '### ' → '###### '
- '#### ' or deeper stays as is."
  (if (string-match "\\`\\([[:space:]]*\\)\\(#+\\)\\(\\s-+.*\\)" line)
    (let*
      (
        (indent (match-string 1 line))
        (hashes (match-string 2 line))
        (rest (match-string 3 line))
        (count (length hashes)))
      (if (>= count 4)
        line
        (let*
          (
            (needed 4) ; minimum visible level
            (extra (- needed count))
            (new-hashes (make-string (+ count extra) ?#)))
          (concat indent new-hashes rest))))
    line))

(defun gptel--trim-blank-lines ()
  "Trim blank lines at beginning and end of current buffer."
  (goto-char (point-min))
  (while (and (not (eobp)) (looking-at-p "^[[:space:]]*$"))
    (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
  (goto-char (point-max))
  (while
    (and (not (bobp))
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        (looking-at-p "^[[:space:]]*$")))
    (forward-line -1)
    (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
  (goto-char (point-min)))

(defun gptel--insert-local-variables ()
  "Insert/replace Local Variables block with gptel info at end of buffer.

Uses the buffer-local variable `gptel--bounds` (set earlier) and writes:

<!-- Local Variables: -->
<!-- gptel-model: gpt-5.1 -->
<!-- gptel--backend-name: \"OpenAI\" -->
<!-- gptel--bounds: ((START . END) ...) -->
<!-- End: -->"
  (let
    (
      (vars-start nil)
      (vars-end nil))
    (save-excursion
      ;; Try to find existing block
      (goto-char (point-max))
      (when (re-search-backward "^<!-- Local Variables: -->" nil t)
        (setq vars-start (match-beginning 0))
        (when (re-search-forward "^<!-- End: -->" nil t)
          (setq vars-end (line-end-position))))
      ;; Delete existing block if present
      (when (and vars-start vars-end)
        (delete-region vars-start vars-end)
        ;; Remove trailing newline if any
        (when (and (not (bobp)) (eq (char-before) ?\n))
          (delete-char -1)))
      ;; Insert new block
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (let*
        (
          (bounds (and (boundp 'gptel--bounds) gptel--bounds))
          (bounds-str (prin1-to-string bounds)))
        (insert "<!-- Local Variables: -->\n")
        (insert "<!-- gptel-model: gpt-5.1 -->\n")
        (insert "<!-- gptel--backend-name: \"OpenAI\" -->\n")
        (insert (format "<!-- gptel--bounds: %s -->\n" bounds-str))
        (insert "<!-- End: -->\n")))))


(provide 'gptel-import-from-android)
;;; gptel-import-from-android.el ends here
