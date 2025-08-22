(defun my/parse-filename-prefix (basename)
  "Attempt to parse a datetime prefix from BASENAME.
Returns a list (NEW-PREFIX ORIGINAL-PREFIX REST) on success, or nil.
- NEW-PREFIX: The normalized datetime string (e.g., \"24-11-30_1545\").
- ORIGINAL-PREFIX: The full string that was matched (e.g., \"20241130-\").
- REST: The remainder of the basename after the prefix."
  (let*
    (
      (patterns
        '
        ( ;; YYYY-MM-DD_HHMM(SS.ffffff) - MODIFIED
          ("^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[_T :-]\\([0-9]\\{2\\}\\):?\\([0-9]\\{2\\}\\)\\(?:\\(:[0-9]\\{2\\}\\)\\(?:[.,][0-9]+\\)?Z?\\)?"
            .
            (year month day hour min))
          ;; YY-MM-DD_HHMM(SS) - MODIFIED
          ("^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)_\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(?:[0-9]\\{2\\}\\)?"
            .
            (year month day hour min))
          ;; YYYYMMDDHHMM(SSffffff) - MODIFIED
          ("^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(?:[0-9]\\{2,\\}\\)?"
            .
            (year month day hour min))
          ;; --- Unchanged patterns below ---
          ;; YYYY-MM-DD
          ("^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" . (year month day))
          ;; YYYYMMDD
          ("^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" . (year month day))
          ;; YYYY-MM
          ("^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)" . (year month))
          ;; YY-MM-DD
          ("^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
            .
            (year month day))))
      (found nil))
    (while (and patterns (not found))
      (let*
        (
          (pattern-def (car patterns))
          (regexp (car pattern-def))
          (parts (cdr pattern-def)))

        (when (string-match regexp basename)
          (let*
            (
              (time-vals
                `
                (:year
                  ,(or (match-string 1 basename) "0")
                  :month ,(or (match-string 2 basename) "1")
                  :day ,(or (match-string 3 basename) "1")
                  :hour ,(or (match-string 4 basename) "0")
                  :min ,(or (match-string 5 basename) "0")))
              (original-prefix (match-string 0 basename))
              (rest
                (replace-regexp-in-string
                  (concat "^" (regexp-quote original-prefix) "[_ -]*")
                  ""
                  basename))
              (time
                (apply #'encode-time
                  0 ; seconds
                  (mapcar
                    #'string-to-number
                    (list
                      (plist-get time-vals :min)
                      (plist-get time-vals :hour)
                      (plist-get time-vals :day)
                      (plist-get time-vals :month)
                      (plist-get time-vals :year))))))
            (setq found
              (list
                ;; 1. New, formatted prefix
                (format-time-string
                  (pcase (length parts)
                    (5 "%y-%m-%d_%H%M") ; Y M D h m
                    (3 "%y-%m-%d") ; Y M D
                    (2 "%y-%m")) ; Y M
                  time)
                ;; 2. Original matched prefix
                original-prefix
                ;; 3. Remainder of filename
                rest)))))
      (setq patterns (cdr patterns)))
    found))


(defun my/test-parse-filename-prefix ()
  "Run a test suite for `my-gptel-parse-filename-prefix`."
  (let
    (
      (test-cases
        '
        ( ;; ---- Success Cases: Full Datetime ----
          ("Full datetime (preferred)"
            "24-11-30_1545_some-chat.md"
            ("24-11-30_1545" "24-11-30_1545" "some-chat.md"))
          ("Full datetime (long year)"
            "2024-11-30_1545-another.txt"
            ("24-11-30_1545" "2024-11-30_1545" "another.txt"))
          ("Full datetime (ISO T)"
            "2024-11-30T15:45 and stuff.org"
            ("24-11-30_1545" "2024-11-30T15:45" "and stuff.org"))
          ("Full datetime (compact)"
            "202411301545-compact-file.adoc"
            ("24-11-30_1545" "202411301545" "compact-file.adoc"))
          ("Full datetime (space sep)"
            "2024-11-30 15:45 with space.txt"
            ("24-11-30_1545" "2024-11-30 15:45" "with space.txt"))

          ;; ---- Success Cases: Date Only ----
          ("Date only (long year)"
            "2024-11-30-daily-notes.md"
            ("24-11-30" "2024-11-30" "daily-notes.md"))
          ("Date only (short year)"
            "24-11-30_another-day.org"
            ("24-11-30" "24-11-30" "another-day.org"))
          ("Date only (compact)"
            "20241130 daily log.txt"
            ("24-11-30" "20241130" "daily log.txt"))

          ;; ---- Success Cases: Month Only ----
          ("Month only"
            "2024-11-monthly-report.md"
            ("24-11" "2024-11" "monthly-report.md"))

          ;; ---- Success Cases: seconds and fractionals ----
          ;; ---- Seconds & Fractional Seconds (should be parsed but truncated) ----
          ("ISO with seconds"
            "2024-11-30T15:45:33-with-seconds.md"
            ("24-11-30_1545" "2024-11-30T15:45:33" "with-seconds.md"))
          ("ISO with Z-offset"
            "2024-11-30T15:45:33Z_zulu-time.org"
            ("24-11-30_1545" "2024-11-30T15:45:33Z" "zulu-time.org"))
          ("Compact with seconds"
            "20241130154533_compact.txt"
            ("24-11-30_1545" "20241130154533" "compact.txt"))
          ("Our format + seconds"
            "24-11-30_154533_with-seconds.md"
            ("24-11-30_1545" "24-11-30_154533" "with-seconds.md"))
          ("ISO with milliseconds"
            "2024-11-30T15:45:33.123_millis.org"
            ("24-11-30_1545" "2024-11-30T15:45:33.123" "millis.org"))
          ("ISO with comma millis"
            "2024-11-30T15:45:33,456_comma.txt"
            ("24-11-30_1545" "2024-11-30T15:45:33,456" "comma.txt"))
          ("ISO with nanoseconds"
            "2024-11-30T15:45:33.123456789_nanos.md"
            ("24-11-30_1545" "2024-11-30T15:45:33.123456789" "nanos.md"))
          ("Compact with millis"
            "20241130154533123-compact-millis.org"
            ("24-11-30_1545" "20241130154533123" "compact-millis.org"))
          ("Ending after seconds"
            "2024-11-30T15:45:33"
            ("24-11-30_1545" "2024-11-30T15:45:33" ""))
          ("Ending after millis"
            "2024-11-30T15:45:33.987"
            ("24-11-30_1545" "2024-11-30T15:45:33.987" ""))

          ;; ---- Edge Cases ----
          ("Filename is only a date" "2024-11-30" ("24-11-30" "2024-11-30" ""))
          ("Filename is date w/ time"
            "24-11-30_1545"
            ("24-11-30_1545" "24-11-30_1545" ""))
          ("Multiple separators"
            "2024-11-30__--double.txt"
            ("24-11-30" "2024-11-30" "double.txt"))
          ("Filename with just numbers" "20241130" ("24-11-30" "20241130" ""))

          ;; ---- Failure Cases (should return nil) ----
          ("No date prefix" "no-date-prefix.md" nil)
          ("Date in middle" "file-with-2024-11-30-in-middle.txt" nil)
          ("Year only" "2024_just-a-year.md" nil)
          ("Not a date" "12345-not-a-date.txt" nil)
          ("Malformed date" "2024-1-3-malformed.md" nil)
          ("Empty string" "" nil)
          ("Just a separator" "-leading-separator.txt" nil)))
      (passes 0)
      (failures 0))
    (with-current-buffer (get-buffer-create "*prefix-parse-test-results*")
      (erase-buffer)
      (dolist (test test-cases)
        (let*
          (
            (name (nth 0 test))
            (input (nth 1 test))
            (expected (nth 2 test))
            (actual (my/parse-filename-prefix input)))
          (if (equal actual expected)
            (progn
              (setq passes (1+ passes))
              (insert (format "PASS: %s\n" name)))
            (progn
              (setq failures (1+ failures))
              (insert (format "FAIL: %s\n" name))
              (insert (format "      Input:    %S\n" input))
              (insert (format "      Expected: %S\n" expected))
              (insert (format "      Actual:   %S\n\n" actual))))))
      (insert "\n-----\n")
      (insert (format "Summary: %d passed, %d failed.\n" passes failures))
      (pop-to-buffer (current-buffer)))))
`
;; (my/test-parse-filename-prefix)

(provide 'datetimes)
