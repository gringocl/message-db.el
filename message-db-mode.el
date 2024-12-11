;; message-db-mode.el --- Major mode for message-db results -*- lexical-binding: t -*-

(require 'transient)

(defgroup message-db nil
  "Major mode for viewing message-db query results."
  :group 'data)

(defvar message-db-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'message-db-refresh)
    (define-key map (kbd "RET") #'message-db-inspect-at-point)
    map)
  "Keymap for message-db-mode.")

(define-derived-mode message-db-mode special-mode "Message-DB"
  "Major mode for viewing message-db query results.

\\{message-db-mode-map}")

(defun message-db--format-active-streams (results)
  "Format RESULTS of active streams query for display."
  (with-temp-buffer
    (insert results)
    (goto-char (point-min))
    (let ((table (make-vector 2 0))  ; For column widths
          formatted-lines)
      ;; First pass: collect maximum column widths
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([^|]+\\)|\\s-*\\([0-9]+\\)" line)
            (let ((stream (match-string 1 line))
                  (pos (match-string 2 line)))
              (aset table 0 (max (aref table 0) (length stream)))
              (aset table 1 (max (aref table 1) (length pos))))))
        (forward-line 1))

      ;; Second pass: format lines
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([^|]+\\)|\\s-*\\([0-9]+\\)" line)
            (let ((stream (string-trim (match-string 1 line)))
                  (pos (string-trim (match-string 2 line))))
              (push (format (format "%%-%ds  %%%ds"
                                  (aref table 0)
                                  (aref table 1))
                          stream pos)
                    formatted-lines))))
        (forward-line 1))

      ;; Return formatted string
      (concat "Stream Name" (make-string (- (aref table 0) 10) ? )
              "  Latest Position\n"
              (make-string (aref table 0) ?=) "  "
              (make-string (aref table 1) ?=) "\n"
              (mapconcat #'identity (nreverse formatted-lines) "\n")))))

(defun message-db-inspect-at-point ()
  "Inspect the message at point in detail."
  (interactive)
  (if-let* ((bounds (bounds-of-thing-at-point 'line))
            (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
            (id (when (string-match "id: \\([^ ]+\\)" line)
                  (match-string 1 line))))
      (message-db--display-message id)
    (user-error "No message at point")))

(defun message-db--stream-at-point ()
  "Get stream name at point in active streams buffer."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^0-9 ][^ ]+\\)\\s-+[0-9]+")
      (match-string-no-properties 1))))

(defun message-db-inspect-stream-at-point ()
  "Show messages for the stream at point."
  (interactive)
  (if-let ((stream (message-db--stream-at-point)))
      (message-db-print-messages stream)
    (user-error "No stream at point")))

(defun message-db--display-message (id)
  "Display detailed view of message with ID."
  (let ((query (format "SELECT * FROM messages WHERE id = '%s'" id)))
    (with-current-buffer (get-buffer-create "*message-db-detail*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message-db-mode)
        (insert (message-db--execute-query query))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun message-db-refresh ()
  "Refresh the current message-db buffer."
  (interactive)
  (when-let ((query (get-text-property (point-min) 'message-db-query)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (message-db--execute-query query))
      (goto-char (point-min)))))

(transient-define-prefix message-db-query ()
  "Query builder for message-db."
  [:description "Message-DB Query Builder"
   ["Filters"
    ("-s" "Stream" "--stream=" :reader message-db--read-stream)
    ("-t" "Type" "--type=" :reader message-db--read-type)
    ("-c" "Category" "--category=")
    ("-m" "Metadata" "--metadata=")]
   ["Actions"
    ("q" "Build Query" message-db--build-query)
    ("e" "Execute Query" message-db--execute-current-query)
    ("x" "Clear Results" message-db--clear-results)]])

(defun message-db--read-stream (&optional _)
  "Read stream name with completion."
  (completing-read "Stream: "
                  (split-string
                   (message-db--execute-query
                    "SELECT DISTINCT stream_name FROM messages;")
                   "\n" t)))

(defun message-db--read-type (&optional _)
  "Read message type with completion."
  (completing-read "Type: "
                  (split-string
                   (message-db--execute-query
                    "SELECT DISTINCT type FROM messages;")
                   "\n" t)))

(defun message-db--build-query ()
  "Build SQL query from current transient arguments."
  (interactive)
  (let* ((args (transient-args 'message-db-query))
         (stream (transient-arg-value "--stream=" args))
         (type (transient-arg-value "--type=" args))
         (category (transient-arg-value "--category=" args))
         (where-clauses
          (delq nil
                (list
                 (when stream
                   (format "stream_name = '%s'" stream))
                 (when type
                   (format "type = '%s'" type))
                 (when category
                   (format "category(stream_name) = '%s'" category))))))
    (concat "SELECT * FROM messages"
            (when where-clauses
              (concat " WHERE " (string-join where-clauses " AND ")))
            " ORDER BY global_position DESC;")))

(defun message-db--execute-current-query ()
  "Execute the currently built query."
  (interactive)
  (let ((query (message-db--build-query)))
    (with-current-buffer (get-buffer-create "*message-db-results*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message-db-mode)
        (insert (message-db--execute-query query))
        (goto-char (point-min))
        (put-text-property (point-min) (1+ (point-min))
                          'message-db-query query)
        (display-buffer (current-buffer))))))

(defun message-db--clear-results ()
  "Clear the results buffer."
  (interactive)
  (when-let ((buf (get-buffer "*message-db-results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))
(defun message-db-active-streams ()
  "Display the 25 most recently active message streams."
  (interactive)
  (let* ((query "SELECT DISTINCT stream_name, MAX(global_position) as latest_position
                FROM messages
                GROUP BY stream_name
                ORDER BY max(global_position) DESC
                LIMIT 25")
         (results (message-db--execute-query query))
         (formatted (message-db--format-active-streams results)))
    (with-current-buffer (get-buffer-create "*message-db-active-streams*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message-db-mode)
        (insert formatted)
        (goto-char (point-min))
        (forward-line 2)  ; Skip header
        ;; Make stream names clickable
        (while (not (eobp))
          (when (message-db--stream-at-point)
            (put-text-property (line-beginning-position)
                             (line-end-position)
                             'mouse-face 'highlight))
          (forward-line 1))
        ;; Add key bindings
        (local-set-key (kbd "RET") #'message-db-inspect-stream-at-point)
        (local-set-key [mouse-1] #'message-db-inspect-stream-at-point)
        (display-buffer (current-buffer))))))

(provide 'message-db-mode)
