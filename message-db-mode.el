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

(defun message-db-inspect-at-point ()
  "Inspect the message at point in detail."
  (interactive)
  (if-let* ((bounds (bounds-of-thing-at-point 'line))
            (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
            (id (when (string-match "id: \\([^ ]+\\)" line)
                  (match-string 1 line))))
      (message-db--display-message id)
    (user-error "No message at point")))

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

(provide 'message-db-mode)
