;; message-db.el --- Interface for message-db -*- lexical-binding: t -*-

;; Copyright (C) 2024 Your Name
;;
;; Author: Miles Starkenburg <milesstarkenburg@gmail.com>
;; Version: 0.2.0
;; Package-Requires: (transient)
;; Keywords: tools
;; URL: https://github.com/gringocl/message-db.el

;;; Commentary:
;; This package provides an Emacs interface for interacting with message-db
;; using transient mode. It replicates the functionality of the message-db
;; command line tools.

;;; Code:

(require 'transient)
(require 'message-db-mode)

(defgroup message-db nil
  "Interface for message-db."
  :group 'tools
  :prefix "message-db-")

(defcustom message-db-database "message_store"
  "Database name for message-db."
  :type 'string
  :group 'message-db)

(defcustom message-db-user "message_store"
  "Database user for message-db."
  :type 'string
  :group 'message-db)

(defcustom message-db-cycles 1000
  "Number of cycles for benchmarking."
  :type 'integer
  :group 'message-db)

;; Utility functions

(defun message-db--execute-query (query &optional args)
  "Execute QUERY against message-db with ARGS and return results."
  (with-temp-buffer
    (apply #'call-process "psql" nil t nil
           (append (list "-d" message-db-database
                        "-U" message-db-user
                        "-P" "pager=off"
                        "-v" "ON_ERROR_STOP=1"
                        "-c" query)
                  args))
    (buffer-string)))

(defun message-db--generate-uuid ()
  "Generate a lowercase UUID."
  (downcase (replace-regexp-in-string
             "\n" "" (shell-command-to-string "uuidgen"))))

(defun message-db--display-buffer (name content)
  "Display CONTENT in buffer with NAME."
  (with-current-buffer (get-buffer-create name)
    (erase-buffer)
    (insert content)
    (sql-mode)
    (display-buffer (current-buffer))))

;; Interactive commands

(defun message-db-clear-messages ()
  "Clear all messages from the database."
  (interactive)
  (when (yes-or-no-p "Really clear all messages? ")
    (message-db--execute-query "TRUNCATE message_store.messages RESTART IDENTITY;")
    (message "Messages cleared.")))

(defun message-db-print-messages (&optional stream-name)
  "Print messages, optionally filtered by STREAM-NAME."
  (interactive
   (list (transient-arg-value "--stream=" (transient-args 'message-db-transient))))
  (let ((query
         (if stream-name
             (format "SELECT * FROM messages WHERE stream_name = '%s' ORDER BY global_position ASC" stream-name)
           "SELECT * FROM messages ORDER BY global_position ASC")))
    (message-db--display-buffer "*message-db-messages*"
                               (message-db--execute-query query))))

(defun message-db-print-version ()
  "Print message store version."
  (interactive)
  (message "Message Store Version: %s"
           (string-trim (message-db--execute-query "SELECT message_store_version();"))))

(defun message-db-category-type-summary (&optional category)
  "Show category type summary, optionally filtered by CATEGORY."
  (interactive
   (list (transient-arg-value "--category=" (transient-args 'message-db-transient))))
  (let ((query
         (if category
             (format "SELECT * FROM category_type_summary WHERE category LIKE '%%%s%%';" category)
           "SELECT * FROM category_type_summary;")))
    (message-db--display-buffer "*message-db-category-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if category
                                          (format "SELECT COUNT(*) FROM messages WHERE category(stream_name) LIKE '%%%s%%';" category)
                                        "SELECT COUNT(*) FROM messages;"))))))
(defun message-db-active-streams ()
  "Display the 25 most recently active message streams.
Shows stream names and their latest message position."
  (interactive)
  (let ((query "SELECT DISTINCT stream_name, MAX(global_position) as latest_position
                FROM messages
                GROUP BY stream_name
                ORDER BY max(global_position) DESC
                LIMIT 25"))
    (message-db--display-buffer "*message-db-active-streams*"
                              (concat "Active Message Streams (Last 25)\n"
                                     "==============================\n\n"
                                     (message-db--execute-query query)))))

(defun message-db-stream-summary (&optional stream-name)
  "Show stream summary, optionally filtered by STREAM-NAME."
  (interactive
   (list (transient-arg-value "--stream=" (transient-args 'message-db-transient))))
  (let ((query
         (if stream-name
             (format "SELECT * FROM stream_summary WHERE stream_name LIKE '%%%s%%' ORDER BY message_count DESC;" stream-name)
           "SELECT * FROM stream_summary ORDER BY message_count DESC;")))
    (message-db--display-buffer "*message-db-stream-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if stream-name
                                          (format "SELECT COUNT(*) FROM messages WHERE stream_name LIKE '%%%s%%';" stream-name)
                                        "SELECT COUNT(*) FROM messages;"))))))

(defun message-db-stream-type-summary (&optional stream-name)
  "Show stream type summary, optionally filtered by STREAM-NAME."
  (interactive
   (list (transient-arg-value "--stream=" (transient-args 'message-db-transient))))
  (let ((query
         (if stream-name
             (format "SELECT * FROM stream_type_summary WHERE stream_name LIKE '%%%s%%' ORDER BY stream_name, message_count DESC;" stream-name)
           "SELECT * FROM stream_type_summary ORDER BY stream_name, message_count DESC, type;")))
    (message-db--display-buffer "*message-db-stream-type-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if stream-name
                                          (format "SELECT COUNT(*) FROM messages WHERE stream_name LIKE '%%%s%%';" stream-name)
                                        "SELECT COUNT(*) FROM messages;"))))))

(defun message-db-type-category-summary (&optional type)
  "Show type category summary, optionally filtered by TYPE."
  (interactive
   (list (transient-arg-value "--type=" (transient-args 'message-db-transient))))
  (let ((query
         (if type
             (format "SELECT * FROM type_category_summary WHERE type LIKE '%%%s%%';" type)
           "SELECT * FROM type_category_summary;")))
    (message-db--display-buffer "*message-db-type-category-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if type
                                          (format "SELECT COUNT(*) FROM messages WHERE type LIKE '%%%s%%';" type)
                                        "SELECT COUNT(*) FROM messages;"))))))

(defun message-db-type-stream-summary (&optional type)
  "Show type stream summary, optionally filtered by TYPE."
  (interactive
   (list (transient-arg-value "--type=" (transient-args 'message-db-transient))))
  (let ((query
         (if type
             (format "SELECT * FROM type_stream_summary WHERE type LIKE '%%%s%%' ORDER BY type, message_count DESC, stream_name;" type)
           "SELECT * FROM type_stream_summary ORDER BY type, message_count DESC, stream_name;")))
    (message-db--display-buffer "*message-db-type-stream-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if type
                                          (format "SELECT COUNT(*) FROM messages WHERE type LIKE '%%%s%%';" type)
                                        "SELECT COUNT(*) FROM messages;"))))))

(defun message-db-type-summary (&optional type)
  "Show type summary, optionally filtered by TYPE."
  (interactive
   (list (transient-arg-value "--type=" (transient-args 'message-db-transient))))
  (let ((query
         (if type
             (format "SELECT * FROM type_summary WHERE type LIKE '%%%s%%' ORDER BY message_count DESC;" type)
           "SELECT * FROM type_summary ORDER BY message_count DESC;")))
    (message-db--display-buffer "*message-db-type-summary*"
                               (concat (message-db--execute-query query)
                                     "\nTotal count: "
                                     (message-db--execute-query
                                      (if type
                                          (format "SELECT COUNT(*) FROM messages WHERE type LIKE '%%%s%%';" type)
                                        "SELECT COUNT(*) FROM messages;"))))))

(defun message-db-write-test-message (&optional stream-name type metadata instances)
  "Write test messages.
Optional STREAM-NAME, TYPE, METADATA, and number of INSTANCES."
  (interactive
   (list (transient-arg-value "--stream=" (transient-args 'message-db-transient))
         (transient-arg-value "--type=" (transient-args 'message-db-transient))
         (transient-arg-value "--metadata=" (transient-args 'message-db-transient))
         (string-to-number (or (transient-arg-value "--instances=" (transient-args 'message-db-transient)) "1"))))
  (let* ((stream (or stream-name (concat "testStream-" (message-db--generate-uuid))))
         (msg-type (or type "SomeType"))
         (meta (or metadata "'{\"metaAttribute\": \"some meta value\"}'"))
         (count (or instances 1)))
    (dotimes (i count)
      (let ((uuid (message-db--generate-uuid)))
        (message "Writing message %d/%d (ID: %s)" (1+ i) count uuid)
        (message-db--execute-query
         (format "SELECT write_message('%s'::varchar, '%s'::varchar, '%s'::varchar, '{\"attribute\": \"some value\"}'::jsonb, %s::jsonb);"
                 uuid stream msg-type meta))))
    (message-db-print-messages stream)))

(defun message-db-benchmark (&optional stream-name cycles)
  "Run benchmark with optional STREAM-NAME and CYCLES."
  (interactive
   (list (transient-arg-value "--stream=" (transient-args 'message-db-transient))
         (string-to-number (or (transient-arg-value "--cycles=" (transient-args 'message-db-transient))
                              (number-to-string message-db-cycles)))))
  (let ((stream (or stream-name (concat "testStream-" (message-db--generate-uuid))))
        (cycle-count (or cycles message-db-cycles)))
    (message "Installing benchmark scripts...")
    (message-db--execute-query (format "\\i %s" "benchmark_write.sql"))
    (message-db--execute-query (format "\\i %s" "benchmark_get.sql"))

    (message "\nBenchmarking write...")
    (message-db--display-buffer "*message-db-benchmark-write*"
                               (message-db--execute-query
                                (format "EXPLAIN ANALYZE SELECT benchmark_write('%s'::varchar, %d::int);"
                                        stream cycle-count)))

    (message "\nBenchmarking get...")
    (message-db--display-buffer "*message-db-benchmark-get*"
                               (message-db--execute-query
                                (format "EXPLAIN ANALYZE SELECT benchmark_get('%s'::varchar, %d::int);"
                                        stream cycle-count)))))

;; Transient interface

(transient-define-prefix message-db-transient ()
  "Command dispatcher for message-db operations."
  :value '("--instances=1" "--cycles=1000")
  ["Parameters"
   ("-s" "Stream name" "--stream="
    :reader (lambda (_prompt _init)
              (completing-read "Stream name: " (split-string
                                                (message-db--execute-query "SELECT DISTINCT stream_name FROM messages;")))))
   ("-t" "Type" "--type="
    :reader (lambda (_prompt _init)
              (completing-read "Type: " (split-string
                                         (message-db--execute-query "SELECT DISTINCT type FROM messages;")))))
   ("-c" "Category" "--category=")
   ("-m" "Metadata" "--metadata=")
   ("-i" "Instances" "--instances=")
   ("-n" "Cycles" "--cycles=")]
  ["Views"
   ("a" "Active streams" message-db-active-streams)
   ("m" "Print messages" message-db-print-messages)
   ("v" "Print version" message-db-print-version)
   ("c" "Category type summary" message-db-category-type-summary)
   ("s" "Stream summary" message-db-stream-summary)
   ("y" "Stream type summary" message-db-stream-type-summary)
   ("g" "Type category summary" message-db-type-category-summary)
   ("r" "Type stream summary" message-db-type-stream-summary)
   ("u" "Type summary" message-db-type-summary)]
  ["Actions"
   ("w" "Write test message" message-db-write-test-message)
   ("b" "Run benchmark" message-db-benchmark)
   ("x" "Clear messages" message-db-clear-messages)])

;;;###autoload
(defun message-db ()
  "Open message-db interface."
  (interactive)
  (message-db-transient))

(provide 'message-db)

;;; message-db.el ends here
