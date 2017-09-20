;;; graphql-mode.el --- Major mode for editing GraphQL schemas        -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  David Vazquez Pua

;; Author: David Vazquez Pua <davazp@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.3") (request "20170131.1747"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a major mode to edit GraphQL schemas and
;; query. The basic functionality includes:
;;
;;    - Syntax highlight
;;    - Automatic indentation
;;
;; Additionally, it is able to
;;    - Sending GraphQL queries to an end-point URL
;;
;; Files with the .graphql extension are automatically opened with
;; this mode.


;;; Code:

(require 'newcomment)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'request)

;;; User Customizations:

(defgroup graphql nil
  "Major mode for editing GraphQL schemas and queries."
  :tag "GraphQL"
  :group 'languages)

(defcustom graphql-indent-level 2
  "Number of spaces for each indentation step in `graphql-mode'."
  :tag "GraphQL"
  :type 'integer
  :safe 'integerp
  :group 'graphql)

(defcustom graphql-url nil
  "URL address of the graphql server endpoint."
  :tag "GraphQL"
  :type 'string
  :group 'graphql)

(defcustom graphql-variables nil
  "file name containing graphql variables."
  :tag "GraphQL"
  :type 'file
  :group 'graphql)

(defun graphql--query (query operation variables)
  "Send QUERY to the server at `graphql-url' and return the
response from the server."
  (let* ((url-request-method "POST")
         (url (format "%s?query=%s" graphql-url (url-encode-url query))))
    (if operation
        (setq url (concat url "&operationName=" operation)))
    (if variables
        (setq url (concat url "&variables=" (url-encode-url variables))))
    (with-temp-buffer (graphql-post-request graphql-url url query operation variables))))

(defun graphql-post-request (host_path url query operation variables)
  "graphql make post request to graphql endpoint url with body of query, operationName, and variables"
  (let* ((body (list (cons "query" query)
                     (cons "operationName" operation)
                     (cons "variables" variables)))
         (response nil))
    (setq response (request
                    url
                    :type "POST"
                    :data (json-encode body)
                    :headers '(("Content-Type" . "application/json"))
                    :parser 'json-read
                    :sync t
                    :complete (lambda (&rest _)
                                (message "%s" (if (string-equal "" operation)
                                                  host_path
                                                (format "%s?operationName=%s" host_path operation))))))
    (json-encode (request-response-data response))))

(defun graphql-beginning-of-query ()
  "Move the point to the beginning of the current query."
  (interactive)
  (while (and (> (point) (point-min))
              (or (> (current-indentation) 0)
                  (> (car (syntax-ppss)) 0)))
    (forward-line -1)))

(defun graphql-end-of-query ()
  "Move the point to the end of the current query."
  (interactive)
  (while (and (< (point) (point-max))
              (or (> (current-indentation) 0)
                  (> (car (syntax-ppss)) 0)))
    (forward-line 1)))

(defun graphql-current-query ()
  "find out the current query/mutation/subscription"
  (let ((start
         (save-excursion
           (graphql-beginning-of-query)
           (point)))
        (end
         (save-excursion
           (graphql-end-of-query)
           (point))))
    (buffer-substring-no-properties start end)))

(defun graphql-current-operation ()
  "get the name of the query operation"
  (let* ((query
         (save-excursion
           (replace-regexp-in-string "^[ \t\n]*" "" (graphql-current-query))))
         (tokens
          (split-string query "[ \f\t\n\r\v]+"))
         (first (nth 0 tokens)))

    (if (or (string-equal first "{") (string-equal first ""))
        (read-string "GraphQL operation: ")
      (replace-regexp-in-string "[({].*" "" (nth 1 tokens)))))

(defun graphql-current-variables (filename)
  "get the content of graphql variables from a file"
  (if (and filename
           (not (string-equal filename ""))
           (not (file-directory-p filename))
           (file-exists-p filename))
      (condition-case nil
          (progn (display-buffer (find-file-noselect filename))
                 (json-encode (json-read-file filename)))
        (error nil))
    nil))

(defun graphql-send-query ()
  (interactive)
  (let* ((url (or graphql-url (read-string "GraphQL URL: " )))
         (var (or graphql-variables (read-file-name "GraphQL Variables: "))))
    (let ((graphql-url url)
          (graphql-variables var))

      (let* ((query (buffer-substring-no-properties (point-min) (point-max)))
             (operation (graphql-current-operation))
             (variables (graphql-current-variables var))
             (response (graphql--query query operation variables)))
        (with-current-buffer-window
         "*GraphQL*" 'display-buffer-pop-up-window nil
         (erase-buffer)
         (when (fboundp 'json-mode)
           (json-mode))
         (insert response)
         (json-pretty-print-buffer))))
    ;; If the query was successful, then save the value of graphql-url
    ;; in the current buffer (instead of the introduced local
    ;; binding).
    (setq graphql-url url)
    (setq graphql-variables var)
    nil))

(defvar graphql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'graphql-send-query)
    map))

(defvar graphql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\$ "'" st)
    st))


(defun graphql-indent-line ()
  (let ((position (point))
        (indent-pos))
    (save-excursion
      (let ((level (car (syntax-ppss (point-at-bol)))))

        ;; Handle closing pairs
        (when (looking-at "\\s-*\\s)")
          (setq level (1- level)))

        (indent-line-to (* graphql-indent-level level))
        (setq indent-pos (point))))

    (when (< position indent-pos)
      (goto-char indent-pos))))


(defvar graphql-definition-regex
  (concat "\\(" (regexp-opt '("type" "input" "interface" "fragment" "query" "mutation" "subscription" "enum")) "\\)"
          "[[:space:]]+\\(\\_<.+?\\_>\\)"))

(defvar graphql-builtin-types
  '("Int" "Float" "String" "Boolean" "ID"))

(defvar graphql-constants
  '("true" "false" "null"))


;;; Check if the point is in an argument list.
(defun graphql--in-arguments-p ()
  (let ((opening (cl-second (syntax-ppss))))
    (eql (char-after opening) ?\()))


(defun graphql--field-parameter-matcher (limit)
  (catch 'end
    (while t
      (cond
       ;; If we are inside an argument list, try to match the first
       ;; argument that we find or exit the argument list otherwise, so
       ;; the search can continue.
       ((graphql--in-arguments-p)
        (let* ((end (save-excursion (up-list) (point)))
               (match (search-forward-regexp "\\(\\_<.+?\\_>\\):" end t)))
          (if match
              ;; unless we are inside a string or comment
              (let ((state (syntax-ppss)))
                (when (not (or (nth 3 state)
                               (nth 4 state)))
                  (throw 'end t)))
            (up-list))))
       (t
        ;; If we are not inside an argument list, jump after the next
        ;; opening parenthesis, and we will try again there.
        (skip-syntax-forward "^(" limit)
        (forward-char))))))


(defvar graphql-font-lock-keywords
  `(
    ;; Type definition
    ("\\(type\\)[[:space:]]+\\(\\_<.+?\\_>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     ("[[:space:]]+\\(implements\\)\\(?:[[:space:]]+\\(\\_<.+?\\_>\\)\\)?"
      nil nil
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))

    ;; Definitions
    (,graphql-definition-regex
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Constants
    (,(regexp-opt graphql-constants) . font-lock-constant-face)

    ;; Variables
    ("\\$\\_<.+?\\_>" . font-lock-variable-name-face)

    ;; Types
    (":[[:space:]]*\\[?\\(\\_<.+?\\_>\\)\\]?"
     (1 font-lock-type-face))

    ;; Directives
    ("@\\_<.+?\\_>" . font-lock-keyword-face)

    ;; Field parameters
    (graphql--field-parameter-matcher
     (1 font-lock-variable-name-face))))


;;;###autoload
(define-derived-mode graphql-mode prog-mode "GraphQL"
  "A major mode to edit GraphQL schemas."
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local indent-line-function 'graphql-indent-line)
  (setq font-lock-defaults
        `(graphql-font-lock-keywords
          nil
          nil
          nil))
  (setq imenu-generic-expression
        `((nil ,graphql-definition-regex 2))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))


(provide 'graphql-mode)
;;; graphql-mode.el ends here
