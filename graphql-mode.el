;;; graphql-mode.el --- Major mode for editing GraphQL schemas        -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  David Vazquez Pua

;; Author: David Vazquez Pua <davazp@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

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
;; query.  The basic functionality includes:
;;
;;    - Syntax highlight
;;    - Automatic indentation
;;
;; Additionally, it is able to
;;    - Sending GraphQL queries to an end-point URL
;;
;; Files with the .graphql and .gql extensions are automatically
;; opened with this mode.


;;; Code:

(require 'newcomment)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'let-alist)

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

(defcustom graphql-variables-file nil
  "File name containing graphql variables."
  :tag "GraphQL"
  :type 'file
  :group 'graphql)

(defcustom graphql-extra-headers '()
  "Headers to send to the graphql endpoint."
  :tag "GraphQL"
  :type 'list
  :group 'graphql)

(defun graphql-locate-config (dir)
  "Locate a graphql config starting in DIR."
  (if-let ((config-dir (locate-dominating-file dir ".graphqlconfig")))
      (concat config-dir ".graphqlconfig")
    (error "Could not find a .graphqlconfig file")))

(defun graphql--completing-read-endpoint (endpoints)
  "Select an endpoint configuration from a list of ENDPOINTS."
  (completing-read "Select Graphql Endpoint:" (mapcar 'car endpoints)))

(defun graphql-open-config ()
  "Open the graphql config."
  (interactive)
  (find-file (graphql-locate-config ".")))

(defun graphql-select-endpoint ()
  "Set parameters based off of the endpoints listed in a .graphqlconfig file."
  (interactive)
  (let ((config (json-read-file (graphql-locate-config "."))))
    (let-alist config
      (if-let ((endpoints .extensions.endpoints)
               (endpoint (cdr (assq (intern (graphql--completing-read-endpoint endpoints)) endpoints))))
          (let-alist endpoint
            (setq graphql-url .url
                  graphql-extra-headers .headers))
          (error "No endpoint configurations in .graphqlconfig")))))

(defun graphql-encode-json (query &optional operation variables)
  "Put together a json like object with QUERY, OPERATION, and VARIABLES."
  (let* ((body '()))
    (push (cons 'query query) body)
    (when (and operation (not (string= operation "")))
      (push (cons 'operationName operation) body))
    (when variables
      (push (cons 'variables variables) body))
    (json-encode body)))

(defun graphql--query (query &optional operation variables)
  "Send QUERY to the server and return the response.

The query is sent as a HTTP POST request to the URL at
`graphql-url'.  The query can be any GraphQL definition (query,
mutation or subscription).  OPERATION is a name for the
operation.  VARIABLES is the JSON string that specifies the values
of the variables used in the query."
  ;; Note that we need to get the value of graphql-url in the current
  ;; before before we switch to the temporary one.
  (let ((url graphql-url))
    (graphql-post-request url query operation variables)))

(declare-function request "request")
(declare-function request-response-data "request")
(declare-function request-response--raw-header "request")

(defun graphql-post-request (url query &optional operation variables)
  "Make post request to graphql server with url and body.

URL hostname, path, search parameters, such as operationName and variables
QUERY query definition(s) of query, mutation, and/or subscription
OPERATION name of the operation if multiple definition is given in QUERY
VARIABLES list of variables for query operation"
  (or (require 'request nil t)
      (error "graphql-post-request needs the request package.  \
Please install it and try again."))
  (let* ((body (graphql-encode-json query operation variables))
         (headers (append '(("Content-Type" . "application/json")) graphql-extra-headers)))
    (request url
             :type "POST"
             :data body
             :headers headers
             :parser 'json-read
             :sync t
             :complete (lambda (&rest _)
                         (message "%s" (if (string-equal "" operation)
                                           url
                                         (format "%s?operationName=%s"
                                                 url operation)))))))

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
  "Return the current query/mutation/subscription definition."
  (let ((start
         (save-excursion
           (graphql-beginning-of-query)
           (point)))
        (end
         (save-excursion
           (graphql-end-of-query)
           (point))))
    (if (not (equal start end))
    (buffer-substring-no-properties start end)
      (save-excursion
    (let ((saved-point (point))
          (line (thing-at-point 'line t)))
      (when (string-match-p (regexp-quote "}") line)
        (search-backward "}" (beginning-of-line)))
      (when (string-match-p (regexp-quote "{") line)
        (search-forward "{" (end-of-line)))
      (if (= (point) saved-point)
          nil
        (graphql-current-query)))))))

(defun graphql-current-operation ()
  "Return the name of the current graphql query."
  (let* ((query
         (save-excursion
           (replace-regexp-in-string "^[ \t\n]*" ""
                     (or (graphql-current-query) ""))))
         (tokens
          (split-string query "[ \f\t\n\r\v]+"))
         (first (nth 0 tokens)))

    (if (or (string-equal first "{") (string-equal first ""))
        nil
      (replace-regexp-in-string "[({].*" "" (nth 1 tokens)))))

(defun graphql-current-variables (filename)
  "Return the current variables contained in FILENAME."
  (if (and filename
           (not (string-equal filename ""))
           (not (file-directory-p filename))
           (file-exists-p filename))
      (condition-case nil
          (progn (get-buffer-create (find-file-noselect filename))
                 (json-read-file filename))
        (error nil))
    nil))

(define-minor-mode graphql-query-response-mode
  "Allows GraphQL query response buffer to be closed with (q)"
  :lighter " GraphQL Response"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
            map))

(defun graphql-send-query ()
  "Send the current GraphQL query/mutation/subscription to server."
  (interactive)
  (let* ((url (or graphql-url (read-string "GraphQL URL: " )))
         (var (or graphql-variables-file (read-file-name "GraphQL Variables: "))))
    (let ((graphql-url url)
          (graphql-variables-file var))

      (let* ((query (buffer-substring-no-properties (point-min) (point-max)))
             (operation (graphql-current-operation))
             (variables (graphql-current-variables var))
             (response (graphql--query query operation variables)))
        (with-current-buffer-window
         "*GraphQL*" 'display-buffer-pop-up-window nil
         (erase-buffer)
         (when (fboundp 'json-mode)
           (json-mode))
         (insert (json-encode (request-response-data response)))
         (json-pretty-print-buffer)
         (goto-char (point-max))
         (insert "\n\n"
                 (propertize (request-response--raw-header response)
                             'face 'font-lock-comment-face
                             'font-lock-face 'font-lock-comment-face))
         (graphql-query-response-mode))))
    ;; If the query was successful, then save the value of graphql-url
    ;; in the current buffer (instead of the introduced local
    ;; binding).
    (setq graphql-url url)
    (setq graphql-variables-file var)
    nil))

(defvar graphql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'graphql-send-query)
    (define-key map (kbd "C-c C-l") 'graphql-select-endpoint)
    map)
  "Key binding for GraphQL mode.")

(defvar graphql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\$ "'" st)
    st)
  "Syntax table for GraphQL mode.")


(defun graphql-indent-line ()
  "Indent GraphQL schema language."
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

(defvar graphql-keywords
  '("type" "input" "interface" "fragment"
    "query" "enum" "mutation" "subscription"
    "Int" "Float" "String" "Boolean" "ID"
    "true" "false" "null" "extend"
    "scalar" "union"))

(defun graphql-completion-at-point ()
  "Return the list of candidates for completion.
This is the function to be used for the hook `completion-at-point-functions'."
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end graphql-keywords . nil)))


(defvar graphql-definition-regex
  (concat "\\(" (regexp-opt '("type" "input" "interface" "fragment" "query"
                              "mutation" "subscription" "enum" "extend"
                              "scalar" "union")) "\\)"
                              "[[:space:]]+\\(\\_<.+?\\_>\\)")
  "Keyword Regular Expressions.")

(defvar graphql-builtin-types
  '("Int" "Float" "String" "Boolean" "ID")
  "Built-in GraphQL Types.")

(defvar graphql-constants
  '("true" "false" "null")
  "Constant GraphQL Types.")


;;; Check if the point is in an argument list.
(defun graphql--in-arguments-p ()
  "Return t if the point is in the arguments list of a GraphQL query."
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
        (and (eobp) (throw 'end nil))
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
     (1 font-lock-variable-name-face)))
  "Font Lock keywords.")


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
  (setq imenu-generic-expression `((nil ,graphql-definition-regex 2)))
  (add-hook 'completion-at-point-functions 'graphql-completion-at-point nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode))


(provide 'graphql-mode)
;;; graphql-mode.el ends here
