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


;;; User Customizations:

(defgroup graphql nil
  "Major mode for editing GraphQL schemas and queries."
  :tag "GraphQL"
  :group 'languages)

(defcustom graphql-indent-level 2
  "Number of spaces for each indentation step in `graphql-mode'."
  :type 'integer
  :safe 'integerp)


(defvar graphql-url
  nil)
(make-variable-buffer-local 'graphql-url)

(defun graphql--query (query)
  "Send QUERY to the server at `graphql-url' and return the
response from the server."
  (let ((url-request-method "POST")
        (url (format "%s/?query=%s" graphql-url (url-encode-url query))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (search-forward "\n\n")
      (buffer-substring (point) (point-max)))))

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
  (let ((start
         (save-excursion
           (graphql-beginning-of-query)
           (point)))
        (end
         (save-excursion
           (graphql-end-of-query)
           (point))))
    (buffer-substring-no-properties start end)))


(defun graphql-send-query ()
  (interactive)
  (unless graphql-url
    (setq graphql-url (read-string "GraphQL URL: " )))
  (let* ((query (graphql-current-query))
         (response (graphql--query query)))
    (with-current-buffer-window
     "*GraphQL*" 'display-buffer-pop-up-window nil
     (erase-buffer)
     (when (fboundp 'json-mode)
       (json-mode))
     (insert response)
     (json-pretty-print-buffer))))



(defvar graphql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'graphql-send-query)
    map))

(defvar graphql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
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
  (concat "\\(" (regexp-opt '("type" "input" "interface" "fragment" "query" "enum")) "\\)"
          "[[:space:]]+\\(\\w+\\)"))

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
               (match (search-forward-regexp "\\(\\w+\\):" end t)))
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
    ("\\(type\\)[[:space:]]+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     ("[[:space:]]+\\(implements\\)\\(?:[[:space:]]+\\(\\w+\\)\\)?"
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
    ("\\$\\w+" . font-lock-variable-name-face)

    ;; Types
    (":[[:space:]]*\\[?\\(\\w+\\)\\]?"
     (1 font-lock-type-face))

    ;; Directives
    ("@\\w+" . font-lock-keyword-face)

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
