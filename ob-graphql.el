(require 'graphql-mode)

(defun org-babel-execute:graphql (body params)
  (let* ((url (cdr (assq :url params)))
	 (op-name (cdr (assq :operation params)))
	 (variables (cdr (assq :variables params)))
	 (variables-val (when variables
			  (json-read-from-string (org-babel-ref-resolve variables))))
	 (headers (cdr (assq :headers params)))
	 (graphql-extra-headers
	  (when headers (org-babel-ref-resolve headers)))
         (response (if variables-val
		       (graphql-post-request url body op-name variables-val)
		     (graphql-post-request url body op-name))))
    (with-temp-buffer
      (insert (json-encode (request-response-data response)))
      (json-pretty-print-buffer)
      (buffer-substring (point-min) (point-max)))))

(provide 'ob-graphql)
