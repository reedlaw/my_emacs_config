;;; http-post-simple.el --- HTTP POST requests using the url library

;; Copyright (C) 2007  Tom Schutzer-Weissmann

;; Author: Tom Schutzer-Weissmann <trmsw at yahoo.co.uk>
;; Maintainer: Tom Schutzer-Weissmann
;; Version: 1.0.1
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?HttpPost

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as1
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; Provides ways to use the url library to perform HTTP POST requests.
;; See the documentation to `http-post-simple' and `http-post-multipart' 
;; for more information.

;;; Change Log:

;; 1.0.1
;;    - Changed documentation

;;; Code:
(require 'url)


(defun http-post-simple (url fields &optional coding-system)
  "Send FIELDS to URL as an HTTP POST request, returning the response
and response headers.
FIELDS is an alist, eg ((field-name . \"value\")); all values
need to be strings, and they are encoded using CODING-SYSTEM,
which defaults to 'utf-8"
  (http-post-simple-internal
   url
   (http-post-encode-fields fields coding-system)
   `(("Content-Type" . "application/x-www-form-urlencoded"))))


(defun http-post-simple-multipart (url fields files &optional coding-system)
  "Send FIELDS and FILES to URL as a multipart HTTP POST, returning the
response and response headers.
FIELDS is an alist, as for `http-post-simple', FILES is an a list of
(fieldname \"filename\" \"file MIME type\" \"file data\")*"
(let ((boundary (http-post-multipart-boundary)))
  (http-post-simple-internal
   url
   (http-post-encode-multipart-data fields files coding-system)
   `(("Content-Type" . ,(format "multipart/form-data; boundary=%S" boundary))))))


;; based on `http-url-encode' from the from http-get package
;; http://savannah.nongnu.org/projects/http-emacs 
(defun http-post-encode-string (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		       (format "%%%02x" c)))
		 (encode-coding-string str content-type))))


(defun http-post-encode-fields (fields &optional coding-system)
  "Encode FIELDS using `http-post-encode-string', where
FIELDS is an alist of (
	(field-name-as-symbol . \"field value as string\") |
	(field-name \"value1\" \"value2\" ...)
	)*

CODING-SYSTEM defaults to 'utf-8"
  (let ((coding-system (or coding-system 'utf-8)))
    (mapconcat #'identity
	       (mapcar '(lambda (field)
			 (concat (symbol-name (car field))
			  "="
			  (http-post-encode-string (cdr field) coding-system)))
		       (mapcan '(lambda (field)
				 (if (atom (cdr field)) (list field)
				     ;; unpack the list
				     (mapcar '(lambda (value)
					       `(,(car field) . ,value))
					     (cdr field))))
			       fields))
	       "&")))


(defun http-post-simple-internal (url data extra-headers)
  (let ((url-request-method        "POST")
	(url-request-data          data)
	(url-request-extra-headers extra-headers))
    (let (header
	  data
	  status)
      (setq url-http-attempt-keepalives nil)
      (with-current-buffer
	  (url-retrieve-synchronously url)
	;; status
	(setq status url-http-response-status)
	;; return the header and the data separately
	(goto-char (point-min))
	(if (search-forward-regexp "^$" nil t)
	    (setq header (buffer-substring (point-min) (point))
		  data   (buffer-substring (1+ (point)) (point-max)))
	    ;; unexpected situation, return the whole buffer
	    (setq data (buffer-string))))
      (values (string-make-multibyte data) header status))))



(defun http-post-multipart-boundary ()
  "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")


(defun http-post-bound-field (&rest parts)
  (let ((boundary (format "--%s" (http-post-multipart-boundary))))
    (http-post-join-strings  boundary parts)))



(defun http-post-encode-multipart-data (fields files coding-system)
  "Return FIELDS and FILES encoded for use as the data for a multipart HTTP POST request"
  (http-post-join-strings
   (mapcar '(lambda (field)
	     (http-post-bound-field
	      (format "Content-Disposition: form-data; name=%S" (symbol-name (car field)))
	      ""
	      (cdr field)))
	   fields)
   (mapcan '(lambda (file)
	     (destructuring-bind (fieldname filename mime-type data) file
	       (http-post-bound-field
		(format "Content-Disposition: form-data; name=%S; filename=%S" fieldname filename)
		(format "Content-type: %s" mime-type)
		""
		data)))
	   files)
   (format "--%s--" (http-post-multipart-boundary))))



(defun http-post-join-strings (&rest bits)
  (let ((sep "\r\n"))
    (mapconcat (lambda (bit)
		 (if (listp bit)
		     (apply 'http-post-join-strings bit)
		     bit))
	       bits sep)))


(provide 'http-post-simple)
;;; http-post-simple.el ends here