;;; util.lisp --- Utility Functions used in the Protocol Buffer Library.
;;
;; Copyright 2009, Georgia Tech Research Corporation
;; Copyright (c) 2010, 2011 Jan Moringen
;; All rights reserved.
;;
;; Author: Neil T. Dantam
;;         Jan Moringen
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the name of the copyright holder(s) nor the names of its
;;   contributors may be used to endorse or promote products derived
;;   from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :protocol-buffer)

(defun symcat (package-or-sym &rest symbols)
  (intern (reduce (lambda (a b)
                    (concatenate 'string (string a) "-" (string b)))
                  (if (symbolp package-or-sym)
                      (cons package-or-sym symbols)
                      symbols))
          (cond
            ((packagep package-or-sym) package-or-sym)
            ((symbolp package-or-sym) (symbol-package package-or-sym))
            (t (error "Can't find package for ~A" package-or-sym)))))

(defun ->lisp-name (name
		    &key
		    allow-dots?)
  (with-output-to-string (stream)
    (iter (for char     in-vector name)
	  (for previous previous  char)
	  (cond
	    ((or (eq char #\_)
		 (and (not allow-dots?)
		      (eq char #\.)))
	     (write-char #\- stream))
	    ((and previous
		  (lower-case-p previous)
		  (upper-case-p char))
	     (write-char #\- stream)
	     (write-char char stream))
	    (t
	     (write-char (char-upcase char) stream))))))

(defun symbol->keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun proto-type-name->lisp-type-symbol (name
					  &key
					  package)
  "DOC"
  (bind ((fully-qualified? (eq (aref name 0) #\.))
	 ((:function possible-splits (start))
	  (let ((index (position #\. name :start start)))
	    (when index
	      (cons index (possible-splits (1+ index))))))
	 ((:flet split (index))
	  (list (subseq name 1 index) (subseq name (1+ index))))
	 ((:flet find-name-in-package (package name))
	  (format t "Trying ~S in ~S"
		  (->lisp-name name)
		  (->lisp-name package :allow-dots? t))
	  (let ((package1 (find-package
			   (->lisp-name package :allow-dots? t))))
	    (unless package1
	      (format t " -> no~%"))
	    (when package1
	      (let ((sym (find-symbol (->lisp-name name) package1)))
		(if sym
		    (format t " -> ~S~%" sym)
		    (format t " -> no~%"))
		sym))))
	 ((:flet do-it ())
	  (if fully-qualified?
	      (some (curry #'apply #'find-name-in-package)
		    (map 'list #'split (possible-splits 1)))
	      (find-symbol (->lisp-name name) package))))
    (unless (or package fully-qualified?)
      (error "~@<Can only handle fully qualified (e.g. start starting ~
with `.') names unless a context package is specified. Name ~S is ~
not fully qualified.~@:>"
	     name))

    (or (do-it)
	(error "~@<Could not find a symbol named ~S in package ~S.~@:>"
	       name package))))
