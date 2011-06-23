;;; util.lisp --- Utility Functions used in the Protocol Buffer Library.
;;
;; Copyright (C) 2009, Georgia Tech Research Corporation
;; Copyright (C) 2010, 2011 Jan Moringen
;; All rights reserved.
;;
;; Author: Neil T. Dantam
;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
  "Try to find a symbol corresponding to NAME. If name starts with
`.', it is considered a \"fully-qualified\" name, that is specifying
the symbol's package along with the symbol. Otherwise, a package has
to be supplied via the PACKAGE parameter. If name contains multiple
`.'s, all possible package-name splits are tried until a matching
symbol is found."
  (bind ((fully-qualified? (eq (aref name 0) #\.))
	 (tried)
	 ((:flet find-symbol* (name package))
	  (push (list name package) tried)
	  (let ((symbol (find-symbol name package)))
	    (when (and symbol
		       (or (enum-type-p symbol)
			   (find-class symbol nil)))
	      symbol)))
	 ((:labels possible-splits (start))
	  (let ((index (position #\. name :start start)))
	    (when index
	      (cons index (possible-splits (1+ index))))))
	 ((:flet split (index))
	  (list (subseq name 1 index) (subseq name (1+ index))))
	 ((:flet find-name-in-package (package name))
	  (let ((package1 (find-package
			   (->lisp-name package :allow-dots? t))))
	    (when package1
	      (find-symbol* (->lisp-name name) package1))))
	 ((:flet do-it ())
	  (if fully-qualified?
	      (some (curry #'apply #'find-name-in-package)
		    (map 'list #'split (possible-splits 1)))
	      (find-symbol* (->lisp-name name) package))))
    (unless (or package fully-qualified?)
      (error 'name-resolution-failed
	     :name             name
	     :package          package
	     :format-control "~@<Can only handle fully ~
qualified (e.g. starting with `.') names unless a context package is ~
specified. Name ~S is not fully qualified.~@:>"
	     :format-arguments `(,name)))

    (or (do-it)
	(error 'name-resolution-failed
	       :name       name
	       :package    package
	       :candidates tried))))


;;;
;;

(defun missing-required-initarg (class initarg)
  "This function can be used to signal an error when INITARG required
by CLASS has not been provided."
  (error 'missing-required-initarg
	 :class class
	 :initarg initarg))
