;;; binio.lisp --- Unit tests for the binio module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(in-package :binio.test)

(defmacro define-coder-suite (name width
			      &key
			      (end              nil)
			      (random-generator (intern (format nil "A-~A" (string name))
							:lift))
			      (test             '=))
  "Define a test suite for the en- and decode pair named NAME."
  (let ((suite-name  (intern (format nil "~A-ROOT" name) :binio.test))
	(encode-name (or (find-symbol (format nil "ENCODE-~A" name) :binio)
			 (error "Could not find encoder for ~S" name)))
	(decode-name (or (find-symbol (format nil "DECODE-~A" name) :binio)
			 (error "Could not find decoder for ~S" name))))
    `(progn
       (deftestsuite ,suite-name (binio-root)
	 ()
	 (:documentation
	  ,(format nil "Unit tests for en-/decoder ~A." name)))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Idempotence test for en-/decoder ~A." name))
	 idempotence

	 (ensure-random-cases 10000
	     ((offset a-non-negative-integer)
	      (value  ,random-generator))
	   (let ((buffer (make-octet-vector 512)))
	     ;; Encode value
	     (multiple-value-bind (encode-num-bytes buffer)
		 (,encode-name value buffer offset)
	       ,@(unless (null width)
			 `((ensure-same
			    encode-num-bytes ,width
			    :test (function ,test))))

	       ;; Decode value
	       (multiple-value-bind (decoded-value num-bytes)
		   (,decode-name buffer offset
				 ,@(when end `((+ offset encode-num-bytes))))
		 (ensure-same
		  decoded-value value
		  :test (function ,test))
		 ,@(unless (null width)
			   `((ensure-same
			      num-bytes ,width
			      :test #'=)))))))))))


;;; Bool
;;

(define-coder-suite bool 1
  :random-generator t-or-nil
  :test             #'eq)


;;; Unsigned and signed 16 bit integers
;;

(define-coder-suite uint16-le 2
  :random-generator a-non-negative-integer)
(define-coder-suite uint16-be 2
  :random-generator a-non-negative-integer)

(define-coder-suite sint16-le 2
  :random-generator an-integer)
(define-coder-suite sint16-be 2
  :random-generator an-integer)


;;; Unsigned and signed 32 bit integers
;;

(define-coder-suite uint32-le 4
  :random-generator a-non-negative-integer)
(define-coder-suite uint32-be 4
  :random-generator a-non-negative-integer)

(define-coder-suite sint32-le 4
  :random-generator an-integer)
(define-coder-suite sint32-be 4
  :random-generator an-integer)


;;; Unsigned and signed 64 bit integers
;;

(define-coder-suite uint64-le 8
  :random-generator a-non-negative-integer)
(define-coder-suite uint64-be 8
  :random-generator a-non-negative-integer)

(define-coder-suite sint64-le 8
  :random-generator an-integer)
(define-coder-suite sint64-be 8
  :random-generator an-integer)


;;; Double float
;;

(define-coder-suite double-le 8
  :random-generator a-double-float)
(define-coder-suite double-be 8
  :random-generator a-double-float)


;;; Single float
;;

(define-coder-suite float-le 4
  :random-generator a-single-float)
(define-coder-suite float-be 4
  :random-generator a-single-float)


;;; Unsigned and signed variable width integers
;;

(define-coder-suite uvarint nil
  :random-generator a-non-negative-integer)
(define-coder-suite svarint nil
  :random-generator an-integer)


;;; Strings
;;

(define-coder-suite utf8    nil
  :end              t
  :random-generator lift::a-string
  :test             'string=)
