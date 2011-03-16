;;; binio.lisp --- Unit tests for the binio module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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
