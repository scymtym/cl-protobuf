;;; conditions.lisp --- Conditions used by protocol buffer frontends.
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

(in-package :protocol-buffer.frontend)


;;; Parsing-related errors
;;

(define-condition proto-parse-error (error)
  ((line              :initarg  :line
		      :type     positive-integer
		      :accessor proto-parse-error-line
		      :documentation
		      "The line in which the parse error occurred.")
   (column            :initarg  :column
		      :type     non-negative-integer
		      :accessor proto-parse-error-column
		      :documentation
		      "The column in which the parse error occurred.")
   (offset            :initarg  :offset
		      :type     non-negative-integer
		      :accessor proto-parse-error-offset
		      :documentation
		      "The offset in characters at which the parse
error occurred.")
   (causing-condition :initarg  :causing-condition
		      :accessor proto-parse-error-causing-condition
		      :documentation
		      "The condition that represents the actual parse
error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Parse error at line ~D, column ~D (offset ~D): ~
~A.~@:>"
	     (proto-parse-error-line              condition)
	     (proto-parse-error-column            condition)
	     (proto-parse-error-offset            condition)
	     (proto-parse-error-causing-condition condition))))
  (:documentation
   "This error is signaled when a parse error occurs. The position of
the parse error as well as the causing condition are recorded in the
instance."))


;;; Import-related errors.
;;

(define-condition import-error (error)
  ((import :initarg  :import
	   :reader   import-error-import
	   :documentation
	   "The offending import."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Import ~S failed.~@:>"
	     (import-error-import condition))))
  (:documentation
   "This error and subclasses are signaled when an import statement is
incorrect or cannot be processed."))

(define-condition cannot-resolve-import (import-error)
  ((locations :initarg  :locations
	      :type     list
	      :reader   import-error-locations
	      :initform nil
	      :documentation
	      "Locations that have been consulted when trying to
resolve the import."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The resource designated by the import ~S could ~
not be resolved. ~:[No locations have been tried. Check ~
PROTOCOL-BUFFER.FRONTEND:*PROTO-LOAD-PATH* or the installed dependency ~
handler~;~:*These locations have been tried: ~{~S~^, ~}~].~@:>"
	     (import-error-import    condition)
	     (import-error-locations condition))))
  (:documentation
   "This error is signaled if the resource designated by an import
statement cannot be found or loaded."))

(defun cannot-resolve-import (import &optional locations)
  "Convenience function for signaling `cannot-resolve-import' errors."
  (error 'cannot-resolve-import
	 :import    import
	 :locations locations))

(define-condition ambiguous-import (import-error)
  ((candidates :initarg  :candidates
	       :type     list
	       :reader   import-error-candidates
	       :initform nil
	       :documentation
	       "The set of candidates causing the ambiguity."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Ambiguous import ~S. Candidates are ~{~S~^, ~
~}.~@:>"
	     (import-error-import     condition)
	     (import-error-candidates condition))))
  (:documentation
   "This error is signaled if there are multiple candidates for an
import and no resolution strategy has been specified."))

(defun ambiguous-import (import &optional candidates)
  "Convenience function for signaling `ambiguous-import' errors."
  (error 'ambiguous-import
	 :import     import
	 :candidates candidates))
