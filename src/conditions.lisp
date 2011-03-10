;;; conditions.lisp --- Conditions used in the cl-protobuf system.
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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

(in-package :protocol-buffer)

(define-condition missing-required-initarg (error)
  ((class   :initarg  :class
	    :type     symbol
	    :accessor missing-required-initarg-class
	    :documentation
	    "The class which requires the missing initarg.")
   (initarg :initarg  :initarg
	    :type     symbol
	    :accessor missing-required-initarg-initarg
	    :documentation
	    "The missing initarg."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The initarg ~S is required by class ~S, but ~
has not been provided.~@:>"
	     (missing-required-initarg-initarg condition)
	     (missing-required-initarg-class   condition))))
  (:documentation
   "This condition is signaled when an initarg that is required by a
class is not provided."))

(define-condition encoding-error (error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an encoding
error occurs."))

(define-condition decoding-error (error)
  ((offset :initarg  :offset
	   :type     non-negative-integer
	   :accessor decoding-error-offset
	   :documentation
	   "The buffer offset at which the decoding error occurred."))
  (:documentation
   "Conditions of this type and subtypes are signaled when a decoding
error occurs."))

(define-condition unexpected-wire-type (decoding-error)
  ((field         :initarg  :field
		  :type     (or symbol string)
		  :accessor unexpected-wire-type-field
		  :documentation
		  "The name of the field that was being processed when
the unexpected wire type was encountered. ")
   (expected-type :initarg  :expected-type
		  :type     wire-type
		  :accessor unexpected-wire-type-expected-type
		  :documentation
		  "The assumed wire type of the field.")
   (found-type    :initarg  :found-type
		  :type     wire-type
		  :accessor unexpected-wire-type-found-type
		  :documentation
		  "The wire type found in the data stream being
decoded."))
  (:report
   (lambda (condition stream)
     (let ((expected-type (unexpected-wire-type-expected-type condition))
	   (found-type    (unexpected-wire-type-found-type condition)))
       (format stream "~@<Invalid wire-type for field ~A at offset ~
~D. Wanted ~A (~A) but found ~A (~A).~@:>"
	       (unexpected-wire-type-field condition)
	       (decoding-error-offset condition)
	       expected-type (wire-type-meaning expected-type)
	       found-type   (wire-type-meaning found-type)))))
  (:documentation
   "This condition is signaled when a typecode is encountered that
cannot be processed in the context in which it appears."))

(define-condition unhandled-field-number (condition)
  ((number :initarg  :number
	   :type     non-negative-integer
	   :accessor unhandled-field-number-number
	   :documentation
	   "The number of the field that cannot be handled in the
current unpacking context.")
   (class  :initarg  :class
	   :type     symbol
	   :accessor unhandled-field-number-class
	   :documentation
	   "The name of the class an instance of was currently being
decoded when the unhandled field number was encountered."))
  (:report
   (lambda (condition stream)
     (with-slots (position) condition
       (format stream "~@<Unhandled field number ~D when decoding an ~
instance of class ~A.~@:>"
	       (unhandled-field-number-number condition)
	       (unhandled-field-number-class condition)))))
  (:documentation
   "This condition is signaled when a field number is encountered that
cannot be processed in the context in which it appears."))
