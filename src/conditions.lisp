;;; conditions.lisp --- Conditions used in the s-protobuf System.
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

(define-condition encoding-error (error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an encoding
error occurs."))

(define-condition decoding-error (error)
  ((offset :initarg  :offset
	   :type     non-negative-integer
	   :documentation
	   ""))
  (:documentation
   "Conditions of this type and subtypes are signaled when a decoding
error occurs."))

(define-condition unexpected-wire-type (decoding-error)
  ((expected-type :initarg  :expected-type
		  :type     wire-type
		  :documentation
		  "")
   (found-type    :initarg  :found-type
		  :type     wire-type
		  :documentation
		  ""))
  (:report
   (lambda (condition stream)
     (with-slots (expected-type found-type) condition
       (format stream "~@<Invalid wire-type for field ~A. Wanted ~A (~A) but found ~A (~A).~@>"
	       'TODO
	       desired-type (pb:wire-type-meaning desired-type)
	       found-type (pb:wire-type-meaning found-type)))))
  (:documentation
   "This condition is signaled when a typecode is encountered that
cannot be processed in the context in which it appears."))

(define-condition unhandled-position (decoding-error) ;; should be continueable with corresponding restart
  ((position :initarg  :position
	     :type     non-negative-integer
	     :documentation
	     ""))
  (:report
   (lambda (condition stream)
     (with-slots (position) condition
       (format stream "~@<Unhandled position ~D in class ~A, buffer ~A, need to skip~@:>"
	       position ',name ,buffer))))
  (:documentation
   "This condition is signaled when a position designator is
encountered that cannot be processed in the context in which it
appears."))
