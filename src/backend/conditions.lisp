;;; conditions.lisp --- Conditions used in the backend.
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

(in-package :protocol-buffer.backend)

(define-condition no-coder (error)
  ((type :initarg  :type
	 :type     symbol
	 :accessor no-coder-type
	 :documentation
	 "The protocol buffer type for which no coder could be
found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Cannot find en- or decoder for protocol buffer type ~S.~@:>"
	     (no-coder-type condition))))
  (:documentation
   "This error is signaled when no en- or decoder can be found for a
specified protocol buffer type."))

(define-condition no-such-target (error)
  ((name :initarg  :name
	 :type     (or string symbol)
	 :accessor no-such-target-name
	 :documentation
	 ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The specified target class ~S cannot be found.~@:>"
	     (no-such-target-name condition))))
  (:documentation
   "This error is signaled if a specified target class cannot be
found."))

(define-condition no-such-package (error)
  ((name :initarg  :name
	 :type     (or string symbol)
	 :accessor no-such-package-name
	 :documentation
	 "The name designating the package that could not be found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The name ~S does not designate a package.~@:>"
	     (no-such-package-name condition))))
  (:documentation
   "This error is signaled when a package that has been specified as
the target for a code generation process could not be found."))

(define-condition no-such-package-for-name (no-such-package)
  ((orig-name :initarg  :orig-name
	      :type     (or string symbol)
	      :accessor no-such-package-orig-name
	      :documentation
	      "The original name from which the package name has been
derived."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The name ~S (translated from ~S) does not ~
designate a package.~@:>"
	     (no-such-package-name      condition)
	     (no-such-package-orig-name condition))))
  (:documentation
   "This error is signaled when a package that is designated by a name
which has been translated from some other name cannot be found. Most
commonly, \"other\" names are protocol buffer package names."))
