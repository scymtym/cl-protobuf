;;; package.lisp --- Package definition for unit tests of the backend module.
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

(cl:in-package :cl-user)

(defpackage :protocol-buffer.backend.test
  (:use
   :cl
   :alexandria
   :iterate
   :bind
   :lift

   :protocol-buffer.backend)
  (:documentation
   "This package contains unit tests for the backend module"))

(in-package :protocol-buffer.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))

(addtest (backend-root
          :documentation
	  "Ensure specifying an invalid target signals an error.")
  invalid-target

  (ensure-condition 'no-such-target
    (emit nil :does-not-exist)))

(deftestsuite emitter-suite ()
  ((descriptors (mapcar
		 (lambda (name)
		   (pbf:load/binary
		    (asdf:component-pathname
		     (asdf:find-component
		      (asdf:find-system :cl-protobuf-test)
		      `("precompiled-descriptors"
			,(format nil "~A.protobin" name))))))
		 '("addressbook" "developer-guide"))))
  (:documentation
   "Superclass for emitter test suites."))
