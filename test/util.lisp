;;; util.lisp --- Unit test for system-wide utilities.
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

(in-package :protocol-buffer.test)

(deftestsuite util-root (root)
  ()
  (:documentation
   "Unit tests for system-wide utilities."))

(deftestsuite ->lisp-name-root (util-root)
  ()
  (:documentation
   "Unit tests for the `->lisp-name' conversion function."))

(addtest (->lisp-name-root
          :documentation
	  "Smoke test for the `->lisp-name' conversion function.")
  smoke
  (ensure-same
   (pb::->lisp-name "bla.Foo.CamelCase_and_underscores")
   "BLA-FOO-CAMEL-CASE-AND-UNDERSCORES"
   :test #'string=))

(deftestsuite proto-type-name->lisp-type-symbol-root (util-root)
  ()
  (:documentation
   "Unit tests for the `proto-type-name->lisp-type-symbol' function."))

(addtest (proto-type-name->lisp-type-symbol-root
          :documentation
	  "Smoke test for the `proto-type-name->lisp-type-symbol' function")
  smoke

  (ensure-same
   (pb::proto-type-name->lisp-type-symbol "StandardClass" :package :cl)
   'cl:standard-class
   :test #'eq)
  (ensure-same
   (pb::proto-type-name->lisp-type-symbol ".cl.StandardClass")
   'cl:standard-class
   :test #'eq)
  (ensure-same
   (pb::proto-type-name->lisp-type-symbol ".ProtocolBuffer.Test.UtilRoot")
   'util-root
   :test #'eq))
