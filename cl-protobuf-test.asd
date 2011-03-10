;;; cl-protobuf-test.asd ---
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

(cl:in-package :cl-user)

(defpackage :cl-protobuf-test-system
  (:use
   :cl
   :asdf))

(in-package :cl-protobuf-test-system)

#.(load-system :cl-protobuf)

(defsystem :cl-protobuf-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "Unit tests for the cl-protobuf system"
  :depends-on  (:cl-protobuf
		:lift)
  :components  ((:module     "test"
		 :components (;; Protocol buffer descriptors for tests
			      (:module     "precompiled-descriptors"
			       :pathname   "data"
			       :components ((:static-file "addressbook.protobin")
					    (:static-file "addressbook.proto.expected")
					    (:static-file "developer-guide.protobin")
					    (:static-file "developer-guide.proto.expected")))

			      (:module     "descriptors"
			       :pathname   "data"
			       :default-component-class asdf::protocol-buffer-descriptor
			       :components ((:file "addressbook")
					    (:file "developer-guide")))

			      (:file       "package"
			       :depends-on ("descriptors"))

			      (:module     "binio"
			       :depends-on ("package")
			       :components ((:file       "package")
					    (:file       "binio"
					     :depends-on ("package"))))

			      (:file       "util"
			       :depends-on ("package"))

			      (:module     "backend"
			       :depends-on ("package")
			       :components ((:file       "package")
					    (:file       "target-proto"
					     :depends-on ("package"))))

			      (:module     "frontend"
			       :depends-on ("package")
			       :components ((:file       "package")
					    (:file       "s-expr"
					     :depends-on ("package"))
					    (:file       "protobin"
					     :depends-on ("package")))))))
  :in-order-to ((test-op (load-op :cl-protobuf-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-protobuf-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
