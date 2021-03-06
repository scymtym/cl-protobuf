;;; cl-protobuf-test.asd ---
;;
;; Copyright (C) 2010-2017 Jan Moringen
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

(defsystem :cl-protobuf-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "Modified BSD License" ; see COPYING file for details.
  :description "Unit tests for the cl-protobuf system"
  :defsystem-depends-on (:cl-protobuf)
  :depends-on  (:cl-protobuf
		:lift)
  :components  (;; Protocol buffer descriptors for tests
		(:module     "precompiled-descriptors"
	         :pathname   "test/data"
		 :components ((:static-file "addressbook.protobin")
			      (:static-file "addressbook.proto.expected")
			      (:static-file "developer-guide.protobin")
			      (:static-file "developer-guide.proto.expected")
			      (:static-file "developer-guide.sexpr.expected")))

		(:module     "descriptors"
		 :pathname   "test/data"
		 :default-component-class asdf::protocol-buffer-descriptor
		 :components ((:file "addressbook")
			      (:file "developer-guide")
			      (:file "empty")
			      (:file "crlf")))

		(:module     "syntax-errors"
		 :pathname   "test/data/syntax-errors"
		 :components ((:static-file "adjacent-dots.proto")
			      (:static-file "invalid-toplevel.proto")))

		(:file       "package"
		 :pathname   "test/package"
		 :depends-on ("descriptors"))

		(:module     "binio"
		 :pathname   "test/binio"
		 :depends-on ("package")
		 :components ((:file       "package")
			      (:file       "binio"
			       :depends-on ("package"))))

		(:file       "util"
		 :pathname   "test/util"
		 :depends-on ("package"))
		(:file       "pack"
		 :pathname   "test/pack"
		 :depends-on ("package"))
		(:file       "unpack"
		 :pathname   "test/unpack"
		 :depends-on ("package"))

		(:module     "backend"
		 :pathname   "test/backend"
		 :depends-on ("package" "descriptors")
		 :components ((:file       "package")
			      (:file       "target-proto"
			       :depends-on ("package"))
			      (:file       "target-class"
			       :depends-on ("package"))
			      (:file       "target-serializer"
			       :depends-on ("package"))
			      (:file       "target-graphviz"
			       :depends-on ("package"))))

		(:module     "frontend"
		 :pathname   "test/frontend"
		 :depends-on ("package")
		 :components ((:file       "package")
			      (:file       "protobin"
			       :depends-on ("package"))
			      (:file       "proto"
			       :depends-on ("package")))))

  :in-order-to ((test-op (load-op :cl-protobuf-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-protobuf-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
