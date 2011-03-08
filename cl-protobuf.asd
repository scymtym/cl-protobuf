;;; cl-protobuf.asd ---
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

(defpackage :cl-protobuf-system
  (:use
   :cl
   :asdf))

(in-package :cl-protobuf-system)

(defsystem :cl-protobuf
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "A protocol buffer compiler. Based on s-protobuf."
  :depends-on  (:alexandria
		:metabang-bind
		:iterate

		:cffi)
  :components  ((:module     "src"
		 :components ((:module     "binio"
			       :components ((:file       "package")
					    (:file       "binio"
					     :depends-on ("package"))))

			      ;; Generic components like en-/decoding
			      ;; and utilities
			      (:file       "package"
			       :depends-on ("binio"))
			      (:file       "util"
			       :depends-on ("package"))
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "packing"
			       :depends-on ("package"
					    "protocol"))

			      ;; Parts of the backend that are used
			      ;; during bootstrapping
			      (:module     "backend-early"
			       :pathname   "backend"
			       :depends-on ("package"
					    "packing")
			       :components ((:file       "package")
					    (:file       "generator-class"
					     :depends-on ("package"))))

			      ;; Parts of the frontend that are used
			      ;; during bootstrapping
			      (:module     "frontend-early"
			       :pathname   "frontend"
			       :depends-on ("package")
			       :components ((:file       "package")
					    (:file       "s-expr"
					     :depends-on ("package"))))

			      ;; Bootstrapping: defines descriptor
			      ;; classes on some methods on them
			      (:static-file "descriptor-definitions"
			       :type       "lisp")
			      (:file       "bootstrap"
			       :depends-on ("package" "util" "types"
					    "backend-early"
					    "frontend-early"))
			      (:file       "descriptor-functions"
			       :depends-on ("bootstrap"))

			      ;; Backend and frontend
			      (:module     "backend"
			       :depends-on ("backend-early"
					    "bootstrap")
			       :components ((:file       "util")
					    (:file       "protocol"
					     :depends-on ("util"))
					    (:file       "code-generating-target-mixin"
					     :depends-on ("util"))
					    (:file       "generator-code"
					     :depends-on ("util"))
					    (:file       "target-class"
					     :depends-on ("protocol"
							  "code-generating-target-mixin"))
					    (:file       "target-serializer"
					     :depends-on ("protocol"
							  "generator-code"
							  "code-generating-target-mixin"))
					    (:file       "target-proto"
					     :depends-on ("protocol"))))

			      (:module     "frontend"
			       :depends-on ("frontend-early"
					    "bootstrap")
			       :components ((:file       "protobin")))

			      ;; Late "bootstrapping"
			      (:file       "bootstrap-late"
			       :depends-on ("bootstrap"
					    "backend")))))
  :in-order-to ((test-op (test-op :cl-protobuf-test))))

(defsystem :cl-protobuf-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "Unit tests for the cl-protobuf system"
  :depends-on  (:cl-protobuf
		:lift)
  :components  ((:module     "test"
		 :components ((:file       "package")

			      (:module     "binio"
			       :depends-on ("package")
			       :components ((:file       "package")
					    (:file       "binio"
					     :depends-on ("package"))))

			      (:file       "util"
			       :depends-on ("package"))

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
