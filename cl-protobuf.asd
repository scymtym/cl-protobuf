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

(cl:in-package :cl-user)

#.(when (asdf:find-system :asdf-system-connections)
  (asdf:load-system :asdf-system-connections))

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
			      (:file       "conditions"
			       :depends-on ("package"))
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
			       :components ((:file       "conditions")
					    (:file       "util")
					    (:file       "protocol"
					     :depends-on ("util"
							  "conditions"))
					    (:file       "code-generating-target-mixin"
					     :depends-on ("util"))
					    (:file       "stream-target-mixin"
					     :depends-on ("protocol"))
					    (:file       "generator-code"
					     :depends-on ("conditions"
							  "util"))
					    (:file       "target-class"
					     :depends-on ("protocol"
							  "code-generating-target-mixin"))
					    (:file       "target-serializer"
					     :depends-on ("protocol"
							  "generator-code"
							  "code-generating-target-mixin"))
					    (:file       "target-proto"
					     :depends-on ("protocol"
							  "stream-target-mixin"))
					    (:file       "target-protofile"
					     :depends-on ("package"
							  "protocol"
							  "target-proto"))
					    (:file       "target-s-expr"
					     :depends-on ("protocol"
							  "code-generating-target-mixin"))
					    (:file       "target-graphviz"
					     :depends-on ("protocol"
							  "stream-target-mixin"))))

			      (:module     "frontend"
			       :depends-on ("frontend-early"
					    "bootstrap")
			       :components ((:file       "protobin")))

			      ;; Late "bootstrapping"
			      (:file       "bootstrap-late"
			       :depends-on ("bootstrap"
					    "backend"))

			      ;; ASDF support
			      (:file       "asdf"
			       :depends-on ("frontend"
					    "backend")))))
  :in-order-to ((test-op (test-op :cl-protobuf-test))))

#+asdf-system-connections
(defsystem-connection :cl-protobuf-and-yacc
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system connection provides a yacc-based parser
for the textual protocol buffer description format."
  :requires    (cl-protobuf
		yacc)
  :components  ((:module     "src"
		 :components ((:module     "frontend"
			       :components ((:file       "proto")))))))
