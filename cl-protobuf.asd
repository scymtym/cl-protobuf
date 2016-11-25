;;; cl-protobuf.asd ---
;;
;; Copyright (C) 2010-2016 Jan Moringen
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

(defpackage :cl-protobuf-system
  (:use
   :cl
   :asdf))

(in-package :cl-protobuf-system)

(defsystem :cl-protobuf
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "Modified BSD License; see COPYING file for details."
  :description "A protocol buffer compiler. Based on s-protobuf by
Neil T. Dantam."
  :depends-on  (:alexandria
		:split-sequence
		:metabang-bind
		:iterate

		:closer-mop
		:cffi

		:yacc)
  :components  ((:module     "binio"
		 :pathname   "src/binio"
		 :components ((:file       "package")
			      (:file       "binio"
			       :depends-on ("package"))))

		;; Generic components like en-/decoding and utilities
		(:module     "src"
		 :depends-on ("binio")
		 :components ((:file       "package")
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "util"
			       :depends-on ("package"))
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "packing"
			       :depends-on ("package" "protocol"))))

		;; Parts of the backend that are used during
		;; bootstrapping
		(:module     "backend-early"
		 :pathname   "src/backend"
		 :depends-on ("src")
		 :components ((:file       "package")
			      (:file       "generator-macros"
			       :depends-on ("package"))
			      (:file       "generator-class"
			       :depends-on ("package"))))

		;; Parts of the frontend that are used during
		;; bootstrapping
		(:module     "frontend-early"
		 :pathname   "src/frontend"
		 :depends-on ("src")
		 :components ((:file       "package")
			      (:file       "s-expr"
			       :depends-on ("package"))))

		;; Bootstrapping: defines descriptor classes on some
		;; methods on them
		(:static-file "descriptor-definitions"
		 :pathname   "src/descriptor-definitions"
		 :type       "lisp")
		(:file       "bootstrap"
		 :pathname   "src/bootstrap"
		 :depends-on ("src" "backend-early" "frontend-early"))
		(:file       "descriptor-functions"
		 :pathname   "src/descriptor-functions"
		 :depends-on ("bootstrap"))

		;; Backend and frontend
		(:module     "backend"
		 :pathname   "src/backend"
		 :depends-on ("backend-early" "bootstrap")
		 :components ((:file       "conditions")
			      (:file       "util")
			      (:file       "macros")
			      (:file       "protocol"
			       :depends-on ("util" "conditions" "macros"))

			      (:file       "code-generating-target-mixin"
			       :depends-on ("util" "macros" "protocol"))
			      (:file       "field-type-delegating-target-mixin"
			       :depends-on ("macros" "protocol"))
			      (:file       "caching-target-mixin"
			       :depends-on ("protocol"))
			      (:file       "file-target-mixin")
			      (:file       "stream-target-mixin"
			       :depends-on ("protocol"))
			      (:file       "generator-code"
			       :depends-on ("conditions" "util"))

			      (:file       "target-lisp-name"
			       :depends-on ("protocol"))
			      (:file       "target-class"
			       :depends-on ("protocol"
					    "code-generating-target-mixin"
					    "field-type-delegating-target-mixin"
					    "caching-target-mixin"
					    "target-lisp-name"))
			      (:file       "target-relations"
			       :depends-on ("protocol" "target-lisp-name"))
			      (:file       "target-serializer"
			       :depends-on ("protocol" "generator-code"
					    "code-generating-target-mixin"
					    "field-type-delegating-target-mixin"
					    "caching-target-mixin"
					    "target-lisp-name"))
			      (:file       "target-offset"
			       :depends-on ("protocol"
					    "code-generating-target-mixin"
					    "caching-target-mixin"))
			      (:file       "target-proto"
			       :depends-on ("protocol" "stream-target-mixin"))
			      (:file       "target-protofile"
			       :depends-on ("protocol" "target-proto"))
			      (:file       "target-graphviz"
			       :depends-on ("protocol" "stream-target-mixin"))))

		(:module     "frontend"
		 :pathname   "src/frontend"
		 :depends-on ("frontend-early" "bootstrap")
		 :components ((:file       "conditions")
			      (:file       "variables")

			      (:file       "lexer")
			      (:file       "parser"
			       :depends-on ("lexer"))
			      (:file       "proto"
			       :depends-on ("conditions" "variables"
					    "parser" "lexer"))

			      (:file       "protobin")))

		;; Late "bootstrapping"
		(:file       "bootstrap-late"
		 :pathname   "src/bootstrap-late"
		 :depends-on ("bootstrap" "backend"))

		;; ASDF support
		(:file       "asdf"
		 :pathname   "src/asdf"
		 :depends-on ("frontend" "backend")))

  :in-order-to ((test-op (test-op :cl-protobuf-test))))
