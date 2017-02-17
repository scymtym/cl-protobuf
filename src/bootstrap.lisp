;;; bootstrap.lisp --- Bootstrap Descriptors of Protocol Buffer Descriptors.
;;
;; Copyright (C) 2008 Google Inc.
;; Copyright (C) 2009 Georgia Tech Research Corporation
;; Copyright (C) 2010-2017 Jan Moringen
;;
;; Author: Neil T. Dantam
;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(defun bootstrap-descriptor (form)
  "Generate definitions for one message type. A message maps to a
class and potentially associated enums.

This is a throw-away function the single purpose of which is getting
the descriptor classes defined."
  (bind (((_ name &rest specs) form)
	 ((:flet filter (type))
	  (mapcar #'rest (remove type specs
				 :key #'first :test-not #'eq)))
	 ((:flet generate-internal-enum (spec))
	  (pbb::generate-enum
	   (let ((*package* (symbol-package name)))
	     (symbolicate name "/" (first spec)))
	   (rest spec)))
	 ((:flet make-field (spec))
	  (bind (((name1 type _
		   &rest args
		   &key
		   optional required repeated packed
                   (default nil default?))
                  spec)
		 (labels (intersection args '(:optional :required :repeated)))
		 (label  (or (first labels) :required)))
	    (declare (ignore optional required repeated))
	    (apply #'pbb::generate-slot name1 type label packed
                   :class-name name
                   (when default? (list :default default))))))
    (handler-bind
	(#+sbcl (sb-c::redefinition-warning #'muffle-warning))
      (eval
       `(progn
	  ,@(mapcan #'generate-internal-enum (filter :enum))
	  (defclass ,name ()
	    ,(map 'list #'make-field (filter :field)))
	  (export ',name (find-package :protocol-buffer)))))))

(defvar *reflective-descriptors-pathname*
  (asdf:component-pathname
   (asdf:find-component
    (asdf:find-system :cl-protobuf)
    '("descriptor-definitions")))
  "The pathname of the file which contains the s-expr definitions of
the reflective protocol buffer descriptors.")

;; Stage 1
;;
;; We use `bootstrap-descriptor' to generate class definitions for the
;; protocol buffer descriptors in descriptor-definitions.lisp:
;; + `pb:message-desc'
;; + `pb:field-desc'
;; etc.
;; Once we have these classes, we can use instances to describe
;; protocol buffers.
;;
;; Note: we have not yet expressed the protocol buffer descriptors
;; from reflection.lisp as *instances* of the generated classes. Until
;; now, we have only used them to *define* the classes. This
;; difference is important because the whole machinery is designed to
;; work with instances of the descriptor classes. For example, the
;; class definitions we have now do not enable us to read binary
;; protocol buffers or generate code.

(iter (for item in-file *reflective-descriptors-pathname*)
      (bootstrap-descriptor item))

;; Stage 2
;;
;; We instantiate the descriptor classes defined in stage 1 for the
;; protocol buffer descriptors in reflection.lisp. The resulting
;; instances describe the reflective protocol buffers in terms of the
;; reflective protocol buffers. These instance are not useful by
;; themselves, but we can feed them to the code generation machinery.

(defvar *reflective-descriptors*
  (iter (for item in-file *reflective-descriptors-pathname*)
	(collect (eval (pbf:process-message item))))
  "A list of reflective protocol buffer descriptor instances. The
descriptors are reflective in the sense that they describe the
protocol buffer descriptor class hierarchy in terms of `message-desc',
`field-desc' etc instances.")

(defvar *reflective-descriptor-file-set*
  (make-instance
   'file-set-desc
   :file (vector (make-instance
		  'file-desc
		  :name         "reflective-descriptors.proto"
		  :package      "protocol-buffer"
		  :message-type (coerce *reflective-descriptors* 'vector))))
  "A protocol buffer file set descriptor instance containing all
reflective protocol buffer descriptors in a single file.")

;; Stage 3
;;
;; We feed the instances describing the reflective protocol buffers to
;; the code generation machinery. This will generate methods for
;; serializing and deserializing on the corresponding classes. As a
;; result, we will be able to read binary protocol buffer descriptors
;; directly into `pb:message-desc' instances and then invoke the code
;; generation machinery, the textual printer, the s-expr emitter or
;; any other backend on them.

;; the code is in bootstrap-late.lisp because it has to be evaluated
;; after the backend has been loaded.
