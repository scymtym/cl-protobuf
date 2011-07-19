;;; target-class.lisp --- Generate Lisp classes from descriptors.
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

(in-package :protocol-buffer.backend)

(defmethod documentation ((thing (eql :class)) (type (eql 'target)))
  "Define Lisp classes based on protocol buffer descriptors. The
generated classes will not automatically have associated `pack' and
`unpack' methods. These have to be generated separately.")


;;; Target class
;;

(defclass target-class (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for class code generation."))

(defclass target-class-forward (code-generating-target-mixin)
  ()
  (:documentation
   "Helper target for class target which generates empty classes for
name resolution."))


;;; Emitter methods for `target-class-forward'
;;

(defmethod emit ((node   message-desc)
		 (target target-class-forward)
		 &key)
  "Define an empty Lisp class for name resolution."
  (call-next-method)

  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (bind ((name1 (intern* (make-lisp-class-name name parent))))
	(eval `(cl:defclass ,name1 () ()))))))

(defmethod emit ((node   enum-desc)
		 (target target-class-forward)
		 &key)
  "Intern a symbol naming the enum for NODE."
  (with-emit-symbols
    (intern* (make-lisp-enum-name (descriptor-name node) parent))))


;;; Emitter methods for `target-class'
;;

(defmethod emit :before ((node   file-set-desc)
			 (target target-class)
			 &key)
  "Generate empty classes for name resolution. "
  (emit node :class-forward))

(defmethod emit ((node   message-desc)
		 (target target-class)
		 &key)
  "Define a Lisp class for NODE. "
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (bind (((:accessors-r/o (export? target-export?)) target)
	     (name1 (intern* (make-lisp-class-name name parent))))
	;; Evaluate nested definitions immediately so types are
	;; available.
	(map nil #'recur enum-type)
	(map nil #'recur nested-type)
	;;
	(eval `(progn ,@(generate-class name1 (map 'list #'recur field))))

	;; Generate descriptor retrieval method.
	(eval `(progn
		 (defmethod message-descriptor ((object ,name1))
		   ,node)
		 (defmethod descriptor-class ((descriptor (eql ,node)))
		   ,(find-class name1))))

	;; Export the class name, if requested.
	(when export?
	  (export name1 (symbol-package name1)))

	;; Return name of generated class.
	name1))))

(defmethod emit ((node   field-desc)
		 (target target-class)
		 &key)
  "Emit a slot specification for NODE."
  (with-emit-symbols
    (with-descriptor-fields (node field-desc)
      (bind ((name1      (intern* (make-lisp-slot-name name)))
	     (type1      (make-lisp-slot-type
			  type type-name package parent))
	     (packed?    (field-packed? node))
	     (class-name (intern* (make-lisp-class-name
				   (pb::message-desc-name parent)
				   grandparent)))) ;; TODO make a function
	#'(lambda ()
	    (apply #'generate-slot
		   name1 type1 label packed?
		   :class-name class-name
		   (unless (emptyp default-value)
		     (list :default default-value))))))))

(defmethod emit ((node   enum-desc)
		 (target target-class)
		 &key)
  "Emit an enum definition for NODE."
  (with-emit-symbols
    (with-descriptor-fields (node enum-desc)
      (bind ((name1 (intern* (make-lisp-enum-name name parent))))
	(eval `(progn ,@(generate-enum name1 (map 'list #'recur value))))

	;; Export the name of the enum, if requested.
	(when (target-export? target)
	  (export name1 (symbol-package name1)))

	;; Return name of generated enum
	name1))))

(defmethod emit ((node   enum-value-desc)
		 (target target-class)
		 &key)
  "Emit name-value-pair for NODE."
  (with-descriptor-fields (node enum-value-desc)
    `(,(make-lisp-enum-value name) ,number)))
