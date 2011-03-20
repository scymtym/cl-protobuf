;;; target-serializer.lisp --- Emit (De)serializer.
;;
;; Copyright (C) 2009, 2010 Georgia Tech Research Corporation
;; Copyright (C) 2010, 2011 Jan Moringen
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

(in-package :protocol-buffer.backend)


;;; Packed Size
;;

(defmethod documentation ((thing (eql :packed-size)) (type (eql 'target)))
  "Emit serializer methods based on the protocol buffer representation
obtained by parsing the binary output of protoc.")

(defclass target-packed-size (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for packed-size target."))

(defmethod emit ((node   pb::message-desc)
		 (target target-packed-size)
		 &key)
  "Generate code for the `packed-size' method."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (nested pb::message-desc-nested-type)
	     (fields pb::message-desc-field)) node)
	   (name1 (intern* (make-lisp-class-name name parent))))
      (map 'nil #'recur nested)
      (eval (generate-packed-size-method
	     name1 (map 'list #'recur fields))))))

(defmethod emit ((node   pb::field-desc)
		 (target target-packed-size)
		 &key)
  "Generate code to compute the packed size of a single slot."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (number    pb::field-desc-number)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1     (intern* (make-lisp-slot-name name)))
	   (type1     (make-lisp-slot-type type type-name package))
	   (repeated? (eq label :repeated))
	   (packed?   (when options
			(pb::field-options-packed options))))
      #'(lambda (object-var)
	  (generate-slot-packed-size name1 type1 number object-var
				     :repeated?  repeated?
				     :packed?    packed?)))))


;;; Serializer
;;

(defmethod documentation ((thing (eql :serializer)) (type (eql 'target)))
  "Emit serializer methods based on the protocol buffer representation
obtained by parsing the binary output of protoc.")

(defclass target-serializer (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for serializer target."))

(defmethod emit ((node   pb::message-desc)
		 (target target-serializer)
		 &key)
  "Generate code for the `pack' method."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (nested pb::message-desc-nested-type)
	     (fields pb::message-desc-field)) node)
	   (name1 (intern* (make-lisp-class-name name parent))))
      (map 'nil #'recur nested)
      (eval (generate-pack-method name1 (map 'list #'recur fields))))))

(defmethod emit ((node   pb::field-desc)
		 (target target-serializer)
		 &key)
  "Generate code to pack a single slot."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (number    pb::field-desc-number)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1     (intern* (make-lisp-slot-name name)))
	   (type1     (make-lisp-slot-type type type-name package))
	   (repeated? (eq label :repeated))
	   (packed?   (when options
			(pb::field-options-packed options))))
      #'(lambda (buffer-var offset-var object-var)
	  (generate-slot-packer
	   type1 name1 number buffer-var offset-var object-var
	   :repeated? repeated?
	   :packed?   packed?)))))


;;; Deserializer
;;

(defmethod documentation ((thing (eql :deserializer)) (type (eql 'target)))
  "Emit serializer methods based on the protocol buffer representation
obtained by parsing the binary output of protoc.")

(defclass target-deserializer (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for deserializer target."))

(defmethod emit ((node   pb::message-desc)
		 (target target-deserializer)
		 &key)
  "Generate code for the `unpack' method."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (nested pb::message-desc-nested-type)
	     (fields pb::message-desc-field)) node)
	   (name1 (intern* (make-lisp-class-name name parent))))
      (map 'nil #'recur nested)
      (eval (generate-unpack-method name1 (map 'list #'recur fields))))))

(defmethod emit ((node   pb::field-desc)
		 (target target-deserializer)
		 &key)
  "Generate code to unpack a single slot"
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (number    pb::field-desc-number)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1     (intern* (make-lisp-slot-name name)))
	   (type1     (make-lisp-slot-type type type-name package))
	   (repeated? (eq label :repeated))
	   (packed?   (when options
			(pb::field-options-packed options)))
	   (desired-wire-type (proto-type->wire-type type repeated? packed?)))
      #'(lambda (read-wire-type-var buffer-form offset-form object-form)
	  (values
	   number
	   `((unless (= ,read-wire-type-var ,desired-wire-type) ;; TODO move to generator-code
	       (error 'pb:unexpected-wire-type
		      :field         ',name1
		      :offset        ,offset-form
		      :expected-type ,desired-wire-type
		      :found-type    ,read-wire-type-var))
	     ,@(generate-slot-unpacker
		type1 name1 buffer-form offset-form object-form
		:repeated? repeated?
		:packed?   packed?)))))))
