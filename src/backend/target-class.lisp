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

(defmethod emit :around ((node   pb::file-desc)
			 (target (eql :class)))
  (maybe-make-package (pb::file-desc-package node))

  (handler-bind (#+sbcl(sb-c::redefinition-warning #'muffle-warning))
    (call-next-method)))

(defmethod emit ((node   pb::message-desc)
		 (target (eql :class)))
  "Define a Lisp class for NODE. "
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (nested pb::message-desc-nested-type)
	     (enums  pb::message-desc-enum-type)
	     (fields pb::message-desc-field)) node) ;; field is actually a sequence of fields
	   (name1 (intern* (make-lisp-class-name name parent)))
	   (forms (generate-class name1 (map 'list #'recur fields))))
      ;; Evaluated nested definitions immediately so type are
      ;; available.
      (unless (emptyp enums)
	(eval `(progn
		 ,@(apply #'append (map 'list #'recur enums)))))
      (unless (emptyp nested)
	(eval `(progn
		 ,@(apply #'append (map 'list #'recur nested)))))
      ;; For nested messages, aggregate forms into parent forms. For
      ;; top-level messages, go ahead and evaluate.
      (if (typep parent 'pb::message-desc)
	  forms
	  (eval `(progn ,@forms))))))

;; TODO handle default value
(defmethod emit ((node   pb::field-desc)
		 (target (eql :class)))
  "Emit a slot specification for NODE."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1 (intern* (make-lisp-slot-name name)))
	   (type1 (if (member type '(:message :enum))
		      (pb::proto-type-name->lisp-type-symbol type-name)
		      type))) ;; TODO do this properly; same code in target-serializer
      #'(lambda ()
	  (generate-slot name1 type1 label
			 (when options (recur options)))))))

(defmethod emit ((node   pb::field-options)
		 (target (eql :class)))
  "Emit a Boolean value indicating whether the packed option is set in
NODE."
  (pb::field-options-packed node))

(defmethod emit ((node   pb::enum-desc)
		 (target (eql :class)))
  "Emit enum definition for NODE."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::enum-desc-name)
	     (values pb::enum-desc-value)) node)
	   (name1 (intern* (make-lisp-enum-name name parent))))
      (generate-enum name1 (map 'list #'recur values))))) ;; TODO when do we evaluate top-level enums?

(defmethod emit ((node   pb::enum-value-desc)
		 (target (eql :class)))
  "Emit name-value-pair for NODE."
  (bind (((:accessors-r/o
	   (name   pb::enum-value-desc-name)
	   (number pb::enum-value-desc-number)) node)
	 (name1 (make-lisp-enum-value name)))
    `(,name1 ,number)))
