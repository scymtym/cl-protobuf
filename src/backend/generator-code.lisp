;;; generator-code.lisp ---
;;
;; Copyright (C) 2009, 2010 Georgia Tech Research Corporation
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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


;;; Packed Size Code Generation
;;

(defun generate-start-code-symbol (proto-type number)
  "Generate start code for the associated wire-type of PROTO-TYPE and
field number NUMBER."
  (pb::make-start-code number (proto-type->wire-type proto-type)))

(defun generate-start-code-size (proto-type number)
  "Return the encoded size of the start code for type PROTO-TYPE and
field number NUMBER."
  (binio:uvarint-size (generate-start-code-symbol proto-type number)))

(defun generate-scalar-size (proto-type number value-form) ;; TODO compare against default value?
  "Generate code to compute the packed size of a scalar field value
VALUE-FORM of type PROTO-TYPE when stored in field number NUMBER."
  (check-type proto-type proto-type "a protocol buffer type designator")

  `(+ ,(generate-start-code-size proto-type number)
      ,(cond
        ((fixed-p proto-type)
	 (fixed-size proto-type))
        ((eq proto-type :bool)
	 1)
        ((uvarint-p proto-type)
         `(,(%proto-type->coder :size-of proto-type) ,value-form))
        ((svarint-p proto-type)
         `(binio:svarint-size ,value-form))
        ((eq proto-type :string)
         `(if (and ,value-form (not (emptyp ,value-form)))
	      (pb::length-delim-size (binio:utf8-size ,value-form))
	      1))
        ((eq proto-type :bytes)
         `(if (and ,value-form (not (emptyp ,value-form)))
	      (pb::length-delim-size (length ,value-form))
	      1))
	((enum-type-p proto-type)
         `(binio:uvarint-size (,(pb::symcat proto-type 'code) ,value-form)))
        (t ;; embedded protocol buffer
	 `(if ,value-form
	      (pb::length-delim-size (pb:packed-size ,value-form))
	      1)))))

(defun generate-repeated-size (proto-type number value-form)
  "Generate packed size of a repeated field of type PROTO-TYPE."
  (if (fixed-p proto-type)

      `(* (length ,value-form)
          (+ ,(generate-start-code-size proto-type number)
             ,(fixed-size proto-type)))

      (with-unique-names (i accum)
        `(let ((,accum 0))
           (dotimes (,i (length ,value-form))
             (incf ,accum
                   ,(generate-scalar-size proto-type number `(aref ,value-form ,i))))
           ,accum))))

(defun generate-repeated-packed-size (proto-type number value-form)
  "Generate packed size of a repeated, packed field of type TYPE."
  (let ((array-size
         (cond
           ((fixed-p proto-type)
	    `(* ,(fixed-size proto-type) (length ,value-form)))
	   ((eq proto-type :bool)
	    `(length ,value-form))
           ((uvarint-p proto-type)
            `(pb::packed-uvarint-size ,value-form))
           ((svarint-p proto-type)
            `(pb::packed-uvarint-size ,value-form))
           ((enum-type-p proto-type)
            `(pb::packed-enum-size #',(pb::symcat proto-type 'code) ,value-form))
           (t
	    (error "Can't pack this type")))))
    (if number
        `(+ ,(generate-start-code-size :bytes number)
            (pb::length-delim-size ,array-size))
        array-size)))

(defun generate-slot-packed-size (name proto-type number object-var
				  &key
				  packed?
				  repeated?
				  &allow-other-keys) ;; TODO arguments
  "Generate code to find the packed size of a single slot."
  (let ((slot-value    `(slot-value  ,object-var ',name))
	(slot-boundp   `(slot-boundp ,object-var ',name))
	(sequence? (or (member proto-type '(:string :bytes))
		       repeated?)))
    `(if (and ,slot-boundp ,@(when sequence?
				   `((not (emptyp ,slot-value)))))
	 ,(funcall (cond
		     ((not repeated?) #'generate-scalar-size)
		     ((not packed?)   #'generate-repeated-size)
		     (t               #'generate-repeated-packed-size))
		   proto-type number slot-value)
	 0)))

(defun generate-packed-size-method (name fields)
  "Generate `packed-size' method for class NAME with slots described
by FIELDS. FIELDS has to be a sequence of functions of one argument.
These receive a form which will evaluate to the instance and return
code to compute the packed size of a slot value."
  (with-unique-names (object-var)
    `(defmethod pb:packed-size ((,object-var ,name))
       (+ ,@(map 'list (rcurry #'funcall object-var) fields)))))


;;; Packer Code Generation
;;

(defun generate-start-code-encoder (proto-type number buffer-form offset-form)
  `(incf ,offset-form (pb::encode-start-code
		       ,number
		       ,(proto-type->wire-type proto-type)
		       ,buffer-form ,offset-form)))

(defun generate-value-packer (proto-type value-form buffer-form offset-form)
  "Generate code to pack a single value into the buffer."
  (if (primitive-type-p proto-type)
      `(,(%proto-type->coder :encode proto-type)
	 ,value-form ,buffer-form ,offset-form)
      `(pb::pack-embedded ,value-form ,buffer-form ,offset-form)))

(defun generate-pack-and-incf (proto-type value-form buffer-form offset-form)
  "Generate code to pack the value VALUE-FORM of type PROTO-TYPE into
BUFFER-FORM and increment OFFSET-FORM appropriately."
  `(incf ,offset-form ,(generate-value-packer
			proto-type value-form buffer-form offset-form)))

(defun generate-scalar-slot-packer (proto-type number buffer-form offset-form value-form)
  "Generate code to pack the scalar slot value VALUE-FORM."
  `(,(generate-start-code-encoder
      proto-type number buffer-form offset-form)
    ,(generate-pack-and-incf
      proto-type value-form buffer-form offset-form)))

(defun generate-repeated-slot-packer (proto-type number buffer-form offset-form value-form)
  "Generate code to pack the repeated slot value VALUE-FORM."
  (with-unique-names (value-var index-var)
    `((let ((,value-var ,value-form))
	(dotimes (,index-var (length ,value-var))
	  ,@(generate-scalar-slot-packer
	     proto-type number buffer-form offset-form
	     `(aref ,value-var ,index-var)))))))

(defun generate-repeated-packed-slot-packer (proto-type number buffer-form offset-form value-form)
  "Generate code to pack the repeated and packed slot value
VALUE-FORM."
  (with-unique-names (value-var index-var)
    `((let ((,value-var ,value-form))
	;; write start code
	,(generate-start-code-encoder :bytes number buffer-form offset-form)
	;; write length
	,(generate-pack-and-incf
	  :uint64 (generate-repeated-packed-size proto-type nil value-var)
	  buffer-form offset-form)
	;; write elements
	(dotimes (,index-var (length ,value-var))
	  ,(generate-pack-and-incf
	    proto-type `(aref ,value-var ,index-var)
	    buffer-form offset-form))))))

(defun generate-slot-packer (proto-type name number buffer-form offset-form object-form
			     &key
			     repeated?
			     packed?)
  "Generate code to pack the slot NAME of type PROTO-TYPE at field
number NUMBER."
  (let ((slot-value    `(slot-value  ,object-form ',name))
	(slot-bound    `(slot-boundp ,object-form ',name))
	(sequence? (or (member proto-type '(:string :bytes))
		       repeated?)))
    `(when (and ,slot-bound ,@(when sequence?
				    `((not (emptyp ,slot-value)))))
       ,@(funcall (cond
		    ((not repeated?) #'generate-scalar-slot-packer)
		    ((not packed?)   #'generate-repeated-slot-packer)
		    (t               #'generate-repeated-packed-slot-packer))
		  proto-type number buffer-form offset-form slot-value))))

(defun generate-pack-method (name fields)
  "Generate `pack' method for protocol buffer class NAME."
  (with-unique-names (object-var buffer-var start-var offset-var)
    `(defmethod pb:pack ((,object-var ,name)
			 &optional
			 (,buffer-var (binio:make-octet-vector
				       (pb:packed-size ,object-var)))
			 (,start-var  0))
       (declare (type binio:octet-vector  ,buffer-var)
                (type non-negative-fixnum ,start-var))
       (let ((,offset-var ,start-var))
	 ,@(map 'list (rcurry #'funcall buffer-var offset-var object-var) fields)
         (values (- ,offset-var ,start-var) ,buffer-var)))))


;;; Unpacker Code Generation
;;

(defun generate-value-unpacker (proto-type buffer-var offset-form &optional instance)
  (if (primitive-type-p proto-type)
      `(,(%proto-type->coder :decode proto-type) ,buffer-var ,offset-form)
      (progn
        (assert instance () "Need instance to unpack embedded message ~A" proto-type)
        `(pb::unpack-embedded ,buffer-var ,instance ,offset-form))))

(defun generate-unpack-and-incf (proto-type buffer-form offset-form &optional instance)
  "Unpack a scalar value and increment START-FORM."
  (with-unique-names (value length)
    `(pb::with-decoding (,value ,length)
       ,(generate-value-unpacker proto-type buffer-form offset-form instance)
       ,@(when (primitive-type-p proto-type)
               `((declare (type ,(pb::scalar-proto-type->lisp-type proto-type) ,value))))
       (incf ,offset-form ,length)
       ,value)))

(defun generate-unpack/scalar (proto-type buffer-form offset-form destination-form)
  "Generate code to decode a scalar value into DESTINATION-FORM."
  `(,@(unless (primitive-type-p proto-type)
        `((unless ,destination-form ;(and ,slot-boundp ,destination-form)
	    (setf ,destination-form
		  (make-instance ',(proto-type->lisp-type proto-type))))))
    (setf ,destination-form
	  ,(generate-unpack-and-incf
	    proto-type buffer-form offset-form destination-form))))

(defun generate-unpack/repeated (proto-type buffer-form offset-form destination-form)
  "Generate code to decode a repeated value into DESTINATION-FORM."
  `((vector-push-extend
     ,(generate-unpack-and-incf
       proto-type buffer-form offset-form
       (unless (primitive-type-p proto-type)
	 `(make-instance ',(scalar-proto-type->lisp-type proto-type))))
     ,destination-form)))

(defun generate-unpack/repeated-packed (proto-type buffer-form offset-form destination-form)
  "Generate code to decode a repeated and packed value into
DESTINATION-FORM."
  `((pb::with-decoding (value length)
	(pb::decode-length-delim
	 ,buffer-form ,offset-form
	 #'(lambda (buffer start end)
	     (declare (type binio:octet-vector  buffer)
		      (type non-negative-fixnum start end))
	     (pb::decode-array
	      ',(proto-type->lisp-type proto-type)
	      #',(%proto-type->coder :decode proto-type)
	      buffer
	      :fixed-bit-size ,(when (fixed-p proto-type)
				     (* 8 (fixed-size proto-type)))
	      :start          start
	      :end            end)))
      (setf ,destination-form value)
      (incf ,offset-form length))))

(defun generate-unpack (proto-type buffer-form offset-form destination-form
			&key
			repeated?
			packed?)
  "Generate code to unpack a value of the type designated by
PROTO-TYPE from BUFFER-FROM at offset OFFSET-FORM and store it at
DESTINATION-FORM."
  (funcall (cond
	     ((not repeated?) #'generate-unpack/scalar)
	     ((not packed?)   #'generate-unpack/repeated)
	     (t               #'generate-unpack/repeated-packed))
	   proto-type buffer-form offset-form destination-form))

(defun generate-slot-unpacker (proto-type name buffer-form offset-form object-form
			       &key
			       repeated?
			       packed?)
  "Generate code to unpack a single slot"
  (generate-unpack
   proto-type buffer-form offset-form `(slot-value ,object-form ',name)
   :repeated? repeated?
   :packed?   packed?))

(defun generate-unpack-method (name fields)
  "Generate code for the UNPACK method"
  (with-unique-names (buffer-var object-var start-var end-var
		      offset-var number-var wire-type-var)
    `(defmethod unpack ((,buffer-var simple-array)
			(,object-var ,name)
			&optional
			(,start-var 0)
			(,end-var   (length ,buffer-var)))
       (declare (type binio:octet-vector  ,buffer-var)
                (type non-negative-fixnum ,start-var ,end-var))

       ;; Loop through BUFFER-VAR until we are at the end. Each
       ;; decoded field will increment OFFSET-VAR by its length. We
       ;; return the filled object OBJECT-VAR and the size of the
       ;; consumed data.
       (do-fields
	   ((,buffer-var ,start-var ,end-var ,offset-var ,number-var ,wire-type-var)
	    (values ,object-var (- ,offset-var ,start-var)))
	 (cond
	   ,@(map 'list
		  (lambda (field)
		    (bind (((:values number body)
			    (funcall field wire-type-var buffer-var offset-var object-var)))
		      `((= ,number-var ,number) ,@body)))
		  fields)
	   (t
	    (signal 'unhandled-field-number
		    :number ,number-var
		    :class  ',name)))))))


;;; Utility functions
;;

(defun %proto-type->coder (operation proto-type)
  "Return the name of a size-of, encoder or decoder (depending on
OPERATION) for PROTO-TYPE. OPERATION has to either :SIZE-OF, :ENCODE
or :DECODE. PROTO-TYPE can be any protocol buffer type but a nested
protocol buffer."
  (check-type operation (member :size-of :encode :decode))
  (check-type proto-type pb::proto-type
	      "a protocol buffer type designator")

  (flet ((make-name (package &rest args)
	   (format-symbol package "~{~A~^-~}" args)))
    (etypecase proto-type
      ((eql :enum)
       (make-name :binio operation "UVARINT"))
      ((member :int32 :uint32 :int64 :uint64)
       (make-name :binio operation (format nil "VAR~A" proto-type)))
      ((member :sint32 :sint64)
       (make-name :binio operation "SVARINT"))
      ((eql :bool)
       (make-name :binio operation "BOOL"))
      ((eql :fixed32)
       (make-name :binio operation "UINT32-LE"))
      ((eql :sfixed32)
       (make-name :binio operation "SINT32-LE"))
      ((eql :fixed64)
       (make-name :binio operation "UINT64-LE"))
      ((eql :sfixed64)
       (make-name :binio operation "SINT64-LE"))
      ((member :double :float)
       (make-name :binio operation proto-type "LE"))
      ((member :string :bytes)
       (make-name :pb operation proto-type))
      (symbol
       (make-name (symbol-package proto-type) proto-type operation))
      (t
       (error 'no-coder
	      :type proto-type)))))
