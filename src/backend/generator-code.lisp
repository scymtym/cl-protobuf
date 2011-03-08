;;; generator-code.lisp ---
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

(defun generate-scalar-size (value-form proto-type number) ;; TODO compare against default value?
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
         `(binio:uvarint-size ,value-form))
        ((svarint-p proto-type)
         `(binio:svarint-size ,value-form))
        ((eq proto-type :string)
         `(if (and ,value-form (string/= ,value-form "")) ;; TODO avoid
	      (pb::length-delim-size (binio:utf8-size ,value-form))
	      0))
        ((eq proto-type :bytes)
         `(if (and ,value-form (string/= ,value-form "")) ;; TODO avoid string stuff
	      (pb::length-delim-size (length ,value-form))
	      0))
	((enum-type-p proto-type)
         `(binio:uvarint-size (,(pb::symcat proto-type 'code) ,value-form)))
        (t ;; embedded protocol buffer
	 `(if ,value-form
	      (pb::length-delim-size (pb:packed-size ,value-form))
	      0)))))

(defun generate-repeated-size (value-form proto-type number)
  "Generate packed size of a repeated field of type PROTO-TYPE."
  (if (fixed-p proto-type)

      `(* (length ,value-form)
          (+ ,(generate-start-code-size proto-type number)
             ,(fixed-size proto-type)))

      (with-unique-names (i accum)
        `(let ((,accum 0))
           (dotimes (,i (length ,value-form))
             (incf ,accum
                   ,(generate-scalar-size `(aref ,value-form ,i) proto-type number)))
           ,accum))))

(defun generate-repeated-packed-size (proto-type value-form &optional number) ;; TODO ever called without number?
  "Generate packed size of a repeated, packed field of type TYPE."
  (let ((array-size
         (cond
           ((fixed-p proto-type)
	    `(* (fixed-size proto-type) (length ,value-form)))
	   ((eq proto-type :bool)
	    `(* (length ,value-form)))
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

(defun generate-slot-packed-size (name proto-type number
				  &key
				  packed? repeated?
				  object-var
				  &allow-other-keys)
  "Generate code to find the packed size of a single slot."
  (let ((slot-value    `(slot-value  ,object-var ',name))
	(slot-boundp   `(slot-boundp ,object-var ',name))
	(length-delim? (pb::length-delim-p proto-type)))
    `(if (and ,slot-boundp ,@(when length-delim? `(,slot-value)))
	 ,(cond
	   ((and (not repeated?) (not packed?))
	    (generate-scalar-size slot-value proto-type number))
	   ((and repeated? (not packed?))
	    (generate-repeated-size slot-value proto-type number))
	   (packed?
	    (generate-repeated-packed-size proto-type slot-value number)))
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
	  ,(generate-start-code-encoder
	    proto-type number buffer-form offset-form)
	  ,(generate-pack-and-incf
	    proto-type `(aref ,value-var ,index-var) buffer-form offset-form))))))

(defun generate-repeated-packed-slot-packer (proto-type number buffer-form offset-form value-form)
  "Generate code to pack the repeated and packed slot value
VALUE-FORM."
  (with-unique-names (value-var index-var)
    `((let ((,value-var ,value-form))
	;; write start code
	,(generate-start-code-encoder :bytes number buffer-form offset-form)
	;; write length
	,(generate-value-packer
	  :uint64 (generate-repeated-packed-size proto-type value-var)
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
	(length-delim? (pb::length-delim-p proto-type)))
    `(when (and ,slot-bound ,@(when length-delim? `(,slot-value)))
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

(defun generate-unpack-and-incf (proto-type buffer-form offset-form  &optional instance)
  "Unpack a scalar value and increment START-FORM."
  (with-unique-names (value length)
    `(pb::with-decoding (,value ,length)
         ,(generate-value-unpacker proto-type buffer-form offset-form instance)
       ,@(when (primitive-type-p proto-type)
               `((declare (type ,(pb::scalar-proto-type->lisp-type proto-type) ,value))))
       (incf ,offset-form ,length)
       ,value)))

(defun generate-scalar-slot-unpacker (proto-type buffer-form offset-form value-form)
  "Generate code to decode a scalar value into VALUE-FORM."
  `(,@(unless (primitive-type-p proto-type)
        `((unless ,value-form ;(and ,slot-boundp ,value-form)
	    (setf ,value-form (make-instance ',(proto-type->lisp-type proto-type))))))
    (setf ,value-form ,(generate-unpack-and-incf
			proto-type buffer-form offset-form value-form))))

(defun generate-repeated-slot-unpacker (proto-type buffer-form offset-form value-form)
  "Generate code to decode a repeated value into VALUE-FORM."
  ;; FIXME: SBCL conses here because it's not a simple array
  ;; would be nice to avoid that
  `((vector-push-extend
     ,(generate-unpack-and-incf
       proto-type buffer-form offset-form
       (unless (primitive-type-p proto-type)
	 `(make-instance ',(scalar-proto-type->lisp-type proto-type))))
     ,value-form)))

(defun generate-repeated-packed-slot-unpacker (proto-type buffer-form offset-form value-form)
  "Generate code to decode a repeated and packed value into
VALUE-FORM."
  `((pb::with-decoding (value length)
	(pb::decode-length-delim
	 ,buffer-form ,offset-form
	 (lambda (buffer start end)
	   (pb::decode-array
	    ',(proto-type->lisp-type proto-type)
	    #',(%proto-type->coder :decode proto-type)
	    buffer-form
	    :fixed-bit-size ,(when (fixed-p proto-type)
				   (* 8 (fixed-size proto-type)))
	    :start          start
	    :end            end)))
      (setf ,value-form value)
      (incf ,offset-form length))))

(defun generate-slot-unpacker (proto-type name buffer-form offset-form object-form
			       &key
			       repeated?
			       packed?)
  "Generate code to unpack a single slot"
  (let ((slot-value  `(slot-value  ,object-form ',name))
	;(slot-boundp `(slot-boundp ,object-var ',name))
	)
    (funcall (cond
	       ((not repeated?) #'generate-scalar-slot-unpacker)
	       ((not packed?)   #'generate-repeated-slot-unpacker)
	       (t               #'generate-repeated-packed-slot-unpacker))
	     proto-type buffer-form offset-form slot-value)))

(defun generate-unpack-method (name fields)
  "Generate code for the UNPACK method"
  (with-unique-names (buffer-var object-var start-var end-var
		      offset-var
		      number-var read-wire-type-var start-code-length-var)
    `(defmethod pb:unpack ((,buffer-var t) ;; TODO which type
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
       (do ((,offset-var ,start-var))
           ((>= ,offset-var ,end-var)
	    (values
	     ,object-var
	     (the fixnum (- ,offset-var ,start-var))))
         (declare (non-negative-fixnum ,offset-var))

         (multiple-value-bind (,number-var ,read-wire-type-var ,start-code-length-var)
             (pb::read-start-code ,buffer-var ,offset-var)
           (declare (non-negative-fixnum ,number-var ,read-wire-type-var
					 ,start-code-length-var))

	   ;; Increase offset to account for consumed start-code.
           (incf ,offset-var ,start-code-length-var)

           (cond
	     ,@(map 'list
		(lambda (field)
		  (multiple-value-bind (field-number body)
		      (funcall field read-wire-type-var buffer-var offset-var object-var)
		    `((= ,number-var ,field-number)
		      ,@body)))
		fields)
             (t
	      (cerror "Unhandled position ~A in class ~A, buffer ~A, need to skip" ;; TODO use condition class
		      ,number-var ',name ,buffer-var))))))))


;;; Utility functions
;;

(defun %proto-type->coder (direction proto-type)
  "Return the name of a en- or decoder (depending on DIRECTION) for
PROTO-TYPE. DIRECTION has to either :ENCODE or :DECODE. PROTO-TYPE can
be any protocol buffer type but a nested protocol buffer."
  (check-type direction  (member :encode :decode)
	      "either :encode or :decode")
  (check-type proto-type pb::proto-type "a protocol buffer type designator")

  (if (keywordp proto-type)
      (bind (((package name)
	      (cond
		((member proto-type '(:int32 :uint32 :int64 :uint64 :enum))
		 '(:binio "UVARINT"))
		((member proto-type '(:sint32 :sint64))
		 '(:binio "SVARINT"))
		((eq proto-type :bool)
		 '(:binio "BOOL"))
		((eq proto-type :fixed32)
		 '(:binio "UINT32-LE"))
		((eq proto-type :sfixed32)
		 '(:binio "SINT32-LE"))
		((eq proto-type :fixed64)
		 '(:binio "UINT64-LE"))
		((eq proto-type :sfixed64)
		 '(:binio "SINT64-LE"))
		((member proto-type '(:double :float))
		 `(:binio ,(format nil "~A-LE" proto-type)))
		((eq proto-type :string)
		 `(:pb "STRING"))
		((eq proto-type :bytes)
		 `(:pb "BYTES"))
		(t
		 (error "Invalid primitive protocol buffer type ~S"
			proto-type)))))
	(intern (format nil "~A-~A" direction name) package))
      (pb::symcat proto-type direction)))
