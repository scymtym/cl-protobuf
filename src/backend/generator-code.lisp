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
        ((enum-type-p proto-type)
         `(binio:uvarint-size (,(pb::symcat type 'code) ,value-form)))
        ((eq proto-type :string)
         `(if ,value-form
	      (pb::length-delim-size (binio:utf8-size ,value-form))
	    0))
        ((eq proto-type :bytes)
         `(if ,value-form
	      (pb::length-delim-size (length ,value-form))
	    0))
        (t
	 `(if ,value-form
	      (pb::length-delim-size (pb::packed-size ,value-form))
	    0)))))

(defun generate-repeated-size (value-var type pos)
  "Generate packed size of a repeated field of type TYPE."
  (if (fixed-p type)

      `(* (length ,value-var)
          (+ ,(generate-start-code-size type pos)
             ,(fixed-size type)))

      (with-unique-names (i accum)
        `(let ((,accum 0))
           (dotimes (,i (length ,value-var))
             (incf ,accum
                   ,(generate-scalar-size `(aref ,value-var ,i) type pos)))
           ,accum))))

(defun generate-repeated-packed-size (value-var type &optional pos)
  "Generate packed size of a repeated, packed field of type TYPE."
  (let ((array-size
         (cond
           ((fixed64-p type)
	    `(* 8 (length ,value-var)))
           ((fixed32-p type)
	    `(* 4 (length ,value-var)))
           ((eq type :boo)
	    `(length ,value-var))
           ((uvarint-p type)
            `(pb::packed-uvarint-size ,value-var))
           ((svarint-p type)
            `(pb::packed-uvarint-size ,value-var))
           ((enum-type-p type)
            `(pb::packed-enum-size #',(pb::symcat type 'code) ,value-var))
           (t
	    (error "Can't pack this type")))))
    (if pos
        `(+ ,(generate-start-code-size :bytes pos)
            (pb::length-delim-size ,array-size))
        array-size)))

(defun generate-slot-packed-size (name type pos
				  &key
				  packed? repeated?
				  object-var
				  &allow-other-keys)
  "Generate code to find the packed size of a single slot."
  (let ((slot-value  `(slot-value  ,object-var ',name))
	(slot-boundp `(slot-boundp ,object-var ',name)))
    `(if (and ,slot-boundp
	      ,@(when (pb::length-delim-p type)
		  `(,slot-value)))
	 ,(cond
	   ((and (not repeated?) (not packed?))
	    (generate-scalar-size slot-value type pos))
	   ((and repeated? (not packed?))
	    (generate-repeated-size slot-value type pos))
	   (packed?
	    (generate-repeated-packed-size slot-value type pos)))
       0)))

(defun generate-packed-size-method (name fields)
  "Generate `packed-size' method for NAME."
  (with-unique-names (object-var)
    `(defmethod protocol-buffer:packed-size ((,object-var ,name))
       (+ ,@(map 'list (rcurry #'funcall object-var) fields)))))


;;; Packer Code Generation
;;

(defun generate-offset-incrementer (type position
				    &key
				    buffer-var offset-var)
  `(incf ,offset-var
	 (pb::encode-start-code ,position
				,(proto-type->wire-type type)
				,buffer-var ,offset-var)))

(defun generate-value-packer (place type
			      &key
			      offset-var
			      buffer-var)
  "Generate code to pack a single value into the buffer."
  (once-only (place buffer-var)
    `(incf ,offset-var
	   ,(case type
	      ((:int32 :uint32 :uint64 :int64 :enum)
	       `(binio:encode-uvarint ,place ,buffer-var ,offset-var))
	      (:bool
	       `(pb::encode-bool ,place ,buffer-var ,offset-var))
	      ((:sint32 :sint64)
	       `(binio:encode-svarint ,place ,buffer-var ,offset-var))
	      ((:fixed32 :sfixed32)
	       `(binio:encode-int ,place :little ,buffer-var ,offset-var 32))
	      ((:fixed64 :sfixed64)
	       `(binio:encode-int ,place :little ,buffer-var ,offset-var 64))
	      ((:double)
	       `(binio:encode-double-float
		 ,place :little ,buffer-var ,offset-var))
	      ((:float)
	       `(binio:encode-single-float
		 ,place :little ,buffer-var ,offset-var))
	      (:string
	       (with-gensyms (strbuf size)
	         `(multiple-value-bind (,size ,strbuf)
		      (binio:encode-utf8 ,place)
		    (incf ,offset-var
			  (binio:encode-uvarint ,size ,buffer-var ,offset-var))
		    (replace ,buffer-var ,strbuf :start1 ,offset-var ,size))))
	      (:bytes
	       `(progn
		  (incf ,offset-var
			(binio:encode-uvarint (length ,place) ,buffer-var ,offset-var))
		  (replace ,buffer-var ,place :start1 ,offset-var ,(length place))))
	      (t ;; pack object
	       (if (enum-type-p type)
		   `(binio:encode-uvarint (,(pb::symcat type 'code) ,place)
					  ,buffer-var ,offset-var)
		   `(pb::pack-embedded ,place ,buffer-var ,offset-var)))))))

(defun generate-slot-packer (name type position
			     &key
			     repeated? packed?
			     buffer-var offset-var startsym object-var
			     &allow-other-keys)
  "Generate code to pack a single slot."
  (let ((slot-value `(slot-value  ,object-var ',name))
	(slot-bound `(slot-boundp ,object-var ',name))
	(count-var  (gensym)))
    `(when (and ,slot-bound ,@(when (pb::length-delim-p type)
				`(,slot-value)))
       ,@(cond
	  ;; Scalar slot
	  ((not repeated?)
	   `(,(generate-offset-incrementer type position
					   :buffer-var buffer-var
					   :offset-var offset-var)
	     ,(generate-value-packer name type
				     :offset-var offset-var
				     :buffer-var buffer-var)))

	  ;; Repeated, unpacked slot
	  ((not packed?)
	   `((dotimes (,count-var (length ,slot-value))
	       ,(generate-offset-incrementer type position
					     :buffer-var buffer-var
					     :offset-var offset-var)
	       ,(generate-value-packer
		 `(aref ,slot-value ,count-var) type
		 :buffer-var buffer-var
		 :offset-var offset-var))))

	  ;; Repeated, packet slot
	  (t
	   `(,(generate-offset-incrementer :bytes position
					   :buffer-var buffer-var
					   :offset-var offset-var)
	     ;; write length
	     ,(generate-value-packer
	       (generate-repeated-packed-size type slot-value) :uint64
	       :buffer-var buffer-var
	       :offset-var startsym)
	     ;; write elements
	     (dotimes (,count-var (length ,slot-value))
	       ,(generate-value-packer
		 `(aref ,slot-value ,count-var) type
		 :buffer-var buffer-var
		 :offset-var offset-var))))))))

(defun generate-pack-method (name fields)
  "Generate `pack' method for protocol buffer class NAME."
  (with-unique-names (object-var buffer-var start-var offset-var)
    `(defmethod protocol-buffer:pack ((,object-var ,name)
				      &key
				      (,buffer-var (binio:make-octet-vector
						    (pb:packed-size ,object-var)))
				      (,start-var  0))
       (declare (type binio:octet-vector ,buffer-var)
                (fixnum                  ,start-var))
       (let ((,offset-var ,start-var))
	 ,@(map 'list (rcurry #'funcall object-var) fields)
         (values (- ,offset-var ,start-var) ,buffer-var)))))


;;; Unpacker Code Generation
;;

(defun generate-decoder-name (proto-type)
  "Find the function symbol to decode this type."
  (check-type proto-type pb::proto-type "a protocol buffer type designator")

  (case proto-type
    ((:int32 :uint32 :int64 :uint64 :enum)
     'binio:decode-uvarint)
    ((:sint32 :sint64)
     'binio:decode-svarint)
    ((:fixed32)
     'pb::decode-uint32)
    ((:sfixed32)
     'pb::decode-sint32)
    ((:fixed64 )
     'pb::decode-uint64)
    ((:sfixed64)
     'pb::decode-sint64)
    (:string
     'pb::decode-string)
    (:bytes
     'pb::decode-bytes)
    (:double
     'pb::decode-double)
    (:float
     'pb::decode-single)
    (:bool
     'pb::decode-bool)
    (t
     (if (enum-type-p proto-type)
         (pb::symcat proto-type 'decode)
         (error "Can't find decoder for this type: ~A" proto-type)))))

(defun generate-value-unpacker (type buffer-var start &optional instance)
  (if (primitive-type-p type)
      `(,(generate-decoder-name type) ,buffer-var ,start)
      (progn
        (assert instance () "Need instance to unpack embedded message ~A" type)
        `(pb::unpack-embedded ,buffer-var ,instance ,start))))

(defun generate-unpack-and-incf (start-place buffer type &optional instance)
  "Unpack a scalar value and increment start."
  (with-unique-names (value length)
    `(pb::with-decoding (,value ,length)
         ,(generate-value-unpacker type buffer start-place instance)
       ,@(when (primitive-type-p type)
               `((declare (type ,(pb::scalar-proto-type->lisp-type type) ,value))))
       (incf ,start-place ,length)
       ,value)))

(defun generate-slot-unpacker (name type
			       &key
			       repeated? packed?
			       buffer-var startsym object-var)
  "Generate code to unpack a single slot"
  (let ((slot-value  `(slot-value  ,object-var ',name))
	(slot-boundp `(slot-boundp ,object-var ',name)))
    (cond
      ;; Scalar slot
      ((not repeated?)
       `(,@(unless (primitive-type-p type)
		   `((unless (and ,slot-boundp ,slot-value)
		       (setf ,slot-value
			     (make-instance ',(proto-type->lisp-type type))))))
	 (setf ,slot-value
	       ,(generate-unpack-and-incf startsym buffer-var type slot-value))))

      ;; Packed array
      (packed?
       `((pb::with-decoding (value length)
	     (pb::decode-length-delim
	      ,buffer-var ,startsym
	      (lambda (buffer start end)
		(pb::decode-array
		 ',(proto-type->lisp-type type)
		 #',(generate-decoder-name type)
		 buffer
		 :fixed-bit-size ,(when (fixed-p type)
					(* 8 (fixed-size type)))
		 :start          start
		 :end            end)))
	   (setf ,slot-value value)
	   (incf ,startsym length))))

      ;; array, not packed
      (t
       ;; FIXME: SBCL conses here because it's not a simple array
       ;; would be nice to avoid that
       `((vector-push-extend
          ,(generate-unpack-and-incf startsym buffer-var type
				     (unless (primitive-type-p type)
				       `(make-instance ',type)))
          ,slot-value))))))

(defun generate-unpack-method (name fields)
  "Generate code for the UNPACK method"
  (with-unique-names (buffer-var object-var start-var end-var position-var pos read-typecode-var startlen)
    `(defmethod protocol-buffer:unpack ((,buffer-var t) ;; TODO which type
					(,object-var ,name)
					&optional
					(,start-var 0)
					(,end-var   (length ,buffer-var)))
       (declare (type binio:octet-vector ,buffer-var)
                (type fixnum             ,start-var ,end-var))

       ;; Loop through BUFFER-VAR until we are at the end. Each
       ;; decoded field will increment POSITION-VAR by its length. We
       ;; return the filled object OBJECT-VAR and the size of the
       ;; consumed data.
       (do ((,position-var ,start-var))
           ((>= ,position-var ,end-var)
	    (values
	     ,object-var
	     (the fixnum (- ,position-var ,start-var))))
         (declare (fixnum ,position-var))

         (multiple-value-bind (,pos ,read-typecode-var ,startlen)
             (pb::read-start-code ,buffer-var ,position-var)
           (declare (fixnum ,pos ,read-typecode-var ,startlen))

	   ;; Increase position to account for consumed start-code.
           (incf ,position-var ,startlen)

           (cond
	     ,@(mapcar
		(lambda (field)
		  (multiple-value-bind (position body)
		      (funcall field read-typecode-var buffer-var position-var object-var)
		    `((= ,pos ,position)
		      ,@body)))
		fields)
             (t
	      (error "Unhandled position ~A in class ~A, buffer ~A, need to skip" ;; TODO use condition class
		     ,pos ',name ,buffer-var))))))))
