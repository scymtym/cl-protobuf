;;; binio.lisp --- Binary encoding
;;
;; Copyright (C) 2009 Georgia Tech Research Corporation
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Neil T. Dantam
;;         Jan Moringen <jmoringe@techfak.uni-bielefe.de>
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

;; types u?int{8,16,32,63}, double, float
;;
;; FTypes
;; + Encoding functions
;;   (value &optional buffer start) => (values bytes-encoded buffer)
;; + Decoding functions
;;   (buffer &optional start)       => (values value bytes-decoded)

(in-package :binio)


;;; Octet vector type
;;

(deftype octet () '(unsigned-byte 8))

(deftype octet-vector (&optional count)
  `(simple-array octet (,count)))

(declaim (ftype (function (fixnum) octet-vector) make-octet-vector)
	 (inline make-octet-vector))

(defun make-octet-vector (count)
  (make-array count :element-type 'octet))

(declaim (ftype (function (&rest list) octet-vector) octet-vector)
	 (inline octet-vector))

(defun octet-vector (&rest args)
  "Return a new octet-vector containing the elements ARGS."
  (make-array (length args)
	      :element-type     'octet
	      :initial-contents args))


;;; Bool en- and decoder
;;

(declaim (ftype (function (t &optional octet-vector fixnum)
			  (values bit octet-vector))
		encode-bool)
	 (inline encode-bool))

(defun encode-bool (val
		    &optional
		    (buffer (make-octet-vector 1))
		    (start  0))
  (setf (aref buffer start) (if val 1 0))
  (values 1 buffer))

(declaim (ftype (function (octet-vector &optional fixnum)
			  (values (member nil t) (eql 1)))
		decode-bool)
	 (inline decode-bool))

(defun decode-bool (buffer &optional start)
  (values (case (aref buffer start)
            (0 nil)
            (1 t)
            (t (error "Invalid boolean value")))
          1))


;;; CFFI decoders
;;

;; FIXME: this CFFI hack is much faster than letting lisp clumsily
;; screw with bignums.  Everything should get converted to use this
;; eventually.

(eval-when (:compile-toplevel :load-toplevel)
  (unless (fboundp 'needs-byteswap)
    (defun needs-byteswap (endian)
      (assert (or (eq :little endian)
		  (eq :big endian)))
      (let ((native-endian (ecase (cffi:with-foreign-object (x :uint16)
				    (setf (cffi:mem-aref x :uint8 0) 1)
				    (setf (cffi:mem-aref x :uint8 1) 0)
				    (cffi:mem-ref x :uint16))
			     (1 :little)
			     (256 :big))))
	(not (eq endian native-endian))))))

;; A fast path for SBCL when native and encoded are the same
;; Question: Do we need to pin the buffer?
#+sbcl
(defmacro def-decoder-sbcl (c-type)
  (let ((foreign-size (cffi:foreign-type-size c-type)))
    `(progn
       (assert (>= (length buffer)
		   (+ start ,foreign-size))
	       () "Buffer too small for requested data type: ~A" ,c-type)
       (values
	(cffi:mem-ref (cffi:inc-pointer (sb-sys:vector-sap buffer) start)
		      ,c-type)
	,foreign-size))))

(defmacro def-decoder-cffi (c-type swap)
  (let ((foreign-size (cffi:foreign-type-size c-type)))
    `(cffi:with-foreign-object (x ,c-type)
       (setf ,@(loop
                  with n = (cffi:foreign-type-size c-type)
                  for i below n
                  for j = (if swap
                              (- n i 1)
                              i)
                  append
                    `((cffi:mem-aref x :uint8 ,j)
                      (aref buffer (+ start ,i)))))
       (values
	(cffi:mem-ref x ,c-type)
	,foreign-size))))

(defmacro def-decoder (name c-type lisp-type endian)
  (let* ((foreign-size   (cffi:foreign-type-size c-type))
	 (swap?          (needs-byteswap endian))
	 (sbcl-fastpath? (and (not swap?)
			      (string= "SBCL" (lisp-implementation-type)))))
    `(progn
       (declaim (ftype (function (octet-vector &optional fixnum)
				 (values ,lisp-type (eql ,foreign-size)))
		       ,name)
		(inline ,name))
       (defun ,name (buffer &optional (start 0))
	 (the ,lisp-type
	   ,(if sbcl-fastpath?
		`(def-decoder-sbcl ,c-type)
		`(def-decoder-cffi ,c-type ,swap?)))))))

;; Note: swapping the byte-order is about 5 times slower
(def-decoder decode-double-float-le :double double-float       :little)
(def-decoder decode-double-float-be :double double-float       :big)

(def-decoder decode-float-le        :float  single-float       :little)
(def-decoder decode-float-be        :float  single-float       :big)

(def-decoder decode-sint64-le       :int64  (signed-byte 64)   :little)
(def-decoder decode-sint64-be       :int64  (signed-byte 64)   :big)

(def-decoder decode-uint64-le       :uint64 (unsigned-byte 64) :little)
(def-decoder decode-uint64-be       :uint64 (unsigned-byte 64) :big)

(def-decoder decode-sint32-le       :int32  (signed-byte 32)   :little)
(def-decoder decode-sint32-be       :int32  (signed-byte 32)   :big)

(def-decoder decode-uint32-le       :uint32 (unsigned-byte 32) :little)
(def-decoder decode-uint32-be       :uint32 (unsigned-byte 32) :big)

(def-decoder decode-sint16-le       :int16  (signed-byte 16)   :little)
(def-decoder decode-sint16-be       :int16  (signed-byte 16)   :big)

(def-decoder decode-uint16-le       :uint16 (unsigned-byte 16) :little)
(def-decoder decode-uint16-be       :uint16 (unsigned-byte 16) :big)

;;;;;;;;;;;;;;;;;;;;;
;;; CFFI ENCODERS ;;;
;;;;;;;;;;;;;;;;;;;;;

;; A fast path for SBCL when native and encoded are the same
;; Question: Do we need to pin the buffer?
#+sbcl
(defmacro def-encoder-sbcl (c-type)
  `(progn
     (assert (>= (length buffer)
                 (+ start ,(cffi:foreign-type-size c-type)))
             () "Buffer too small for requested data type: ~A" ,c-type)
     (setf (cffi:mem-ref (cffi:inc-pointer (sb-sys:vector-sap buffer)
                                           start)
                         ,c-type)
           value)))


(defmacro def-encoder-cffi (c-type swap)
  `(cffi:with-foreign-object (x ,c-type)
     (setf (cffi:mem-ref x ,c-type) value)
     (setf ,@(loop
                with n = (cffi:foreign-type-size c-type)
                for i below n
                for j = (if swap
                            (- n i 1)
                            i)
                append
                  `((aref buffer (+ start ,i))
                    (cffi:mem-aref x :uint8 ,j))))
     (cffi:mem-ref x ,c-type)))

(defmacro def-encoder (name c-type lisp-type endian)
  (let* ((foreign-size   (cffi:foreign-type-size c-type))
	 (swap?          (needs-byteswap endian))
	 (sbcl-fastpath? (and (not swap?)
			      nil
			      (string= "SBCL" (lisp-implementation-type)))))
  `(progn
     (declaim (ftype (function (,lisp-type
				&optional
				octet-vector
				non-negative-fixnum)
			       (values non-negative-fixnum octet-vector))
		     ,name)
	      (inline ,name))
     (defun ,name (value
		   &optional
                   (buffer (make-octet-vector ,foreign-size))
                   (start 0))
       ,(if sbcl-fastpath?
	    `(def-encoder-sbcl ,c-type)
	    `(def-encoder-cffi ,c-type ,swap?))
       (values ,foreign-size buffer)))))

(def-encoder encode-double-le :double double-float       :little)
(def-encoder encode-double-be :double double-float       :big)
(def-encoder encode-float-le  :float  single-float       :little)
(def-encoder encode-float-be  :float  single-float       :big)

(def-encoder encode-uint64-le :uint64 (unsigned-byte 64) :little)
(def-encoder encode-uint64-be :uint64 (unsigned-byte 64) :big)
(def-encoder encode-sint64-le :int64  (signed-byte 64)   :little)
(def-encoder encode-sint64-be :int64  (signed-byte 64)   :big)

(def-encoder encode-uint32-le :uint32 (unsigned-byte 32) :little)
(def-encoder encode-uint32-be :uint32 (unsigned-byte 32) :big)
(def-encoder encode-sint32-le :int32  (signed-byte 32)   :little)
(def-encoder encode-sint32-be :int32  (signed-byte 32)   :big)

(def-encoder encode-uint16-le :uint16 (unsigned-byte 16) :little)
(def-encoder encode-uint16-be :uint16 (unsigned-byte 16) :big)
(def-encoder encode-sint16-le :int16  (signed-byte 16)   :little)
(def-encoder encode-sint16-be :int16  (signed-byte 16)   :big)

;;;;;;;;;;;;;;;;;;;;;
;;; integer types ;;;
;;;;;;;;;;;;;;;;;;;;;

(declaim (inline decode-uint decode-sint encode-int))

(defun decode-uint (buffer endian &optional (start 0) (bits 32))
  (declare (fixnum start bits)
           (type octet-vector buffer))
  (do* ((n (the fixnum (/ bits 8)))
        (end (+ start n))
        (accum 0)
        (i start (1+ i))
        (k (the fixnum
             (ecase endian
               (:little 0)
               (:big (* 8 (1- n)))))
           (the fixnum
             (ecase endian
               (:little (+ k 8))
               (:big (- k 8))))))
       ((= i end) (values accum n))
    (setq accum (dpb (aref buffer i)
                     (byte 8 k)
                     accum))))

(defun decode-sint (buffer endian &optional (start 0) (bits 32) )
  (declare (fixnum start bits)
           (type (octet-vector) buffer))
  (let ((result (decode-uint buffer endian start bits))
        (count (/ bits 8)))
    (declare (fixnum count))
    (when (logbitp (1- bits) result)
      (decf result (ash 1 bits)))
    (values result count)))

(declaim (ftype (function (integer symbol &optional (or null binio:octet-vector) fixnum fixnum) *)
		encode-int))

(defun encode-int (val endian
		   &optional
		   buffer
		   (start 0)
		   (bits  32))
  (do* ((n (the fixnum (/ bits 8)))
        (buffer (or buffer (make-octet-vector n)))
        (end (+ start n))
        (i start (1+ i))
        (k (the fixnum
             (ecase endian
               (:little 0)
               (:big    (* 8 (1- n)))))
           (the fixnum
             (ecase endian
               (:little (+ 8 k))
               (:big    (- k 8))))))
       ((= i end) (values n buffer))
    (setf (aref buffer i) (ldb (byte 8 k) val))))

  ;; (let* ((count (/ bits 8))
  ;;        (buffer (or buffer (make-octet-vector count))))
  ;;   (declare (type octet-vector buffer))
  ;;   (dotimes (i count)
  ;;     (setf (aref-endian buffer i start count endian)
  ;;           (ldb (byte 8 (* i 8)) val)))
  ;;   (values (/ bits 8) buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; floating point types ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-cffi-cast (name from-lisp-type from-c-type to-c-type)
  (with-unique-names (val x)
    `(progn
       (declaim (inline ,name))
       (defun ,name (,val)
         "Use CFFI to extract the bits of val by a C-like cast."
         (declare (type ,from-lisp-type ,val))
         (cffi:with-foreign-object (,x ,from-c-type)
           (setf (cffi:mem-ref ,x ,from-c-type) ,val)
           (cffi:mem-ref ,x ,to-c-type))))))

(def-cffi-cast scary-single-float-bits single-float       :float  :uint32)
(def-cffi-cast scary-make-single-float (unsigned-byte 32) :uint32 :float)

(def-cffi-cast scary-double-float-bits double-float       :double :uint64)
(def-cffi-cast scary-make-double-float (unsigned-byte 64) :uint64 :double)

(declaim (inline decode-double-float)
	 (ftype (function (octet-vector symbol &optional integer) *) decode-double-float))

(defun decode-double-float (buffer endian &optional (start 0))
  (scary-make-double-float (decode-uint buffer endian start 64)))

(declaim (inline encode-double-float))

(defun encode-double-float (val endian &optional buffer (start 0))
  (let ((bits   (scary-double-float-bits val))
        (buffer (or buffer (make-octet-vector 8))))
    (declare (type octet-vector buffer))
    (encode-int bits endian buffer start 64)))

(declaim (ftype (function (octet-vector t &optional integer) *) decode-single-float))

(defun decode-single-float (buffer endian &optional (start 0))
  (scary-make-single-float (decode-sint buffer endian start)))

(defun encode-single-float (val endian
			    &optional
			    (buffer (binio:make-octet-vector 4))
			    (start  0))
  (let ((bits (scary-single-float-bits val)))
    (declare (type octet-vector buffer))
    (encode-int bits endian buffer start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fixed integer types ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro def-uint-decoder (name bits endian)
;;     `(progn
;;        (declaim (inline ,name))
;;        (defun ,name (buffer &optional (start 0))
;;          (declare (type octet-vector buffer)
;;                   (type fixnum start))
;;          (logior ,@(loop
;;                       with n = (/ bits 8)
;;                       for i below n
;;                       for j = (ecase endian
;;                                 (:little i)
;;                                 (:big (- n i 1)))
;;                       collect
;;                         `(ash (aref buffer (+ start ,i)) ,(* 8 j)))))))

;; (defmacro def-sint-decoder (name bits uint-decoder)
;;   `(progn
;;      (declaim (inline ,name))
;;      (defun ,name (buffer &optional (start 0))
;;        (declare (type octet-vector buffer)
;;                 (type fixnum start))
;;        (let ((u (,uint-decoder buffer start)))
;;          (declare (type (unsigned-byte ,bits) u))
;;          (let ((s (if (logbitp ,(1- bits) u)
;;                       (- u ,(ash 1 bits))
;;                       u)))
;;            (declare (type (signed-byte ,bits) s))
;;            s)))))

;; (def-uint-decoder decode-uint-32-le 32 :little)
;; (def-uint-decoder decode-uint-64-le 64 :little)
;; (def-uint-decoder decode-uint-32-be 32 :big)
;; (def-uint-decoder decode-uint-64-be 64 :big)

;; (def-sint-decoder decode-sint-32-le 32 decode-uint-32-le)
;; (def-sint-decoder decode-sint-64-le 64 decode-uint-64-le)
;; (def-sint-decoder decode-sint-32-be 32 decode-uint-32-be)
;; (def-sint-decoder decode-sint-64-be 64 decode-uint-64-be)


;;;;;;;;;;;;;;;;;;;;
;;; varint types ;;;
;;;;;;;;;;;;;;;;;;;;

;; arbitrary precision zig-zagging

(declaim (ftype (function (integer) *) varint-zigzag))

(defun varint-zigzag (value)
  (- (* 2 (abs value))
     (* (signum value)
        (ash (1- (signum value)) -1))))

(declaim (ftype (function (integer) *) varint-unzigzag))

(defun varint-unzigzag (value)
  (let ((lowbit (ldb (byte 1 0) value)))
    (* (ash (+ value lowbit) -1)
       (- 1 (* 2 lowbit)))))

;; i don't know how to do this to arbitrary precision for negative
;; numbers.  The google implemention gives uint32_t and uint64_t.
;; Let's be unsigned.

(declaim (inline uvarint-size)
	 (ftype (function (non-negative-integer) *) uvariant-size))

(defun uvarint-size (value)
  (max 1 (ceiling (integer-length value) 7)))

(declaim (inline svarint-size)
	 (ftype (function (non-negative-integer) *) svariant-size))

(defun svarint-size (value)
  (uvarint-size (varint-zigzag value)))

(declaim (ftype (function (non-negative-integer
			   &optional
			   octet-vector
			   fixnum) *) encode-uvarint))

(defun encode-uvarint (value
		       &optional
                       (buffer (make-octet-vector (uvarint-size value)))
                       (start  0))
  (loop
     for v = value then (ash v -7)
     for v-next = (ash v -7)
     for j from 0 below most-positive-fixnum
     for i = (+ start j)
     until (or (and (zerop v) (> j 0))
               ;; cut out negative handling.
	       ;;(and (< value 0)
	       ;;(= j 10) ;; i guess we'll use google's arbitrary limit...
                    ;;; fixup last element
	       ;;(setf (ldb (byte 1 7) (aref buffer (1- i)))
	       ;;0)))
               )
     do (progn
          (setf (aref buffer i)
                (logior (ldb (byte 7 0) v)
                        (if (zerop v-next) 0 (ash 1 7)))))
     finally (return (values (- i start) buffer))))

(declaim (ftype (function (octet-vector fixnum) *) decode-uvarint))

(defun decode-uvarint (buffer start)
  (loop
     for i from 1      ; octets read
     for j from start  ; position in buffer
     for k from 0 by 7 ; position in integer
     for octet = (aref buffer j)
     for piece = (ldb (byte 7 0) octet)
     for accum = piece then (dpb piece (byte 7 k) accum)
     when (not (logbitp 7 octet))
     return (values accum i)))

(declaim (ftype (function (t &optional octet-vector t) *) encode-svarint))

(defun encode-svarint (value
		       &optional
                       (buffer (make-octet-vector (svarint-size value)))
                       (start  0))
  (encode-uvarint (varint-zigzag value) buffer start))

(defun decode-svarint (buffer start)
  (declare (type octet-vector buffer))
  (multiple-value-bind (uv i)
      (decode-uvarint buffer start)
    (values (varint-unzigzag uv) i)))

(defun read-file-octets (filespec &key limit)
  (with-open-file (stream filespec :element-type 'octet)
    (let ((buffer (make-octet-vector
		   (if limit
		       (max (file-length stream) limit)
		       (file-length stream)))))
      (read-sequence buffer stream)
      buffer)))

;;;;;;;;;;;;;;;
;;; strings ;;;
;;;;;;;;;;;;;;;

#-sbcl
(defun encode-utf8 (string
                         &key
                         (string-start 0) (string-end (length string))
                         buffer (buffer-start 0))
  (let ((buffer (or buffer (make-octet-vector (- string-end string-start)))))
    (loop
       for i-b from buffer-start
       for i-s from string-start below string-end
       do (setf (aref buffer i-b)
                (char-code (aref string i-s))))
    (values (- string-end string-start)
            buffer)))

#-sbcl
(defun decode-utf8 (buffer &key
                         (buffer-start 0) (buffer-end (length buffer))
                         (string-start 0)
                         (string (make-string (+ string-start
                                                 (- buffer-end buffer-start)))))
  (loop
     for i-s from string-start
     for i-b from buffer-start below buffer-end
     do (setf (aref string i-s)
              (code-char (aref buffer i-b))))
  (values string
          (- buffer-end buffer-start)))


#+sbcl
(defun encode-utf8 (string
                    &key
                    (string-start 0)
		    (string-end   (length string))
                    buffer
		    (buffer-start 0))
  (let ((octets (sb-ext:string-to-octets string
                                         :start string-start
                                         :end   string-end)))
    (values (length octets)
            (if buffer
                (replace buffer octets :start1 buffer-start)
                octets))))



#+sbcl
(defun decode-utf8 (buffer &key
                    (string-start 0) string
                    (buffer-start 0) (buffer-end (length buffer)))
  (let ((str (sb-ext:octets-to-string buffer
                                      :start           buffer-start
                                      :end             buffer-end
                                      :external-format :utf8)))
    (values (if string
               (replace string str :start1 string-start)
               str)
            (- buffer-end buffer-start))))

(defun utf8-size (string)
  (multiple-value-bind (size buffer)
      (encode-utf8 string)
    (declare (ignore buffer))
    size))
