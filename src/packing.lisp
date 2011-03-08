;;; packing.lisp ---
;;
;; Copyright (C) 2009 Georgia Tech Research Corporation
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

(in-package :protocol-buffer)

;; encoders return (values bytes-encoded buffer)
;; decoders return (values value bytes-decoded)

;; All decoding functions return (values object bytes-decoded)

;; make it obvious what we're doing
(defmacro with-decoding ((value length) decode-expr &body body)
  `(multiple-value-bind (,value ,length)
       ,decode-expr
     (declare (non-negative-fixnum ,length))
     ,@body))


;;; Start codes
;;

(declaim (ftype (function ((member 0 1 2 5))
			  (member :varint :fixed64 :fixed32 :size-delimited))
		wire-type-meaning)
	 (inline wire-type-meaning))

(defun wire-type-meaning (type)
  (ecase type
    (0 :varint)
    (1 :fixed64)
    (5 :fixed32)
    (2 :size-delimited)))

(declaim (ftype (function (non-negative-fixnum non-negative-fixnum)
			  non-negative-fixnum)
		make-start-code)
	 (inline make-start-code))

(defun make-start-code (field-number wire-type)
  (logior (ash field-number 3) wire-type))

(declaim (ftype (function (octet-vector non-negative-fixnum)
			  (values non-negative-fixnum non-negative-fixnum non-negative-fixnum))
		read-start-code)
	 (inline read-start-code))

(defun read-start-code (buffer start)
  "Read a start code from BUFFER at position START.
Return three values: position, wire-type and the number of bytes
read."
  (with-decoding (number-and-wire-type length)
      (decode-uvarint buffer start)
    (declare (type non-negative-fixnum number-and-wire-type length))
    (values
     (ash number-and-wire-type -3)
     (ldb (byte 3 0) number-and-wire-type)
     length)))

(declaim (ftype (function (non-negative-fixnum (member 0 1 2 5) octet-vector non-negative-fixnum)
			  (values non-negative-fixnum octet-vector))
		encode-start-code)
	 (inline encode-start-code))

(defun encode-start-code (field-number wire-type buffer start)
  (binio:encode-uvarint (make-start-code field-number wire-type)
			buffer start))


;;; Size computations for:
;; + variable-width packed arrays
;; + length-delimited values
;;

(declaim (ftype (function ((simple-array integer (*)))
			  non-negative-integer)
		packed-uvariant-size))

(defun packed-uvarint-size (array)
  (loop for x across array
	summing (binio:uvarint-size x)))

(declaim (ftype (function ((simple-array integer (*)))
			  non-negative-integer)
		packed-svariant-size))

(defun packed-svarint-size (array)
  (loop for x across array
	summing (binio:svarint-size x)))

(declaim (ftype (function (function t) non-negative-integer)
		packed-enum-size))

(defun packed-enum-size (coder array)
  (loop for x across array
	summing (binio:uvarint-size (funcall coder x))))

(declaim (ftype (function (fixnum) non-negative-integer)
		length-delim-size))

(defun length-delim-size (length)
  (+ (binio:uvarint-size length) length))

(declaim (ftype (function (t octet-vector fixnum) bit) encode-bool))

(defun encode-bool (val buffer start)
  (setf (aref buffer start) (if val 1 0))
  1)


;;; Fixed-width decoders
;;

(declaim (inline decode-uint32
                 decode-sint32
                 decode-uint64
                 decode-sint64
                 decode-double
                 decode-bool))

(defun decode-uint32 (buffer start)
  (values (binio:decode-uint32-le buffer start)
          4))

(defun decode-sint32 (buffer start)
  (values (binio:decode-sint32-le buffer start)
          4))

(defun decode-uint64 (buffer start)
  (values (binio:decode-uint64-le buffer start)
          8))

(defun decode-sint64 (buffer start)
  (values (binio:decode-sint64-le buffer start)
          8))

(defun decode-double (buffer start)
  (values (binio:decode-double-float-le buffer start)
					;(binio:decode-double-float buffer :little start)
          8))


(defun decode-single (buffer start)
  (values (binio:decode-float-le buffer start)
          4))

(declaim (ftype (function (octet-vector fixnum) *) decode-bool))

(defun decode-bool (buffer start)
  (values (case (aref buffer start)
            (0 nil)
            (1 t)
            (t (error "Invalid boolean value")))
          1))

(defmacro decode-length-and-incf-start (start-place buffer)
  "reads the length field,
increments start-place by length,
returns (values length length-of-length)"
  (with-unique-names (isym len-sym len-len-sym)
    `(let ((,isym ,start-place))
       (with-decoding (,len-sym ,len-len-sym)
	   (binio:decode-uvarint ,buffer ,isym)
	 (declare (type non-negative-integer ,len-sym))
         (setf ,start-place (+ ,isym ,len-len-sym))
         (values ,len-sym ,len-len-sym)))))

(declaim (ftype (function (octet-vector non-negative-integer function)
			  (values t non-negative-integer))
		decode-length-delim))

(defun decode-length-delim (buffer start decoder)
  "decoder is (lambda (buffer start end)"
  (let ((i start))
    (with-decoding (len len-len)
	(decode-length-and-incf-start i buffer)
      (declare (type non-negative-integer len))
      (with-decoding (val val-len)
	  (funcall decoder buffer i (+ i len))
        (assert (= val-len len))
        (values val (+ len len-len))))))

(declaim (ftype (function (octet-vector integer) *)
		decode-string))

(defun decode-string (buffer start)
  (decode-length-delim buffer start
                       #'(lambda (buffer start end)
			   (binio::decode-utf8 buffer
					       :buffer-start start
					       :buffer-end end))))

(declaim (ftype (function (octet-vector integer) *)
		decode-bytes))

(defun decode-bytes (buffer start)
  (decode-length-delim buffer start
		       #'(lambda (buffer start end)
			   (declare (type octet-vector         buffer)
				    (type non-negative-integer start end))
			   (values (subseq buffer start end)
				   (- end start)))))

(declaim (ftype (function (t function octet-vector
			   &key
			   (:fixed-bit-size (or null positive-fixnum))
			   (:start          non-negative-integer)
			   (:end            non-negative-integer)) *)
		decode-array))

(defun decode-array (type decoder buffer
		     &key
                     (fixed-bit-size nil)
                     (start          0)
                     (end            (length buffer)))
  (assert (or (not fixed-bit-size)
              (zerop (rem fixed-bit-size 8))) ()
              "Can only decode integral-octet-sized types")

  (let ((array (make-array (if fixed-bit-size
			       (/ (- end start)
				  (/ fixed-bit-size 8))
			       0)
			   :element-type type
			   :adjustable   (not fixed-bit-size)
			   :fill-pointer (not fixed-bit-size))))
    (do ((i start)
	 (j 0 (1+ j)))
	((>= i end) (values array (- i start)))
      (multiple-value-bind (value length)
	  (funcall decoder buffer i)
	(incf i (if fixed-bit-size (/ fixed-bit-size 8) length))
	(if fixed-bit-size
	    (setf (aref array j) value)
	    (vector-push-extend value array))))))
;; TODO separate functions


;;; En- and decoding of embedded protocol buffers
;;

(declaim (ftype (function (t octet-vector non-negative-fixnum)
			  (values non-negative-fixnum octet-vector))
		pack-embedded)
	 (inline pack-embedded))

(defun pack-embedded (object buffer start)
  (let* ((size         (packed-size object))
         (size-size    (binio:encode-uvarint size buffer start))
         (encoded-size (pack object buffer (+ start size-size))))
    (declare (type non-negative-fixnum size size-size encoded-size))
    (assert (= size encoded-size))
    (values (+ size-size encoded-size) buffer)))

(declaim (ftype (function (octet-vector t non-negative-integer)
			  (values t non-negative-integer))
		unpack-embedded)
	 (inline unpack-embedded))

(defun unpack-embedded (buffer object start)
  (decode-length-delim buffer start
                       #'(lambda (buffer start end)
			   (declare (type octet-vector        buffer)
				    (type non-negative-fixnum start end))
			   (unpack buffer object start end))))
