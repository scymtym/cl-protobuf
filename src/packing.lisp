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
Return three values: field number, wire-type and the number of bytes
read."
  (with-decoding (number-and-wire-type length)
      (decode-uvarint buffer start)
    (declare (type non-negative-fixnum number-and-wire-type length))
    (let ((number    (ash number-and-wire-type -3))
	  (wire-type (ldb (byte 3 0) number-and-wire-type)))
      (unless (typep wire-type 'wire-type)
	(error 'invalid-wire-type
	       :offset     start
	       :designator wire-type))
      (values number wire-type length))))

(declaim (ftype (function (non-negative-fixnum wire-type octet-vector non-negative-fixnum)
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


;;; Decoding functions
;;

(defmacro decode-length-and-incf-start (start-place buffer)
  "Decodes the length information stored at START-PLACE in BUFFER and
increments START-PLACE by that length. Returns two values: the decoded
length and the length of its encoded representation."
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
  "Apply DECODER to the length-delimited range at START in BUFFER.
DECODER has to be a function of three arguments: a buffer, a range
start and a range end position."
  (let ((offset start))
    (with-decoding (len len-len)
	(decode-length-and-incf-start offset buffer)
      (declare (type non-negative-integer len))
      (with-decoding (val val-len)
	  (funcall decoder buffer offset (+ offset len))
        (assert (= val-len len))
        (values val (+ len len-len))))))

(declaim (ftype (function (string octet-vector non-negative-fixnum)
			  (values non-negative-fixnum octet-vector))
		encode-string))

(defun encode-string (value buffer start)
  (bind ((offset start)
	 ((:values temp-size temp-buffer)
	  (binio:encode-utf8 value)))
    (incf offset (binio:encode-uvarint temp-size buffer start))
    (replace buffer temp-buffer
	     :start1 offset
	     :end1   (incf offset temp-size))
    (values (- offset start) buffer)))

(declaim (ftype (function (octet-vector non-negative-fixnum)
			  (values string non-negative-fixnum))
		decode-string))

(defun decode-string (buffer start)
  (decode-length-delim buffer start
                       #'(lambda (buffer start end)
			   (declare (type octet-vector         buffer)
				    (type non-negative-integer start end))
			   (binio:decode-utf8 buffer start end))))

(declaim (ftype (function ((array (unsigned-byte 8) (*)) octet-vector non-negative-fixnum)
			  (values non-negative-fixnum octet-vector))
		encode-bytes))

(defun encode-bytes (value buffer start)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((value-size (length value))
	(offset     start))
    (declare (type non-negative-fixnum offset))
    (incf offset (binio:encode-uvarint value-size buffer start))
    (if (typep value 'octet-vector)
	(replace buffer value
		 :start1 offset
		 :end1   (incf offset value-size))
	(replace buffer value
		 :start1 offset
		 :end1   (incf offset value-size)))
    (values (- offset start) buffer)))

(declaim (ftype (function (octet-vector non-negative-fixnum)
			  (values (array (unsigned-byte 8) (*)) non-negative-fixnum))
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


;;; Size of packed field values
;;

#+seems-to-be-wrong
(declaim (ftype (function (wire-type octet-vector non-negative-fixnum)
			  (values non-negative-fixnum
				  &optional non-negative-fixnum))
		packed-field-size)
	 (inline packed-field-size))

(defun packed-field-size (wire-type buffer start)
  "Return the size of the field at START in BUFFER which should be
considered to be of the type designated by WIRE-TYPE."
  (ecase (wire-type-meaning wire-type)
    (:varint
     (nth-value 1 (binio:decode-uvarint buffer start)))
    (:fixed32
     4)
    (:fixed64
     8)
    (:size-delimited
     (multiple-value-bind (value-length length-length)
	 (binio:decode-uvarint buffer start)
       (declare (type non-negative-fixnum value-length length-length))
       (values (+ value-length length-length) length-length)))))
