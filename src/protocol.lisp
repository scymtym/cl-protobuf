;;; protocol.lisp --- Protocol buffer protocol.
;;
;; Copyright (C) 2008 Google Inc.
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


;;; Message protocol
;;

(defgeneric message-descriptor (object)
  (:documentation
   "Return the protocol buffer descriptor according to which OBJECT
was generated."))


;;; Protocol buffer protocol ;)
;;

(defgeneric packed-size (object)
  (:documentation
   "Compute the number of octets that are required to represent OBJECT
in the protocol buffer wire format."))

(defgeneric pack (object &optional buffer start)
  (:documentation
   "Convert OBJECT into protocol buffer wire format and store it in
BUFFER at start offset START. If BUFFER is not supplied, a new octet
vector of appropriate size is allocated. Two values are returned: the
number of emitted octets and the buffer."))

(defun pack1 (object)
  (multiple-value-bind (size buffer)
      (pack object)
    (declare (ignore size))
    buffer))

(defgeneric unpack (buffer object &optional start end)
  (:documentation
   "Decode the protocol buffer wire format representation in BUFFER
into OBJECT. When specified only use the portion from START to END of
BUFFER. Two values are returned: the modified OBJECT and the number of
consumed octets."))

(defgeneric offset (buffer message field &optional start end)
  (:documentation
   "Find and return the offset in BUFFER at which the first instance
of the FIELD of the protocol buffer message MESSAGE occurs.
When supplied, START and END restrict the search to a range within
BUFFER."))


;;; Default behavior
;;

(defmethod unpack ((buffer t) (object class) &optional (start 0) end)
  "Instantiate the class OBJECT and load BUFFER into the instance."
  (let ((instance (make-instance object)))
    (apply #'unpack buffer instance start
	   (when end (list end)))))

(defmethod unpack ((buffer t) (object symbol) &optional (start 0) end)
  "Instantiate the class designated by OBJECT and load BUFFER into the
instance."
  (unpack buffer (find-class object) start end))

(defmethod unpack ((source stream) (object t) &optional (start 0) end)
  "Unpack OBJECT from stream SOURCE."
  (unless (zerop start)
    (iter (repeat start) (read-byte source)))

  (bind (((:flet read-whole-file ())
	  (let ((buffer (make-array 0
				    :element-type '(unsigned-byte 8)
				    :fill-pointer 0)))
	    (iter (for c in-stream source :using #'read-byte)
		  (vector-push-extend c buffer))
	    (coerce buffer '(simple-array (unsigned-byte 8) (*)))))
	 ((:flet read-range ())
	  (let ((buffer (make-array (- end start)
				    :element-type '(unsigned-byte 8))))
	    (read-sequence buffer source)
	    buffer))
	 (buffer (if end (read-range) (read-whole-file))))
    (unpack buffer object)))

(defmethod unpack ((source pathname) (object t) &optional (start 0) end)
  "Open a stream for SOURCE and, potentially seek to START, then
unpack the contents into OBJECT."
  (with-input-from-file (stream source
				:element-type '(unsigned-byte 8))
    (unless (zerop start)
      (file-position stream start))
    (unpack stream object 0 (- (or end (file-length stream)) start))))

(defmethod unpack ((source string) (object t) &optional (start 0) end)
  "Open the file designated by SOURCE, potentially seek to START, then
unpack the contents into OBJECT."
  (unpack (pathname source) object start end))


;;; Enum translation protocol
;;

(defgeneric enum-symbol (name code)
  (:documentation
   "Map the number CODE to its corresponding symbol in the enum
designated by NAME."))

(defgeneric enum-code (name symbol)
  (:documentation
   "Map SYMBOL to its corresponding numeric value in the enum
designated by NAME."))
