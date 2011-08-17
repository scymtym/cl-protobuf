;;; target-offset.lisp --- Generate methods for partial unpacking/inspection.
;;
;; Copyright (C) 2011 Jan Moringen
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


;;; Offset
;;

(defmethod documentation ((thing (eql :offset)) (type (eql 'target)))
  "Emit an `offset' method which determines the offset of a protocol
buffer message field within the packed representation of the
containing protocol buffer message.")

(defclass target-offset (caching-target-mixin
			 code-generating-target-mixin)
  ()
  (:documentation
   "Target class for offset target."))

(defmethod emit ((node   message-desc)
		 (target target-offset)
		 &key)
  "Generate code for the `offset' method."
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (map 'nil #'recur nested-type)
      (reduce #'nconc (map 'list #'recur field)))))

(defmethod emit ((node   field-desc)
		 (target target-offset)
		 &key)
  "Generate code to find the offset of a single field."
  (with-emit-symbols
    (with-descriptor-fields (node field-desc)
      (bind ((name1 (emit node '(:lisp-name :nested? nil)))
	     ((:flet generate-offset-method (specializer))
	      `(defmethod offset ((buffer   simple-array)
				  (message  message-desc)
				  (field    ,specializer)
				  &optional
				  (start 0)
				  (end   (length buffer)))
		 (%offset ,number buffer start end))))
	(list (eval (generate-offset-method `(eql ',name1)))
	      (eval (generate-offset-method `(eql ,node))))))))


;;; Extractor
;;

(defmethod documentation ((thing (eql :extractor)) (type (eql 'target)))
  "Emit methods that unpack individual fields of packed protocol
buffer messages without unpacking the entire protocol buffer
message.")

(defclass target-extractor (caching-target-mixin
			    code-generating-target-mixin)
  ()
  (:documentation
   "Target class for extractor target."))

(defmethod emit ((node   message-desc)
		 (target target-extractor)
		 &key)
  "Generate code for the `extract' method."
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (map 'nil #'recur nested-type)
      (reduce #'nconc (map 'list #'recur field)))))

(defmethod emit ((node   field-desc)
		 (target target-extractor)
		 &key)
  "Generate code to extract the value of a single field."
  (with-emit-symbols
    (with-descriptor-fields (node field-desc)
      (bind ((name1     (emit node '(:lisp-name :nested? nil)))
	     (type1     (make-lisp-slot-type node))
	     (repeated? (field-repeated? node))
	     (packed?   (field-packed? node))
	     ((:flet generate-extract-method (specializer))
	      `(defmethod extract ((buffer  simple-array)
				   (message message-desc)
				   (field   ,specializer)
				   &optional
				   (start 0)
				   (end   (length buffer)))
		 (declare (type binio:octet-vector buffer)
			  (type non-negative-fixnum start end))

		 ;; Loop through BUFFER until we hit the field or the
		 ;; end of the buffer.
		 (let ((result ,@(when repeated?
				       `(,(generate-initform type1 repeated? packed?)))))
		  (do-fields ((buffer start end offset number wire-type))
		    ;; If we are at the desired field, extract and
		    ;; return the value.
		    (if (= number ,number)
			(progn
			  ,@(generate-unpack type1 'buffer 'offset 'result
					     :repeated? repeated?
					     :packed?   packed?)
			  ,@(unless repeated? `((return result))))
			;; If we are at some other field, skip it.
			(incf offset (pb::packed-field-size wire-type buffer offset))))
		  result))))
	(list (eval (generate-extract-method `(eql ,node)))
	      (eval (generate-extract-method `(eql ',name1))))))))


;;; Utility functions
;;

(declaim (ftype (function (non-negative-fixnum
			   binio:octet-vector
			   non-negative-fixnum
			   non-negative-fixnum)
			  (or null non-negative-fixnum))
		%offset))

(defun %offset (field-number buffer start end)
  "Loop through encoded fields in the range START to END in BUFFER
until a field with number FIELD-NUMBER is encountered. Return the
offset of that field."
  ;; Loop through BUFFER until we hit the field or the end of the
  ;; buffer.
  (do-fields ((buffer start end offset number wire-type))
    ;; If we are at the desired field, return the offset.
    (when (= number field-number)
      (return offset))
    ;; If we are at some other field, skip it.
    (incf offset (pb::packed-field-size wire-type buffer offset))))
