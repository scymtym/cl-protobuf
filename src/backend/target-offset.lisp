;;; target-offset.lisp --- Generate methods for partial unpacking/inspection.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :protocol-buffer.backend)


;;; Offset
;;

(defmethod documentation ((thing (eql :offset)) (type (eql 'target)))
  "Emit an `offset' method which determines the offset of a protocol
buffer message field within the packed representation of the
containing protocol buffer message.")

(defclass target-offset (code-generating-target-mixin)
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
      (bind ((name1 (intern* (make-lisp-slot-name name)))
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

(defclass target-extractor (code-generating-target-mixin)
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
      (bind ((name1 (intern* (make-lisp-slot-name name)))
	     (type1 (make-lisp-slot-type type type-name package))
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
		 (do-fields ((buffer start end offset number wire-type))
		   ;; If we are at the desired field, extract and
		   ;; return the value.
		   (when (= number ,number)
		     (let (result)
		       ,@(generate-unpack type1 'buffer 'offset 'result)
		       (return result)))
		   ;; If we are at some other field, skip it.
		   (incf offset (pb::packed-field-size wire-type buffer offset))))))
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
