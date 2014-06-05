;;; descriptor-functions.lisp --- Extra methods on descriptor classes.
;;
;; Copyright (C) 2011, 2012, 2014 Jan Moringen
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

(in-package :protocol-buffer)


;;; Some useful methods for descriptor classes
;;

(defgeneric descriptor-name (descriptor)
  (:documentation
   "Return the name of DESCRIPTOR. The only purpose of this function
is providing a unified interface."))

(macrolet
    ((generate-descriptor-name (class accessor)
       `(defmethod descriptor-name ((descriptor ,class))
	  "Return the name of DESCRIPTOR."
	  (,accessor descriptor))))
  (generate-descriptor-name file-desc       file-desc-name)
  (generate-descriptor-name enum-desc       enum-desc-name)
  (generate-descriptor-name message-desc    message-desc-name)
  (generate-descriptor-name field-desc      field-desc-name)
  (generate-descriptor-name enum-value-desc enum-value-desc-name))

(defgeneric descriptor-qualified-name (descriptor)
  (:documentation
   "Return the qualified name of DESCRIPTOR. This is most useful for
message descriptor for which the qualified name is of the form
\".PACKAGE.MESSAGE\"."))

(defmethod descriptor-qualified-name ((descriptor file-desc))
  (file-desc-package descriptor))

(defvar *descriptor-qualified-name* (make-hash-table :test #'eq))

(defmethod descriptor-qualified-name ((descriptor t))
  (or (gethash descriptor *descriptor-qualified-name*)
      (error "~@<~A does not have a qualified name.~@:>" descriptor)))

(defgeneric descriptor-parent (descriptor)
  (:documentation
   "Return the parent descriptor of DESCRIPTOR. For example, the
parent descriptor of a field descriptor is the containing message
descriptor."))

(defmethod descriptor-parent ((descriptor file-set-desc))
  nil)

(defvar *descriptor-parent* (make-hash-table :test #'eq))

(defmethod descriptor-parent ((descriptor t))
  (or (gethash descriptor *descriptor-parent*)
      (error "~@<~A does not have a parent.~@:>" descriptor)))

(defgeneric descriptor-children (descriptor)
  (:documentation
   "Return a sequence of children of DESCRIPTOR. For example, the
children of a message descriptor are computed by concatenating the
nested enum descriptors and the nested message descriptors."))

(macrolet
    ((generate-descriptor-children (class &body accessors)
       `(defmethod descriptor-children ((descriptor ,class))
	  "Return the children of DESCRIPTOR."
	  (concatenate 'vector
		       ,@(iter (for accessor in accessors)
			       (collect `(,accessor descriptor)))))))
  (generate-descriptor-children file-set-desc
     file-set-desc-file)
  (generate-descriptor-children file-desc
     file-desc-enum-type file-desc-message-type)
  (generate-descriptor-children message-desc
     message-desc-enum-type message-desc-nested-type))

(defgeneric descriptor-file (descriptor)
  (:documentation
   "Return the file descriptor (`file-desc' object) in which the
message, enum or field descriptor DESCRIPTOR is defined."))

(defmethod descriptor-file ((descriptor file-desc))
  descriptor)

(defmethod descriptor-file ((descriptor t))
  (descriptor-file (descriptor-parent descriptor)))

(defgeneric descriptor-class (descriptor)
  (:documentation
   "Return the class that has been generated based on DESCRIPTOR or
nil if there is no such class."))

(defvar *descriptor-class* (make-hash-table :test #'eq))

(defmethod descriptor-class ((descriptor t))
  (or (gethash descriptor *descriptor-class*)
      (error "~@<No class for descriptor ~A.~@:>" descriptor)))


;;; `print-object' methods
;;

(defmethod print-object ((object file-set-desc) stream)
  (bind (((:accessors-r/o
	   (files file-set-desc-file)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~D)" (length files)))))

(defmethod print-object ((object file-desc) stream)
  (bind (((:accessors-r/o
	   (name     file-desc-name)
	   (messages file-desc-message-type)
	   (enums    file-desc-enum-type)
	   (services file-desc-service)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S M(~2D) E(~2D) S(~2D)"
	      name (length messages) (length enums) (length services)))))

(defmethod print-object ((object message-desc) stream)
  (bind (((:accessors-r/o
	   (name   message-desc-name)
	   (nested message-desc-nested-type)
	   (enums  message-desc-enum-type)
	   (fields message-desc-field)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S N(~2D) E(~2D) F(~2D)"
	      name (length nested) (length enums) (length fields)))))

(defmethod print-object ((object field-desc) stream)
  (bind (((:accessors-r/o
	   (number        field-desc-number)
	   (name          field-desc-name)
	   (type          field-desc-type)
	   (label         field-desc-label)
	   (default-value field-desc-default-value)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~D ~S ~A ~A~:[~; [~A]~]"
	      number name type label
	      (and default-value (not (emptyp default-value))) ;; TODO avoid string stuff
	      default-value))))

(defmethod print-object ((object enum-desc) stream)
  (bind (((:accessors-r/o
	   (name   enum-desc-name)
	   (values enum-desc-value)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S (~D)" name (length values)))))

(defmethod print-object ((object enum-value-desc) stream)
  (bind (((:accessors-r/o
	   (name   enum-value-desc-name)
	   (number enum-value-desc-number)) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S ~D" name number))))


;;; Field descriptors
;;

(defun field-primitive? (field-desc)
  "Return non-nil if the type of FIELD-DESC is a primitive type."
  (primitive-type-p (field-desc-type field-desc)))

(defun field-enum? (field-desc)
  "Return non-nil if the type of FIELD-DESC is an enum type."
  (eq (field-desc-type field-desc) :enum))

(defun field-message? (field-desc)
  "Return non-nil if the type of FIELD-DESC is a message type."
  (eq (field-desc-type field-desc) :message))

(defun field-repeated? (field-desc)
  "Return non-nil if FIELD-DESC describes a repeated field."
  (eq (field-desc-label field-desc) :repeated))

(defun field-packed? (field-desc)
  "Return non-nil if FIELD-DESC describes a repeated field."
  (field-options-packed (field-desc-options field-desc)))

(defun field-optional? (field-desc)
  "Return non-nil if FIELD-DESC describes an optional field."
  (eq (field-desc-label field-desc) :optional))

(defgeneric field-type-descriptor (descriptor)
  (:documentation
   "Return the descriptor instance that describes the type of
DESCRIPTOR. DESCRIPTOR has to be a field descriptor."))

(defvar *field-type-descriptor* (make-hash-table :test #'eq))

(defmethod field-type-descriptor ((descriptor t))
  (gethash descriptor *field-type-descriptor*))


;;; Finding descriptors
;;

(defgeneric find-descriptor (qualified-name
			     &key
			     error?)
  (:documentation
   "Find and return the protocol buffer descriptor whose qualified
name is QUALIFIED-NAME. When ERROR? is non-nil, signal an error if no
such descriptor can be found, otherwise return nil."))

(defmethod find-descriptor ((qualified-name string)
			    &key
			    (error? t))
  "Convert QUALIFIED-NAME into keyword."
  (find-descriptor (make-keyword qualified-name) :error? error?))

(defvar *find-descriptor* (make-hash-table :test #'eq))

(defmethod find-descriptor ((name symbol) &key error?)
  (or (gethash name *find-descriptor*)
      (when error?
        (error "No such descriptor: ~A" name))))

(defgeneric find-package1 (name
			   &key
			   error?)
  (:documentation
   "Find and return file set descriptor containing file descriptors
whose package name is NAME. When ERROR? is non-nil, signal an error if
no such descriptor can be found, otherwise return nil."))

(defmethod find-package1 ((name string)
			  &key
			  (error? t))
  "Converter NAME into keyword."
  (find-package1 (make-keyword name) :error? error?))

(defvar *find-package1* (make-hash-table :test #'eq))

(defmethod find-package1 ((name symbol) &key error?)
  (or (gethash name *find-package1*)
      (when error?
        (error "No such package: ~A" name))))


;;; High-level utilities
;;

(defun dependency-closure (descriptor)
  "Return a `file-set-desc' object which contains files (`file-desc'
objects) on which DESCRIPTOR depends. The set of files is determined
as the transitive closure with respect to field-type dependencies."
  (bind ((result (make-instance 'file-set-desc))
	 ((:accessors-r/o (files file-set-desc-file)) result)
	 ((:flet do-file (file))
	  (unless (find file files :test #'eq)
	    (vector-push-extend file files)))
	 (seen (make-hash-table :test #'eq))
	 ((:labels do-descriptor (descriptor))
	  (when (gethash descriptor seen)
	    (return-from do-descriptor))
	  (setf (gethash descriptor seen) t)
	  (do-file (descriptor-file descriptor))
	  (map 'nil (compose #'do-descriptor #'field-type-descriptor)
	       (remove-if (complement #'field-message?)
			  (message-desc-field descriptor)))))
    (do-descriptor descriptor)
    result))
