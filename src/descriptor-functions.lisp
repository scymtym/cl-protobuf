;;; descriptor-functions.lisp --- Extra methods on descriptor classes.
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

(defgeneric descriptor-parent (descriptor)
  (:documentation
   "Return the parent descriptor of DESCRIPTOR. For example, the
parent descriptor of a field descriptor is the containing message
descriptor."))

(defgeneric descriptor-class (descriptor)
  (:documentation
   "Return the class that has been generated based on DESCRIPTOR or
nil if there is no such class."))

(defmethod no-applicable-method
    ((generic-function (eql (fdefinition 'descriptor-class)))
     &rest args)
  "Return nil, if there is no class for the descriptor."
  (declare (ignore args))
  nil)


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
  (not (primitive-type-p (field-desc-type field-desc))))

(defun field-message? (field-desc)
  "Return non-nil if the type of FIELD-DESC is a message type."
  (not (primitive-type-p (field-desc-type field-desc))))

(defun field-repeated? (field-desc)
  "Return non-nil if FIELD-DESC describes a repeated field."
  (eq (field-desc-label field-desc) :repeated))

(defun field-optional? (field-desc)
  "Return non-nil if FIELD-DESC describes an optional field."
  (eq (field-desc-label field-desc) :optional))


;;; Finding descriptors
;;

(defgeneric find-descriptor (qualified-name
			     &key
			     error?)
  (:documentation
   "Find and return the protocol buffer descriptor whose qualified
name is QUALIFIED-NAME. When ERROR? is non-nil, signal an error if no
such descriptor can be found, otherwise return nil."))

(defmethod no-applicable-method
    ((generic-function (eql (fdefinition 'find-descriptor)))
     &rest args)
  "The specified name did not designate a protocol buffer
descriptor. Signal an error or return nil."
  (bind (((qualified-name &key error?) args))
    (when error?
      (error "No such descriptor: ~A" qualified-name))))

(defmethod find-descriptor ((qualified-name string)
			    &key
			    (error? t))
  "Convert QUALIFIED-NAME into keyword."
  (find-descriptor (make-keyword qualified-name) :error? error?))
