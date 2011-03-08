;;; generator-class.lisp ---
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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


;;; Enum Generation
;;

(defun generate-enum (name values)
  "Generate code to map between enum codes and keyword symbols."
  (setf (get name 'pb::enum) t)
  (bind ((enum-symbol (pb::symcat name 'symbol))
	 (enum-code   (pb::symcat name 'code)))
    (with-unique-names (sym code buffer start value length)
      `((deftype ,name ()
	  '(or null (member ,@(mapcar #'first values))))
	(defun ,enum-symbol (,code)
	  (case ,code
	    ,@(iter (for (symbol code) in values) (collect `(,code ,symbol)))
	    (t (error "Unknown enum ~A code: ~A" ',name ,code))))
	(defun ,(pb::symcat name 'code) (,sym)
	  (case ,sym
	    ,@(iter (for (symbol code) in values) (collect `(,symbol ,code)))
	    (t (error "Unknown enum ~A symbol: ~A" ',name ,sym))))
	(defun ,(pb::symcat name 'encode) (,value ,buffer ,start)
	  (binio:encode-uvarint (,enum-code ,value) ,buffer ,start))
	(defun ,(pb::symcat name 'decode) (,buffer ,start)
	  (pb::with-decoding (,value ,length)
	      (binio:decode-uvarint ,buffer ,start)
	    (values (,enum-symbol ,value) ,length)))))))


;;; Class Generation
;;

(defun generate-initform (type repeated? packed?) ;; TODO we do not need label; only repeated?
  "Generate an initform for a slot of type TYPE."
  (cond
   ;; Scalar types
   ((and (not repeated?) (not packed?))
    (cond
     ((member type '(:bool :boolean)) nil)
     ((eq type :double)               0d0)
     ((eq type :float)                0s0)
     ((enum-type-p type)              nil) ;; has be check before integer-type-p
     ((integer-type-p type)           0)
     ((eq type :string)               "") ;; TODO
     (t                               nil)))

   ;; Packed array
   ((and repeated? packed?)
    nil)

   ;; Not packed array
   ((and repeated? (not packed?))
    `(make-array 0
		 :element-type ',(proto-type->lisp-type type)
		 :fill-pointer t
		 :adjustable   t))

   ;; Packed, not repeated
   (t
    (error "Can't make initform for packed, non-repeated elements."))))

(defun generate-slot (name type label packed?
		      &key
		      class-name)
  (let ((repeated? (eq label :repeated))
	(optional? (eq label :optional)))
    `(,name
      :initarg  ,(pb::symbol->keyword name)
      :type     ,(proto-type->lisp-type type repeated? optional?)
      ,@(when class-name
	      `(:accessor ,(%make-lisp-accessor-name class-name name)))
      :initform ,(generate-initform type repeated? packed?)))) ;; TODO use default value

(defun generate-class (name fields &optional doc)
  "Generate a class definition for a class named NAME with slots
specified by SPECS."
  ;; Generate the class twice: once without any slots and a second
  ;; time with slots which may refer to the generated class itself.
  `((cl:defclass ,name () ())
    (cl:defclass ,name ()
      ,(mapcar #'funcall fields)
      ,@(when doc
	      `((:documentation ,doc))))))


;;; Utility functions
;;

;; TODO this is also in util.lisp, but we don't have util.lisp in backend-early
(defun %make-lisp-accessor-name (class-name slot-name)
  (intern (concatenate 'string
		       (string class-name)
		       "-"
		       (string slot-name))
	  (symbol-package class-name)))
