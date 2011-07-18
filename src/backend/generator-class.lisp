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
	    (t (error "Unknown enum ~A code: ~A" ',name ,code)))) ;; TODO proper condition
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

(defun generate-initform (type repeated? packed?
			  &key
			  (default nil default-supplied?))
  "Generate an initform for a slot of type TYPE."
  (cond
   ;; Scalar types
   ((not repeated?)
    (cond
     ((eq type :bool)
      (if default-supplied?
	  (cond
	    ((string= default "true")  t)
	    ((string= default "false") nil)
	    (t                         (error "invalid default"))) ;; TODO
	   nil))
     ((eq type :double)
      (if default-supplied?
	  (coerce (read-from-string default) 'double-float)
	  0d0))
     ((eq type :float)
      (if default-supplied?
	  (coerce (read-from-string default) 'single-float)
	  0s0))
     ((enum-type-p type)
      (if default-supplied?
	  (make-lisp-enum-value default)
	  nil))
     ((integer-type-p type)
      (if default-supplied?
	  (read-from-string default)
	  0))
     ((eq type :string)
      (or default ""))
     ((eq type :bytes)
      (if default-supplied?
	  (sb-ext:string-to-octets default) ;; TODO this is not correct: from descriptor.proto: For bytes, contains the C escaped value.  All bytes >= 128 are escaped.
	  '(make-array 0 :element-type '(unsigned-byte 8))))
     ((find-class type)
      `(make-instance ',type))

     (t
      (error "Cannot generate initform for ~:[~; repeated~] ~:[~; ~
packed~] type ~S"
	     repeated? packed? type)))) ;; TODO can this happen? proper condition?

   ;; Not packed array
   ((not packed?)
    `(make-array 0
		 :element-type ',(proto-type->lisp-type type)
		 :fill-pointer t
		 :adjustable   t))

   ;; Packed array
   (t
    `(make-array 0
		 :element-type ',(proto-type->lisp-type type)
		 :fill-pointer nil
		 :adjustable   nil))))

(defun generate-slot (name type label packed?
		      &key
		      class-name
		      (default nil default-supplied?))
  (let ((repeated? (eq label :repeated))
	(optional? (eq label :optional)))
    `(,name
      :initarg  ,(make-keyword name)
      :type     ,(proto-type->lisp-type type repeated? optional?)
      ,@(when class-name
	      `(:accessor ,(%make-lisp-accessor-name class-name name)))
      :initform ,(apply #'generate-initform type repeated? packed?
			(when default-supplied?
			  (list :default default)))))) ;; TODO use default value

(defun generate-class (name fields &optional doc)
  "Generate a class definition for a class named NAME with slots
specified by SPECS."
  ;; Generate the class twice: once without any slots and a second
  ;; time with slots which may refer to the generated class itself.
  `((cl:defclass ,name () ())
    (cl:defclass ,name ()
      ,(map 'list #'funcall fields)
      ,@(when doc `((:documentation ,doc))))))


;;; Utility functions
;;

;; TODO this is also in util.lisp, but we don't have util.lisp in backend-early
(defun %make-lisp-accessor-name (class-name slot-name)
  (let ((*package* (symbol-package class-name)))
    (symbolicate class-name "-" slot-name)))
