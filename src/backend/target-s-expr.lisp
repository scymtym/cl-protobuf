;;; target-s-expr.lisp --- Emit S-Expression Syntax.
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

(in-package :protocol-buffer.backend)


;;;
;;

(defmethod documentation ((thing (eql :s-expr)) (type (eql 'target)))
  "Convert descriptor instances into s-expression-based
representation.")


;;; Target class
;;

(defclass target-s-expr (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for conversion to s-expr syntax."))


;;; Emit methods
;;

(defmethod emit ((node t) (target target-s-expr))
  node)

(defmethod emit ((node   pb::file-set-desc)
		 (target target-s-expr))
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (file pb::file-set-desc-file)) node))
      (apply #'nconc (map 'list #'recur file)))))

(defmethod emit ((node   pb::file-desc)
		 (target target-s-expr))
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (enums    pb::file-desc-enum-type)
	     (messages pb::file-desc-message-type)) node))
      (nconc (map 'list #'recur enums)
	     (map 'list #'recur messages)))))

(defmethod emit ((node   pb::message-desc)
		 (target target-s-expr))
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (nested pb::message-desc-nested-type)
	     (enums  pb::message-desc-enum-type)
	     (fields pb::message-desc-field)) node)
	   (name1 (intern* (make-lisp-class-name name))))
      `(:message ,name1
		 ,@(map 'list #'recur enums)
		 ,@(map 'list #'recur nested)
		 ,@(map 'list #'recur fields)))))

(defmethod emit ((node   pb::field-desc)
		 (target target-s-expr))
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (number    pb::field-desc-number)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1     (intern* (make-lisp-slot-name name)))
	   (type1     (if (member type '(:message :enum))
			  (pb::proto-type-name->lisp-type-symbol
			   type-name :package package)
			  type))
	   (repeated? (eq label :repeated))
	   (packed?   (when options
			(pb::field-options-packed options))))
      (when (and packed? (not repeated?))
	(error "Can't have packed, nonrepeated field"))

      `(:field ,name1
	       ,type1
	       ,number
	       ,@(when label   `(,label t))
	       ,@(when packed? '(:packed t))))))

(defmethod emit ((node   pb::enum-desc)
		 (target target-s-expr))
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::enum-desc-name)
	     (values pb::enum-desc-value)) node)
	   (name1 (intern* (make-lisp-enum-name name))))
      `(:enum ,name1
	      ,@(map 'list #'recur values)))))

(defmethod emit ((node   pb::enum-value-desc)
		 (target target-s-expr))
  (bind (((:accessors-r/o
	   (name   pb::enum-value-desc-name)
	   (number pb::enum-value-desc-number)) node)
	 (name1 (make-lisp-enum-value name)))
    `(,name1 ,number)))
