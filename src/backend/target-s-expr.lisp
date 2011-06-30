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

(defmethod emit ((node t) (target target-s-expr)
		 &key)
  node)

(defmethod emit ((node   file-set-desc)
		 (target target-s-expr)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node file-set-desc)
      (apply #'nconc (map 'list #'recur file)))))

(defmethod emit ((node   file-desc)
		 (target target-s-expr)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node file-desc)
      (nconc (map 'list #'recur enum-type)
	     (map 'list #'recur message-type)))))

(defmethod emit ((node   message-desc)
		 (target target-s-expr)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (bind ((name1 (intern* (make-lisp-class-name name))))
	`(:message ,name1
		   ,@(map 'list #'recur enum-type)
		   ,@(map 'list #'recur nested-type)
		   ,@(map 'list #'recur field))))))

(defmethod emit ((node   field-desc)
		 (target target-s-expr)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node field-desc)
      (bind ((name1     (intern* (make-lisp-slot-name name)))
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
		 ,@(when packed? '(:packed t)))))))

(defmethod emit ((node   enum-desc)
		 (target target-s-expr)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node enum-desc)
      (bind ((name1 (intern* (make-lisp-enum-name name))))
	`(:enum ,name1
		,@(map 'list #'recur value))))))

(defmethod emit ((node   enum-value-desc)
		 (target target-s-expr)
		 &key)
  (with-descriptor-fields (node enum-value-desc)
    (bind ((name1 (make-lisp-enum-value name)))
      `(,name1 ,number))))
