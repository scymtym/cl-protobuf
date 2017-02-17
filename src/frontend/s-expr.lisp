;;; s-expr.lisp ---
;;
;; Copyright (C) 2010-2017 Jan Moringen
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

(in-package :protocol-buffer.frontend)

(defun process-message (form)
  (bind (((_ name &rest specs) form)
	 ((:flet process-children (type initarg func))
	  `(,initarg (vector ,@(map 'list func (remove type specs
						       :key      #'first
						       :test-not #'eq))))))
    `(make-instance
      'pb::message-desc
      :name ',(string name)
      ,@(process-children :field   :field       #'process-field)
      ,@(process-children :message :nested-type #'process-message)
      ,@(process-children :enum    :enum-type   #'process-enum))))

(defun process-field (form)
  (bind (((_ name type position
	   &rest args
	   &key
	   optional required repeated
	   packed
           (default nil default?))
          form))
    (declare (ignore optional required repeated))
    (let ((labels (intersection args '(:optional :required :repeated))))
      `(make-instance 'pb::field-desc
		      :name    ,(string-downcase (string name))
		      :type    ',(cond
				  ((keywordp type)
				   type)
				  ((pb::enum-type-p type)
				   :enum)
				  (t
				   :message)) ;; TODO do this properly
		      ,@(unless (keywordp type)
				`(:type-name ,(substitute
					       #\. #\/ (string type))))
                      ,@(when default?
                          `(:default-value ,default))
		      :number  ,position
		      :label   ',(or (first labels) :required)
		      :options (make-instance 'pb::field-options
					      :packed ,packed)))))

(defun process-enum (form)
  (bind (((_ name &rest values) form))
    `(make-instance 'pb::enum-desc
		    :name  ,(string name)
		    :value (vector ,@(map 'list #'process-enum-value values)))))

(defun process-enum-value (form)
  (bind (((name number) form))
    `(make-instance 'pb::enum-value-desc
		    :name   ,(string name)
		    :number ,number)))
