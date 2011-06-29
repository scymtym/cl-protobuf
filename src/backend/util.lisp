;;; util.lisp ---
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


;;; Naming-related functions
;;

(defun make-lisp-enum-name (name &optional parent) ;; TODO change to parents
  "Return a suitable name for an enumeration based on NAME and
PARENT. PARENT should be non-nil for nested enumerations."
  (pb::->lisp-name (%maybe-nested-name name parent)))

(defun make-lisp-enum-value (name)
  "Return a suitable name for the enumeration value originally named
NAME."
  (make-keyword (pb::->lisp-name name)))

(defun make-lisp-class-name (name &optional parent)
  "Return a suitable name for a class based on NAME and PARENT. PARENT
should be non-nil for nested messages."
  (pb::->lisp-name (%maybe-nested-name name parent)))

(defun make-lisp-slot-name (name)
  "Return a suitable name for a slot based on the field name NAME."
  (pb::->lisp-name name))

(defun make-lisp-slot-type (type type-name package)
  "Return a symbol that designates a suitable slot type for the field
type TYPE. "
  (if (member type '(:message :enum))
      (pb::proto-type-name->lisp-type-symbol
       type-name :package package)
      type))

(defun make-lisp-accessor-name (class-name slot-name)
  "CLASS-NAME and SLOT-NAME have to be proper symbolic names."
  (let ((*package* (symbol-package class-name)))
    (symbolicate class-name "-" slot-name)))


;;; Package-related functions
;;

(defun maybe-find-package-or-loose (name)
  "If NAME designates a package, try to find it."
  (if (and name (string/= name "")) ;; TODO can we avoid the ugly "" case?
      (let ((package-name (pb::->lisp-name name :allow-dots? t)))
	(or (find-package package-name)
	    (error 'no-such-package-for-name
		   :name      package-name
		   :orig-name name)))
      (find-package :cl-user)))

(defun maybe-make-package (name)
  "If NAME designates a package, create it if necessary. "
  (when (and name (string/= name "")) ;; TODO can we avoid the ugly "" case?
    (let ((package-name (pb::->lisp-name name :allow-dots? t)))
      (or (find-package package-name)
	  (make-package package-name)))))


;;;
;;

(defun %maybe-nested-name (name parent)
  (if (and parent (typep parent 'pb::message-desc))
      (concatenate
       'string
       (pb::message-desc-name parent) "-" name)
      name))
