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

(defun make-lisp-slot-type (node)
  "Return a symbol that designates a suitable slot type for the field
NODE."
  (if (or (field-message? node) (field-enum? node))
      (emit (field-type-descriptor node) :lisp-name)
      (pb::field-desc-type node)))

(defun make-lisp-accessor-name (class-name slot-name)
  "CLASS-NAME and SLOT-NAME have to be proper symbolic names."
  (let ((*package* (symbol-package class-name)))
    (symbolicate class-name "-" slot-name)))

(defun resolve-name (name context
		     &key
		     (error? t))
  "Try to resolve NAME in the descriptor CONTEXT or one of its
parents. If NAME is a qualified name \(i.e. starting with \".\"),
CONTEXT is not used. If no descriptor can be found for NAME, nil is
returned or an error is signaled, depending on ERROR?."
  (bind (((:values components qualified?) (parse-name name))
	 ((:labels name-or-package (descriptor))
	  (if (typep descriptor 'file-desc)
	      (pb::file-desc-package descriptor)
	      (descriptor-name descriptor)))

	 ((:labels resolve-in (components context))
	  (bind (((first &rest rest) components)
		 (match (find first (descriptor-children context)
			      :key  #'name-or-package
			      :test #'string=)))
	    (cond
	      ((not match) nil)
	      ((not rest)  match)
	      (t           (resolve-in rest match)))))

	 ((:labels possible-splits (package name))
	  (bind (((first &rest rest) name))
	    (cond
	      (package
	       (cons (cons (format nil "~{~A~^.~}" package) name)
		     (when rest
		       (possible-splits (append package (list first)) rest))))
	      (rest
	       (possible-splits (append package (list first)) rest))
	      (t
	       (list name)))))

	 ((:labels resolve (components context &optional up?))
	  (if (typep context 'file-set-desc)
	      ;; If CONTEXT is a file set, try to resolve NAME in all
	      ;; its files, but without "upwards recursion". This
	      ;; necessary to resolve a name in a package consisting
	      ;; of multiple files.
	      (some (curry #'resolve components) (descriptor-children context))
	      ;; For other containers, search in the container itself
	      ;; and then, if UP? is non-nil, in its parents.
	      (or (some (rcurry #'resolve-in context)
			(possible-splits nil components))
		  (when (and up? (descriptor-parent context))
		    (resolve components (descriptor-parent context) up?))))))

    (or (and qualified? (find-descriptor name))
	(resolve components context t)
	(bind ((context-components
		(parse-name (descriptor-qualified-name context))))
	  (iter outer
		(for package-names on (rest (reverse context-components)))
		(iter (for (package-name . name) in (possible-splits
						     (reverse package-names)
						     components))
		      (when-let* ((package1 (find-package1 package-name
							   :error? nil))
				  (result   (resolve name package1)))
			(return-from outer result)))))
	(when error?
	  (error 'name-resolution-failed
		 :name name)))))


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
