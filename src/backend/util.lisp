;;; util.lisp ---
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


;;; Naming-related functions
;;

(defun make-lisp-enum-name (name parent)
  "DOC"
  (pb::->lisp-name (%maybe-nested-name name parent)))

(defun make-lisp-class-name (name parent)
  "DOC"
  (pb::->lisp-name (%maybe-nested-name name parent)))

(defun make-lisp-slot-name (name)
  "DOC"
  (pb::->lisp-name name))

(defun make-lisp-accessor-name (class-name slot-name)
  "CLASS-NAME and SLOT-NAME have to be proper symbolic names."
  (intern (concatenate 'string
		       (string class-name)
		       "-"
		       (string slot-name))
	  (symbol-package class-name)))


;;; Package-related functions
;;

(defun maybe-find-package-or-loose (name)
  "If NAME designates a package, try to find it."
  (if (and name (string/= name "")) ;; TODO can we avoid the ugly "" case?
      (let ((package-name (pb::->lisp-name name :allow-dots? t)))
	(or (find-package (string-upcase name))
	    (error "Could not find package ~S (for ~S)" ;; TODO proper condition
		   package-name name)))
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
