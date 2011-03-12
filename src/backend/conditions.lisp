;;; conditions.lisp --- Conditions used in the backend.
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

(define-condition no-coder (error)
  ((type :initarg  :type
	 :type     symbol
	 :accessor no-coder-type
	 :documentation
	 "The protocol buffer type for which no coder could be
found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Cannot find en- or decoder for protocol buffer type ~S.~@:>"
	     (no-coder-type condition))))
  (:documentation
   "This error is signaled when no en- or decoder can be found for a
specified protocol buffer type."))

(define-condition no-such-target (error)
  ((name :initarg  :name
	 :type     (or string symbol)
	 :accessor no-such-target-name
	 :documentation
	 ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The specified target class ~S cannot be found.~@:>"
	     (no-such-target-name condition))))
  (:documentation
   "This error is signaled if a specified target class cannot be
found."))

(define-condition no-such-package (error)
  ((name :initarg  :name
	 :type     (or string symbol)
	 :accessor no-such-package-name
	 :documentation
	 "The name designating the package that could not be found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The name ~S does not designate a package.~@:>"
	     (no-such-package-name condition))))
  (:documentation
   "This error is signaled when a package that has been specified as
the target for a code generation process could not be found."))

(define-condition no-such-package-for-name (no-such-package)
  ((orig-name :initarg  :orig-name
	      :type     (or string symbol)
	      :accessor no-such-package-orig-name
	      :documentation
	      "The original name from which the package name has been
derived."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The name ~S (translated from ~S) does not ~
designate a package.~@:>"
	     (no-such-package-name      condition)
	     (no-such-package-orig-name condition))))
  (:documentation
   "This error is signaled when a package that is designated by a name
which has been translated from some other name cannot be found. Most
commonly, \"other\" names are protocol buffer package names."))
