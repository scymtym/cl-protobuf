;;; package.lisp --- Package definition for unit tests of the backend module.
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

(cl:in-package :cl-user)

(defpackage :protocol-buffer.backend.test
  (:use
   :cl
   :alexandria
   :iterate
   :lift

   :protocol-buffer.backend)
  (:documentation
   "This package contains unit tests for the backend module"))

(in-package :protocol-buffer.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))

(deftestsuite emitter-suite ()
  ((descriptors (mapcar
		 (lambda (name)
		   (pbf:load/binary
		    (asdf:component-pathname
		     (asdf:find-component
		      (asdf:find-system :cl-protobuf-test)
		      `("test" "descriptors"
			       ,(format nil "~A.protobin" name))))))
		 '("addressbook" "developer-guide"))))
  (:documentation
   "Superclass for emitter test suites."))
