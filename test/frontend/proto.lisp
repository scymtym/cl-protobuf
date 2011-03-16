;;; proto.lisp --- Unit tests for the textual proto. desc. frontend.
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

(in-package :protocol-buffer.frontend.test)

(deftestsuite proto-frontend-root (frontend-root)
  ((descriptor-components (asdf:module-components
			   (asdf:find-component
			    (asdf:find-system :cl-protobuf-test)
			    '("test" "descriptors"))))
   (descriptor-files)
   (error-components      (asdf:module-components
			   (asdf:find-component
			    (asdf:find-system :cl-protobuf-test)
			    '("test" "syntax-errors"))))
   (error-files))
  (:setup
   (setf descriptor-files (map 'list #'asdf:component-pathname
			       descriptor-components)
	 error-files      (map 'list #'asdf:component-pathname
			       error-components)))
  (:documentation
   "Unit tests for the textual protocol buffer description
frontend."))

(addtest (proto-frontend-root
          :documentation
	  "Smoke test for the textual protocol buffer description
frontend.")
  smoke

  (iter (for file in descriptor-files)
	(let ((result-pathname (load/text file))
	      (result-string   (load/text (namestring file))))
	  (iter (for result in (list result-pathname result-string))
		(ensure
		 (typep result 'pb::file-set-desc)
		 :report    "~@<When parsing the file ~A, the result ~S was ~
of type ~S, not ~S.~@:>"
		 :arguments (file result (type-of result) 'pb::file-set-desc))))))

(addtest (proto-frontend-root
          :documentation
	  "Ensure that syntax errors signal parse errors.")
  syntax-errors

  (iter (for file in error-files)
	(ensure-condition 'proto-parse-error
	  (load/text file))))

(addtest (proto-frontend-root
          :documentation
	  "Test passing asdf components to the frontend functions.")
  asdf

  (iter (for component in descriptor-components)
	(let ((result (load/text component)))
	  (ensure
	   (typep result 'pb::file-set-desc)
	   :report    "~@<When parsing the ASDF component ~A, the result ~
~S was of type ~S, not ~S.~@:>"
	   :arguments (component result (type-of result) 'pb::file-set-desc)))))
