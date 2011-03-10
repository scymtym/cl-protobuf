;;; target-proto.lisp ---
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

(in-package :protocol-buffer.backend.test)

(deftestsuite target-proto-root (backend-root
				 emitter-suite)
  ()
  (:function
   (expected-output-for-descriptor (descriptor)
     (let* ((name              (pb::file-desc-name
				(aref (pb::file-set-desc-file descriptor) 0)))
	    (expected-pathname (asdf:system-relative-pathname
				(asdf:find-system :cl-protobuf-test)
				(format nil "test/data/~A" name)
				:type "expected")))
       (read-file-into-string expected-pathname))))
  (:documentation
   "Unit tests for the proto target."))

(addtest (target-proto-root
          :documentation
	  "Smoke test for the proto target.")
  smoke

  (iter (for descriptor in descriptors)
	(let ((result   (with-output-to-string (stream)
			  (emit descriptor `(:proto :stream ,stream))))
	      (expected (expected-output-for-descriptor descriptor)))
	(ensure-same
	 result expected
	 :test      #'string=
	 :report    "~@<First mismatch at ~D.~@:>"
	 :arguments ((mismatch result expected))))))
