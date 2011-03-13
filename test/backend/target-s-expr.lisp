;;; target-s-expr.lisp ---
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

(deftestsuite backend-s-expr-root (backend-root
				   emitter-suite)
  ()
  (:function
   (expected-output-for-descriptor (descriptor)
     (bind (((:flet proto->sexpr (filename))
	     (replace filename "sexpr"
		      :start1 (- (length filename) 5)))
	    (name              (pb::file-desc-name
				(aref (pb::file-set-desc-file descriptor) 0)))
	    (expected-pathname (asdf:system-relative-pathname
				(asdf:find-system :cl-protobuf-test)
				(format nil "test/data/~A"
					(proto->sexpr name))
				:type "expected")))
       (with-input-from-file (stream expected-pathname)
	 (read stream)))))
  (:function
   (tree-mismatch (tree1 tree2)
		  (format t "~S~%~S~%" tree1 tree2)
     (when (and (typep tree1 'sequence) (typep tree2 'sequence))
       (let* ((mismatch (mismatch tree1 tree2 :test #'equal))
	      (child1   (elt tree1 mismatch))
	      (child2   (elt tree2 mismatch)))
	 (cons mismatch (tree-mismatch child1 child2))))))
  (:documentation
   "Unit tests for the s-expr target."))

(addtest (backend-s-expr-root
          :documentation
	  "Smoke test for the s-expr target.")
  smoke

  (iter (for descriptor in descriptors)
	(let ((result   (emit descriptor :s-expr))
	      (expected (expected-output-for-descriptor descriptor)))
	  (ensure-same
	   result expected
	   :test      #'equal
	   :report    "~@<First mismatch at ~S.~@:>"
	   :arguments ((tree-mismatch result expected))))))
