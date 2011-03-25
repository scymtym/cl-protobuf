;;; target-s-expr.lisp ---
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
