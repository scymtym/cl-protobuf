;;; target-proto.lisp --- Unit tests for the (textual) proto backend.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
