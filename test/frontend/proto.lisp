;;; proto.lisp --- Unit tests for the textual proto. desc. frontend.
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

(in-package :protocol-buffer.frontend.test)

(deftestsuite proto-frontend-root (frontend-root)
  ((descriptor-components (asdf:module-components
			   (asdf:find-component
			    (asdf:find-system :cl-protobuf-test)
			    '("descriptors"))))
   (descriptor-files)
   (error-components      (asdf:module-components
			   (asdf:find-component
			    (asdf:find-system :cl-protobuf-test)
			    '("syntax-errors"))))
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
	      (result-list     (load/text (list file))))
	  (iter (for result in (list result-pathname
				     result-list))
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

(addtest (proto-frontend-root
          :documentation
	  "Test import resolution and associated error handling.")
  import

  (let ((*proto-load-path* nil))
    (ensure-condition 'cannot-resolve-import
      (load/text "import \"/absolute-path/no-such-file.proto\";"))
    (ensure-condition 'cannot-resolve-import
      (load/text "import \"no-such-file.proto\";"))))
