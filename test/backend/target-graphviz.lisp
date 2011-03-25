;;; target-graphviz.lisp --- Unit tests for the graphviz target.
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

(deftestsuite target-graphviz-root (backend-root
				    emitter-suite)
  ()
  (:function
   (output-pathname-for-descriptor (descriptor type)
     (let ((name (pb::file-desc-name
		  (aref (pb::file-set-desc-file descriptor) 0))))
       (asdf:system-relative-pathname
	(asdf:find-system :cl-protobuf)
	(make-pathname
	 :type      type
	 :directory '(:relative "doc" "figures")
	 :defaults  name)))))
  (:documentation
   "Unit tests for the graphviz target."))

(addtest (target-graphviz-root
          :documentation
	  "Smoke test for the graphviz target.")
  smoke-dot

  (iter (for descriptor in descriptors)
	(let ((dot-name (output-pathname-for-descriptor descriptor "dot")))
	  (emit descriptor `(:graphviz-dot-file :pathname ,dot-name)))))

(addtest (target-graphviz-root
          :documentation
	  "Smoke test for the graphviz target.")
  smoke-image

  (iter (for descriptor in descriptors)
	(let ((png-name (output-pathname-for-descriptor descriptor "png")))
	  (emit descriptor `(:graphviz-image-file :pathname ,png-name)))))
