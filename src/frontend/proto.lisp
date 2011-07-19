;;; proto.lisp --- Interface for the proto parser.
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

(in-package :protocol-buffer.frontend)

(defgeneric load/text (source)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description.
Return a `file-set-desc' instance that contains the complete
description in its child nodes.

Note: Currently, the grammar accepts only a (quite usable) subset of
the textual protocol buffer description language.

Note: This function does not load data in a protocol buffer format,
but textual protocol buffer descriptors using the grammar described at
http://code.google.com/apis/protocolbuffers/docs/proto.html."))

(defmethod load/text ((source stream))
  (make-instance
   'file-set-desc
   :file (make-array 1
		     :initial-element (make-file-desc
				       "<stream>" (parse source))
		     :fill-pointer    1)))

(defmethod load/text ((source pathname))
  (let* ((set  (with-input-from-file (stream source)
		 (load/text stream)))
	 (file (aref (pb::file-set-desc-file set) 0)))
    (setf (pb::file-desc-name file)
	  (concatenate 'string (pathname-name source) ".proto"))
    (pbb:emit set :relations)
    set))

(defmethod load/text ((source string))
  (load/text (parse-namestring source)))

(defmethod load/text ((source list))
  "This method read descriptions from all files in the list SOURCE and
collects the resulting `file-desc' instance in one `file-set-desc'."
  (bind (((result &rest rest) (map 'list #'load/text source))
	 ((:flet merge-one (desc))
	  (vector-push-extend
	   (aref (pb::file-set-desc-file desc) 0)
	   (pb::file-set-desc-file result))))
    (map nil #'merge-one rest)
    result))

(defmethod load/text ((source asdf::protocol-buffer-descriptor))
  "Load a textual protocol buffer description from the ASDF component
SOURCE."
  (load/text (asdf:component-pathname source)))
