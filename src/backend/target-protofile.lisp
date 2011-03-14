;;; target-protofile.lisp --- Emit multi-file textual descriptions.
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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

(in-package :protocol-buffer.backend)


;;;
;;

(defmethod documentation ((thing (eql :protofile)) (type (eql 'target)))
  "Convert the structure based protocol buffer representation obtained
by parsing the binary output of protoc into textual protocol buffer
syntax.")


;;; Target class
;;

(defclass target-protofile ()
  ((directory :initarg  :directory
	      :type     pathname
	      :accessor target-directory
	      :documentation
	      "The directory into which the one or more proto files
should be written."))
  (:default-initargs
   :directory (missing-required-initarg 'target-protofile :directory))
  (:documentation
   "Instances of this class can be used to print textual protocol
buffer descriptions into multiple files in a common directory."))


;;; Emitter methods
;;

(defmethod emit ((node   pb::file-desc)
		 (target target-protofile)
		 &key)
  (bind (((:accessors-r/o
	   (file pb::file-desc-name)) node))
    (with-output-to-file (stream (merge-pathnames
				  file
				  (target-directory target))
				 :if-exists :supersede)
      (emit node `(:proto :stream ,stream)))))
