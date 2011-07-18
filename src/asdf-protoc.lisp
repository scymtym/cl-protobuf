;;; asdf.lisp --- ASDF integration.
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

(in-package :asdf)

(defclass protocol-buffer-descriptor (source-file)
  ((type :initform "proto"))
  (:documentation
   "Instances of this component class represent file containing
protocol buffer descriptions using the textual descriptor format. The
proto file format is documented at
http://code.google.com/apis/protocolbuffers/docs/proto.html."))


;;; Compile Operation
;;

(defmethod output-files ((operation compile-op)
			 (file      protocol-buffer-descriptor))
  "Construct a pathname for the output of the protocol buffer compiler
based on the name and pathname of FILE."
  (values
   ;; We only have a single output file.
   (list (make-pathname
	  :name     (component-name file)
	  :type     "potobin"
	  :defaults (component-pathname file)))
   ;; This means: "Translate these pathnames"
   nil))

(defmethod output-files ((operation (eql :compile-op))
			 (file      protocol-buffer-descriptor))
  (output-files (make-instance (find-class (intern (string operation) :asdf)))
		file))
;; TODO can we avoid this one?

(defmethod perform ((operation compile-op)
		    (file      protocol-buffer-descriptor))
  "Call the protocol buffer compiler (protoc) to compile the textual
representation into a binary representation."
  (let* ((output-file   (first (output-files operation file)))
	 (input-path    (component-pathname file))
	 (input-dirctory (namestring
			  (make-pathname
			   :directory (pathname-directory input-path))))
	 (input-option  (namestring (make-pathname :type "proto"
						   :defaults input-path))) ;; TODO hack
	 (output-option (format nil "-o~A" (namestring output-file)))
	 (args          (list "-I" input-dirctory
			      input-option output-option)))

    ;; Ensure output directory exists
    (ensure-directories-exist
     (make-pathname :directory (pathname-directory output-file)))

    ;; Run the compiler
    (format *standard-output* "~@<; ~@;Compiling protocol buffer ~
descriptor ~A like this: ~_~A ~{~A~^ ~}~@:>~%"
	    file "protoc" args)
    (let ((result (sb-ext:process-exit-code
		   (sb-ext:run-program "protoc" args
				       :search t
				       :output *trace-output*
				       :error  *trace-output*))))
      (unless (zerop result)
	(error "~@<The protocol buffer compiler failed (exit code ~D) ~
to compile ~A to ~A.~@:>"
	       result file output-file)))))


;;; Load Operation
;;

(defmethod perform :around ((operation load-op)
			    (file      protocol-buffer-descriptor))
  (let ((pbb::*emit-print*   *asdf-verbose*)
	(pbb::*emit-verbose* nil))
    (call-next-method)))

;; TODO allow control over what is generated
(defmethod perform ((operation load-op)
		    (file      protocol-buffer-descriptor))
  "Load the compiled protocol buffer descriptor."
  (let* ((binary-pathname (first (output-files :compile-op file)))
	 (descriptors     (pbf:load/binary binary-pathname)))
    (pbb:emit descriptors :class)
    (pbb:emit descriptors :relations)
    (pbb:emit descriptors :packed-size)
    (pbb:emit descriptors :serializer)
    (pbb:emit descriptors :deserializer)
    (pbb:emit descriptors :extractor)
    (pbb:emit descriptors :offset)))


;;; Add features
;;

(pushnew :asdf-protocol-buffer-descriptors *features*)
(pushnew :asdf-protocol-buffers-use-protoc *features*)
