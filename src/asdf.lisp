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

(defgeneric component-proto-load-path (component)
  (:method-combination append)
  (:documentation
   "Return a list of protocol buffer load path entries that should be
added to the current protocol buffer load path when loading
COMPONENT."))

(defmethod component-proto-load-path append ((component component))
  (when (component-parent component)
    (component-proto-load-path (component-parent component))))

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
  "We do not compile, so there is no output."
  ;; We do not have any output files. The second nil means: "Translate
  ;; these pathnames"
  (values nil nil))

(defmethod perform ((operation compile-op)
		    (file      protocol-buffer-descriptor))
  "Compiling is a no-op since we load the textual description
directly."
  (values))


;;; Load Operation
;;

(defmethod perform :around ((operation load-op)
			    (file      protocol-buffer-descriptor))
  (let ((pbb:*emit-print*      *asdf-verbose*)
	(pbb:*emit-verbose*    nil)
	(pbf:*proto-load-path* (remove-duplicates
				(append (component-proto-load-path file)
					pbf:*proto-load-path*))))
    (call-next-method)))

(defmethod perform ((operation load-op)
		    (file      protocol-buffer-descriptor))
  "Load the compiled protocol buffer descriptor."
  (let ((descriptors (pbf:load/text (component-pathname file))))
    (pbb:emit descriptors :class)
    (pbb:emit descriptors :packed-size)
    (pbb:emit descriptors :serializer)
    (pbb:emit descriptors :deserializer)
    (pbb:emit descriptors :extractor)
    (pbb:emit descriptors :offset)))


;;; Load method from component
;;

(defmethod protocol-buffer.frontend:load/text
    ((source protocol-buffer-descriptor)
     &key &allow-other-keys)
  "Load a textual protocol buffer description from the ASDF component
SOURCE."
  (protocol-buffer.frontend:load/text (component-pathname source)))


;;;
;;

(defclass protocol-buffer-descriptor-directory (module)
  ()
  (:default-initargs
   :default-component-class 'protocol-buffer-descriptor)
  (:documentation
   "Instances of this module class represent directories which contain
protocol buffer descriptor files (usually of type \"proto\") directly
or in their subdirectories. The protocol buffer load-path is extended
to contain the pathname of the respective module. Import statements
with relative filenames are then interpreted as being relative to
the pathname of the module."))

(defmethod component-proto-load-path append ((component protocol-buffer-descriptor-directory))
  (list (component-pathname component)))

(defmethod (setf module-default-component-class)
    ((new-value (eql nil))
     (module     protocol-buffer-descriptor-directory))
  "Prevent our default component class from being overwritten with
  nil."
  nil)


;;; Add features
;;

(pushnew :asdf-protocol-buffer-descriptors *features*)
(pushnew :asdf-protocol-buffers-use-lisp   *features*)
