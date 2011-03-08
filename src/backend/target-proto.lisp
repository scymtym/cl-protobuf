;;; target-proto.lisp ---
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

(defmethod documentation ((thing (eql :proto)) (type (eql 'target)))
  "Convert the structure based protocol buffer representation obtained
by parsing the binary output of protoc into textual protocol buffer
syntax.")


;;; Target class
;;

(defclass target-proto (stream-target-mixin)
  ()
  (:documentation
   "Instances of this class are used to specify the stream to which
protocol buffer descriptions should be printed."))


;;; Target-specific macros
;;

(defmacro with-proto-logical-block ((kind name) &body body)
  "Execute BODY with the stream of the current emit target bound to a
pretty-printing stream that indents all output produced within BODY to
a certain depth. In addition, a scope of kind KIND and name NAME is
printed around the output."
  (with-unique-names (target-var stream-var)
    `(let* ((,target-var (context-target *context*))
	    (,stream-var (target-stream ,target-var)))
       (format ,stream-var "~A ~A {~%" ,kind ,name)
       (unwind-protect
	    (pprint-logical-block (,stream-var nil
					       :per-line-prefix "  ")
	      (setf (target-stream ,target-var) ,stream-var)
	      ,@body)
	 (setf (target-stream ,target-var) ,stream-var)
	 (format ,stream-var "}~%")))))


;;; Emitter methods
;;

(defmethod emit ((node t) (target (eql :proto)))
  (error "Have to specify stream for :proto target"))

(defmethod emit :before ((node   pb::file-set-desc)
			 (target target-proto))
  (with-stream-emit-symbols stream
    (format stream "// File Descriptor Set~%")))

(defmethod emit ((node   pb::file-desc)
		 (target target-proto))
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name     pb::file-desc-name)
	     (package  pb::file-desc-package)
	     (options  pb::file-desc-options)
	     (enums    pb::file-desc-enum-type)
	     (messages pb::file-desc-message-type)) node))
      (format stream "// BEGIN File Descriptor Proto~%")
      (format stream "// Name: ~A~%" name)

      (format stream "package ~A;~%" package)
      (unless (emptyp options)
	(map nil #'recur options))
      (unless (emptyp enums)
	(format stream "// Enums:~%")
	(map nil #'recur enums))
      (unless (emptyp messages)
	(format stream "// Messages:~%")
	(map nil #'recur messages))
      ;; TODO services
      (format stream "// END File Descriptor Proto~%"))))

(defmethod emit ((node   pb::file-options)
		 (target target-proto))
  (with-stream-emit-symbols stream
    (iter (for (slot label) in
	       '((pb::java-package         "java_package")
		 (pb::java-outer-classname "java_outer_classname")))
	  (let ((value (slot-value node slot)))
	    (when (and value (string/= value "")) ;; TODO avoid string stuff
	      (format stream "option ~A = ~S;~%" label value))))))

(defmethod emit ((node   pb::message-desc)
		 (target target-proto))
  (bind (((:accessors-r/o
	   (name pb::message-desc-name)) node))
    (with-proto-logical-block ("message" name)
      (call-next-method))))

(defmethod emit ((node   pb::field-desc)
		 (target target-proto))
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (number    pb::field-desc-number)
	     (label     pb::field-desc-label)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (default   pb::field-desc-default-value)
	     (options   pb::field-desc-options)) node))

      (format stream "~(~A~) ~A ~A = ~A"
	      label
	      (if (member type '(:message :enum))
		  type-name
		  (string-downcase type))
	      name number)
      (when (and default (string/= default "")) ;; TODO avoid string stuff
	(format stream " [default = ~A]" default))
      (when options
	(recur options))
      (format stream ";~%"))))

(defmethod emit ((node   pb::field-options)
		 (target target-proto))
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (packed pb::field-options-packed)) node))
      (format stream " ~:[~;[packed = true]~]" packed))))

(defmethod emit ((node   pb::enum-desc)
		 (target target-proto))
  (bind (((:accessors-r/o
	   (name pb::enum-desc-name)) node))
    (with-proto-logical-block ("enum" name)
      (call-next-method))))

(defmethod emit ((node   pb::enum-value-desc)
		 (target target-proto))
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name   pb::enum-value-desc-name)
	     (number pb::enum-value-desc-number)) node))
      (format stream "~16A = ~2D;~%" name number))))
