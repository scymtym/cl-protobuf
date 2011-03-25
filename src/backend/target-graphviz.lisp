;;; target-graphviz.lisp --- render descriptor relationships using graphviz.
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

(defmethod documentation ((thing (eql :graphviz)) (type (eql 'target)))
  "This target emits graphviz graph descriptions that correspond to
the relationships of the supplied protocol buffer descriptor
instances.")


;;; Target class
;;

(defclass target-graphviz (stream-target-mixin)
  ()
  (:documentation
   "Instances of this target class can be used to direct graphviz
output representing protocol buffer descriptor instances into a given
stream."))

(defclass target-graphviz-record-line (stream-target-mixin)
  ()
  (:documentation
   "A helper target class which is used to format message field
descriptors within a record shaped node representing a message
descriptor instance."))


;;; Target-specific macros
;;

(defmacro with-graphviz-logical-block ((kind name) &body body)
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

(defmethod emit ((node   pb::file-set-desc)
		 (target target-graphviz)
		 &key)
  (with-graphviz-logical-block ("digraph" "foo")
    (call-next-method)))

(defmethod emit ((node   pb::message-desc)
		 (target target-graphviz)
		 &key)
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (fields pb::message-desc-field)) node))
      (format stream "~A [shape=record,label=<<TABLE BORDER=\"0\"><TR><TD COLSPAN=\"3\">~A</TD></TR>"
	      (string->graphviz-label name) name)
      (map 'nil (rcurry #'emit (make-instance 'target-graphviz-record-line
					      :stream stream))
	   fields)
      (format stream "</TABLE>>]~%"))
    (call-next-method)))

(defmethod emit ((node   pb::field-desc)
		 (target target-graphviz-record-line)
		 &key)
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name   pb::field-desc-name)
	     (type   pb::field-desc-type)
	     (number pb::field-desc-number)) node))
      (format stream "<TR><TD align=\"left\">~A</TD>~
<TD align=\"left\">~A</TD>~
<TD align=\"right\">~D</TD></TR>"
	      (string->graphviz-label (string type))
	      (string->graphviz-label name)
	      number))))

(defmethod emit ((node   pb::field-desc)
		 (target target-graphviz)
		 &key)
  "Generate code to unpack a single slot"
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)) node))
      (unless (or (member type +proto-types+)
		  (typep (find-class 'type nil) 'built-in-class))
	(format stream "~A -> ~A [label=~S]~%"
		(string->graphviz-label (pb::message-desc-name parent))
		(string->graphviz-label (string type-name))
		name)))))

(defmethod emit ((node   pb::enum-desc)
		 (target target-graphviz)
		 &key)
  "Generate a record-shaped node for the enum NODE."
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name   pb::enum-desc-name)) node))
      (format stream "~A [shape=record,label=<<TABLE BORDER=\"0\"><TR><TD COLSPAN=\"2\">~A</TD></TR>"
	      (string->graphviz-label name)
	      name)
      (call-next-method)
      (format stream "</TABLE>>]~%"))))

(defmethod emit ((node   pb::enum-value-desc)
		 (target target-graphviz)
		 &key)
  "Generate a table row for the enum value NODE."
  (with-stream-emit-symbols stream
    (bind (((:accessors-r/o
	     (name   pb::enum-value-desc-name)
	     (number pb::enum-value-desc-number)) node))
      (format stream "<TR><TD align=\"left\">~A</TD><TD align=\"right\">~A</TD></TR>"
	      (string->graphviz-label name)
	      number))))


;;; Utility functions
;;

(defun string->graphviz-label (string)
  "Turn STRING into a legal graphviz label string."
  (let ((temp (if (find #\. string)
		  (subseq string (1+ (position #\. string :from-end t)))
		  string)))
    (substitute #\_ #\- temp)))
