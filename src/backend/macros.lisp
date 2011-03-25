;;; macros.lisp --- Macros for backends.
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

(in-package :protocol-buffer.backend)



;;;
;;

(defmacro with-emit-restarts (node-var target-var &body body)
  "Establish restarts."
  (with-unique-names (result-var read-value-var)
    `(bind ((,result-var)
	    ((:flet ,read-value-var ())
	     (format *query-io* "Replacement value: ")
	     (force-output *query-io*)
	     (list (read *query-io*))))
       (tagbody
	:retry
	  (restart-case
	      (setf ,result-var (multiple-value-list (progn ,@body)))

	    ;; Retry running the emit method.
	    (retry ()
	      :report
	      (lambda (stream)
		(format stream
			"~@<Retry running the emit method for node ~S and target ~S.~@:>"
			,node-var ,target-var))
	      (go :retry))

	    ;; Skip the emit method.
	    (skip ()
	      :report
	      (lambda (stream)
		(format stream
			"~@<Skip the emit method for node ~S and target ~S.~@:>"
			,node-var ,target-var)))

	    ;; Use a replacement value.
	    (use-value (value)
	      :report
	      (lambda (stream)
		(format stream
			"~@<Specify a value instead of running the ~
emit method for node ~S and target ~S.~@:>"
			,node-var ,target-var))
	      :interactive ,read-value-var
	      (setf ,result-var (list value)))))
       (values-list ,result-var))))

(defmacro with-updated-context (node-var target-var &body body)
  "During the execution of BODY, set the current target type to
TARGET-VAR and push NODE-VAR onto the context stack."
  (with-unique-names (old-target-var)
    `(let ((,old-target-var (context-target *context*)))
       (setf (context-target *context*) ,target-var)
       (push ,node-var (context-stack *context*))
       (unwind-protect
	    (progn ,@body)
	 (pop (context-stack *context*))
	 (setf (context-target *context*) ,old-target-var)))))
