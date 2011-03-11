;;; macros.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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
