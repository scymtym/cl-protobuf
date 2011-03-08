;;; protocol.lisp ---
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


;;; Utility Functions TODO move to end of file
;;

(defmacro with-emit-symbols (&body body)
  "Execute BODY with the following symbols added to the lexical scope:
+ package            :: The package that is the current target of the
                        emission process.
+ parent             :: The parent of the current node or nil.
+ grandparent        :: The parent of the parent of the current node
                        or nil.
+ ancestors          :: List of all ancestor nodes.
+ parent-is-message? :: non-nil if parent is non-nil and of a subtype
                        of `message-desc'.
+ recur              :: A closure that accepts a node and calls `emit'
                        with the current target on that node.
+ intern*            :: Similar to `intern' but use the context package."
  `(symbol-macrolet ((package            (context-package *context*))
		     (parent             (second (context-stack *context*)))
		     (grandparent        (third (context-stack *context*)))
		     (ancestors          (rest (context-stack *context*)))
		     (parent-is-message? (typep parent 'pb::message-desc)))
     (flet ((recur (node)
	      (emit node (context-target *context*)))
	    (intern* (name) ;; TODO rename
	      (intern name package)))
       (declare (ignorable #'recur #'intern*))
       ,@body)))


;;; Backend Documentation
;;

(intern "TARGET") ;; for (documentation :TARGET 'protocol-buffer.backend:target)


;;; Special variables
;;

(declaim (special *emit-verbose*))

(defvar *emit-verbose* nil
  "If non-nil print strings to standard output during `emit' calls
which describe what is being emitted.")

(declaim (special *emit-print*))

(defvar *emit-print* nil
  "TODO")


;;; Backend Context
;;

(defclass context ()
  ((target  :initarg  :target
	    :accessor context-target
	    :initform nil
	    :documentation
	    "The target of the current emission process.")
   (stack   :initarg  :stack
	    :type     list
	    :accessor context-stack
	    :initform nil
	    :documentation
	    "A stack of nodes currently being processed in the current
emission process.")
   (package :initarg  :package
	    :type     (or null package)
	    :accessor context-package
	    :initform nil
	    :documentation
	    "The target package of the current emission process."))
  (:documentation
   "Instances of this class are used to keep track of the current
state of a particular emission process. This state consists of:
+ the emission target
+ the stack of nodes currently being processed
+ the package to which symbols are currently emitted"))

(declaim (special *context*))

(defvar *context* (make-instance 'context)
  "This variable holds the emission context of the current thread.")


;;;
;;

(defgeneric emit (node target) ;; &key verbose print &allow-other-keys
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET."))


;;;
;;

(defmethod  emit ((node t) (target list))
  (let* ((target-name (first target))
	 (class-name  (or (find-symbol
			   (format nil "TARGET-~A" target-name)
			   :pbb)
			  (error "No such target ~A" target-name)))
	 (target1    (apply #'make-instance class-name (rest target))))
    (emit node target1)))

(defmethod emit ((node t) (target symbol))
  (emit node (list target)))


;;; Housekeeping and such
;;

(defmethod emit :before ((node t) (target standard-object)) ;; TODO &key for verbose print
  ;; Printing
  (when *emit-verbose*
    (format *standard-output* "~@<; ~@;emitting ~S for target ~S~@:>~%"
	    node target))
  (when *emit-print*
    (format *standard-output* "~@<; ~@;emitting (~A)~@:>~%" (type-of node))))

(defmethod emit :around ((node t) (target standard-object))
  ;; Set current target type and push the current node onto the
  ;; context stack.
  (let ((old-target (context-target *context*)))
    (setf (context-target *context*) target)
    (push node (context-stack *context*))
    (unwind-protect
	 (call-next-method)
      (pop (context-stack *context*))
      (setf (context-target *context*) old-target))))


;;; Default recursion behavior
;;

(defmethod emit ((node t) (target standard-object))
  (values))

(defmethod emit ((node pb::file-set-desc) (target standard-object))
  "Multi-file container; default behavior is recursion over files."
  (with-emit-symbols
    (map nil #'recur (pb::file-set-desc-file node))))

(defmethod emit ((node pb::file-desc) (target standard-object))
  "File; default behavior is recursion over messages, enums, services,
extensions and options."
  (with-emit-symbols
    (map nil #'recur (pb::file-desc-message-type node))
    (map nil #'recur (pb::file-desc-enum-type    node))
    (map nil #'recur (pb::file-desc-service      node))
    (map nil #'recur (pb::file-desc-extension    node))
    (map nil #'recur (pb::file-desc-options      node))))

(defmethod emit ((node pb::message-desc) (target standard-object))
  "Message; default behavior is recursion over contained elements."
  (with-emit-symbols
    (map nil #'recur (pb::message-desc-enum-type   node))
    (map nil #'recur (pb::message-desc-nested-type node))
    (map nil #'recur (pb::message-desc-field       node))
    (map nil #'recur (pb::message-desc-extension   node))))

(defmethod emit ((node pb::field-desc) (target standard-object))
  "Field; default behavior is recursion into options it any."
  (with-emit-symbols
    (bind (((:accessors-r/o
	     (options pb::field-desc-options)) node))
      (when options
	(recur options)))))

(defmethod emit ((node pb::enum-desc) (target standard-object))
  "Enum; default behavior is recursion over enum values."
  (with-emit-symbols
    (map 'list #'recur (pb::enum-desc-value node))))
