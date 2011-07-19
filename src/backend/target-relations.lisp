;;; target-relations.lisp --- Generation of methods that implement relations.
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


;;; Target relations
;;

(defmethod documentation ((thing (eql :relations)) (type (eql 'target)))
  "Define methods that implement relations between protocol buffer
descriptors.")

(defclass target-relations (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for generating methods which implement relations
between protocol buffer descriptors."))

(defclass target-relations-fixup (code-generating-target-mixin)
  ()
  (:documentation
   "Target class generating and fixing some things after basic
relations have been emitted for all descriptors."))


;;; Emitter methods
;;

(defmethod emit :after ((node   file-set-desc)
			(target target-relations)
			&key)
  (emit node :relations-fixup))

(defmethod emit ((node   file-desc)
		 (target target-relations)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node file-desc)
      ;; Add or replace NODE to/in the file set which represents the
      ;; package.
      (let* ((container (ensure-package package))
	     (index     (position (descriptor-name node)
				  (pb::file-set-desc-file container)
				  :key  #'descriptor-name
				  :test #'string=)))
	(if index
	    (setf (aref (pb::file-set-desc-file container) index) node)
	    (vector-push-extend node (pb::file-set-desc-file container)))

	;; Generate relation methods for NODE.
	(eval
	 `(progn
	    (defmethod descriptor-qualified-name ((descriptor (eql ,node)))
	      ,package)
	    (defmethod descriptor-parent ((descriptor (eql ,node)))
	      ,container)
	    (defmethod find-descriptor ((name (eql ,(make-keyword name)))
					&key error?)
	      (declare (ignore error?))
	      ,node))))

      ;; Recurse into child nodes.
      (call-next-method))))

(defmethod emit ((node   message-desc)
		 (target target-relations)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (bind ((qualified-name (make-qualified-name
			      (descriptor-qualified-name parent) name)))

	;; Generate relation methods for NODE.
	(eval
	 `(progn
	    (defmethod descriptor-qualified-name ((descriptor (eql ,node)))
	      ,qualified-name)
	    (defmethod descriptor-parent ((descriptor (eql ,node)))
	      ,parent)
	    (defmethod find-descriptor ((name (eql ,(make-keyword qualified-name)))
					&key error?)
	      (declare (ignore error?))
	      ,node)))

	;; Recurse into children.
	(call-next-method)))))


;;; Emit methods for `target-relations-fixup
;;

(defmethod emit ((node   field-desc)
		 (target target-relations-fixup)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node field-desc)
      (when (member type '(:enum :message))
	;; Find the descriptor describing the type of NODE. Based on
	;; whether it is a message descriptor or an enum descriptor,
	;; maybe change type of NODE to :message.
	(let ((type (pb::resolve-name type-name parent)))
	  (setf (pb::field-desc-type node) (etypecase type
					     (message-desc :message)
					     (enum-desc    :enum)))

	  ;; When the type of NODE, TYPE, is a message, associate TYPE
	  ;; by means of a method on `field-type-descriptor'.
	  (when (field-message? node)
	    (eval `(defmethod field-type-descriptor ((descriptor (eql ,node)))
		     ,type))))))))


;;; Utility functions
;;

(defun ensure-package (name)
  "Return the `file-set-desc' instance for the package named NAME,
creating an empty `file-set-desc' instance, if necessary."
  (or (find-package1 name :error? nil)
      (let ((package (make-instance 'file-set-desc)))
	(eval `(defmethod find-package1 ((name (eql ,(make-keyword name)))
					 &key
					 error?)
		 (declare (ignore error?))
		 ,package))
	package)))

(defun make-qualified-name (parent-name name)
  "Return a qualified name in which PARENT-NAME qualifies NAME unless
NAME is itself a qualified name."
  (cond
    ((starts-with #\. name)
     name)
    ((starts-with #\. parent-name)
     (format nil "~A.~A" parent-name name))
    (t
     (format nil ".~A.~A" parent-name name))))
