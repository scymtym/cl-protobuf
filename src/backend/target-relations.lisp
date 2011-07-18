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


;;; Emitter methods
;;

(defmethod emit ((node   file-desc)
		 (target target-relations)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node file-desc)
      ;; Add or replace NODE to/in the file set which represents the
      ;; package.
      (let* ((container (ensure-package package))
	     (index     (position node (pb::file-set-desc-file container)
				  :key  #'descriptor-name
				  :test #'string=)))
	(if index
	    (setf (aref container index) node)
	    (vector-push-extend node (pb::file-set-desc-file container))))

      ;; Generate relation methods for NODE.
      (eval
       `(progn
	  (defmethod descriptor-qualified-name ((descriptor (eql ,node)))
	    ,package)
	  (defmethod descriptor-parent ((descriptor (eql ,node)))
	    ,parent)
	  (defmethod find-descriptor ((name (eql ,(make-keyword name)))
				      &key error?)
	    (declare (ignore error?))
	    ,node)))

      ;; Recurse into child nodes.
      (call-next-method))))

(defmethod emit ((node   message-desc)
		 (target target-relations)
		 &key)
  (with-emit-symbols
    (with-descriptor-fields (node message-desc)
      (bind ((qualified-name (make-qualified-name
			      (pb::descriptor-qualified-name parent) name)))

	;; Generate relation methods for NODE.
	(eval
	 `(progn
	    (defmethod descriptor-qualified-name ((descriptor (eql ,node)))
	      ,qualified-name)
	    (defmethod descriptor-parent ((descriptor (eql ,node)))
	      ,parent)
	    (defmethod descriptor-package ((descriptor (eql ,node)))
	      ,(find (complement (of-type 'message-desc)) ancestors))
	    (defmethod find-descriptor ((name (eql ,(make-keyword qualified-name)))
					&key error?)
	      (declare (ignore error?))
	      ,node)))

	;; Recurse into children.
	(call-next-method)))))


;;; Utility functions
;;

(defun ensure-package (name)
  "Return the `file-set-desc' instance for the package named NAME,
creating an empty `file-set-desc' instance, if necessary."
  (or (pb::find-package1 name :error? nil)
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
