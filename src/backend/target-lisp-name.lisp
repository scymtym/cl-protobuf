;;; target-lisp-name.lisp --- Generation of Lisp names from descriptor nodes.
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

(defmethod documentation ((thing (eql :lisp-name)) (type (eql 'target)))
  "Generate Lisp packages and symbols names for protocol buffer
descriptor nodes.")


;;; Target class
;;

(defclass target-lisp-name ()
  ((result-type :initarg  :result-type
		:type     (member symbol string)
		:reader   target-result-type
		:initform 'symbol
		:documentation
		"Stores the desired result type of the name
generation.")
   (nested?     :initarg  :nested?
		:type     boolean
		:reader   target-nested?
		:initform t
		:documentation
		"Controls whether names of containing descriptors
should be included in generated names."))
  (:documentation
   "Target class for generating Lisp names."))


;;; Emitter methods for `target-lisp-name'
;;

(defmethod emit :around ((node   standard-object)
			 (target target-lisp-name)
			 &key)
  (ecase (target-result-type target)
    (symbol
     (bind (((:accessors-r/o (nested? target-nested?)) target)
	    ((:values package name)
	     (emit node `(:lisp-name :result-type string
				     :nested?     ,nested?))))
       (intern name package)))
    (string
     (call-next-method))))

(defmethod emit ((node   file-desc)
		 (target target-lisp-name)
		 &key)
  (values (maybe-make-package (pb::file-desc-package node)) nil))

(defmethod emit ((node   enum-desc)
		 (target target-lisp-name)
		 &key)
  (let ((container (descriptor-parent node))
	(suffix    (pb::->lisp-name (descriptor-name node))))
    (bind (((:values package name)
	    (emit container '(:lisp-name :result-type string))))
      (values package (format nil "~@[~A/~]~A" name suffix)))))

(defmethod emit ((node   message-desc)
		 (target target-lisp-name)
		 &key)
  (let ((container (descriptor-parent node))
	(suffix    (pb::->lisp-name (descriptor-name node))))
    (bind (((:values package name)
	    (emit container '(:lisp-name :result-type string))))
      (values package (format nil "~@[~A/~]~A" name suffix)))))

(defmethod emit ((node   field-desc)
		 (target target-lisp-name)
		 &key)
  (bind (((:accessors-r/o (nested? target-nested?)) target)
	 (container (descriptor-parent node))
	 (suffix    (pb::->lisp-name (descriptor-name node))))
    (bind (((:values package name)
	    (emit container '(:lisp-name :result-type string))))
      (values package (format nil "~:[~*~;~A-~]~A" nested? name suffix)))))
