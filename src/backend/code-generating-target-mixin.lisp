;;; code-generating-target-mixin.lisp ---
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

(defclass code-generating-target-mixin ()
  ()
  (:documentation
   "This class can be used as a superclass for target classes that
represent a code generation target."))

(defmethod emit :around ((node   pb::file-desc)
			 (target code-generating-target-mixin)
			 &key)
  (let* ((package-name (pb::file-desc-package node))
	 (package      (progn
			 (maybe-make-package package-name)
			 (maybe-find-package-or-loose package-name))))
    (setf (context-package *context*) package)
    (unwind-protect
	 (handler-bind (#+sbcl (sb-c::redefinition-warning #'muffle-warning))
	   (call-next-method))
      (setf (context-package *context*) nil))))
