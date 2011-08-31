;;; code-generating-target-mixin.lisp ---
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

(defclass code-generating-target-mixin ()
  ((optimization-settings :initarg  :optimization-settings
			  :type     list
			  :accessor target-optimzation-settings
			  :initform nil
			  :documentation
			  "Optimization settings that should be used
when generating code.")
   (export?               :initarg  :export?
			  :type     boolean
			  :accessor target-export?
			  :initform t
			  :documentation
			  "Controls whether symbols by which generated
code is designated should be exported."))
  (:documentation
   "This class can be used as a superclass for target classes that
represent a code generation target."))

(defmethod emit ((node   file-desc)
		 (target code-generating-target-mixin)
		 &key)
  (with-emit-symbols
    (nconc
     (map 'list #'recur (pb::file-desc-enum-type    node))
     (map 'list #'recur (pb::file-desc-message-type node)))))

(defmethod emit :around ((node   file-desc)
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
