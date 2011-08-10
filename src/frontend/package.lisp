;;; package.lisp --- Definition of the frontend package.
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

(cl:in-package :cl-user)

(defpackage :protocol-buffer.frontend
  (:nicknames :pbf)

  (:use
   :cl
   :alexandria
   :iterate
   :bind

   :protocol-buffer)

  ;; Conditions
  (:export
   :proto-parse-error
   :proto-parse-error-line
   :proto-parse-error-column
   :proto-parse-error-offset
   :proto-parse-error-causing-condition

   :import-error
   :import-error-import

   :cannot-resolve-import
   :import-error-locations

   :ambiguous-import
   :import-error-candidates)

  ;; Variables
  (:export
   :*proto-load-path*)

  ;; S-expr frontend
  (:export
   :define-message

   :process-message
   :process-field
   :process-enum
   :process-enum-value)

  ;; Binary descriptor format
  (:export
   :load/binary)

  ;; Textual descriptor format
  (:export
   :load/text

   :load-from-path)

  (:documentation
   "The frontend package of the protocol buffer system contains
mechanism for getting protocol buffer descriptor into the Lisp system
from different sources."))
