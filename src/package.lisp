;;; package.lisp --- Definition of protocol buffer main package.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(defpackage :protocol-buffer
  (:nicknames :pb)

  (:use
   :cl
   :alexandria
   :split-sequence
   :iterate
   :bind

   :binio)

  ;; Conditions
  (:export
   :missing-required-initarg
   :missing-required-initarg-class
   :missing-required-initarg-initarg

   :name-resolution-failed
   :name-resolution-failed-name
   :name-resolution-failed-package
   :name-resolution-failed-candidates

   :encoding-error

   :decoding-error
   :decoding-error-offset

   :invalid-wire-type
   :invalid-wire-type-designator

   :unexpected-wire-type
   :unexpected-wire-type-field
   :unexpected-wire-type-expected-type
   :unexpected-wire-type-found-type

   :unhandled-field-number
   :unhandled-field-number-number
   :unhandled-field-number-class)

  ;; Wire types
  (:export
   :wire-type
   :wire-type-meaning)

  ;; Types
  (:export
   :+proto-types+

   :proto-type

   :enum-type-p :primitive-type-p
   :fixed64-p :fixed32-p :fixed-p
   :varint-p :varint-enum-p :svarint-p :uvarint-p :integer-type-p
   :length-delim-p

   :fixed-size)

  ;; Type conversions
  (:export
   :proto-type->wire-type

   :scalar-proto-type->lisp-type
   :proto-type->lisp-type)

  ;; Enum Protocol
  (:export
   :enum-symbol :enum-code)

  ;; Message protocol
  (:export
   :message-descriptor)

  ;; Protocol Buffer Protocol :)
  (:export
   :packed-size
   :pack :unpack
   :pack*

   :offset
   :extract)

  ;; Utility functions
  (:export
   :parse-name)

  ;; Protocol buffer descriptors
  (:export
   :descriptor-name
   :descriptor-qualified-name
   :descriptor-parent
   :descriptor-children
   :descriptor-file
   :descriptor-class)

  ;; Field descriptor functions
  (:export
   :field-primitive?
   :field-enum?
   :field-message?
   :field-repeated?
   :field-packed?
   :field-optional?

   :field-type-descriptor)

  ;; Finding descriptors by name
  (:export
   :find-descriptor
   :find-package1)

  ;; High-level utilities
  (:export
   :dependency-closure)

  (:documentation
   "This package contains generic protocol buffer infrastructure."))
