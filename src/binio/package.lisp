;;; package.lisp ---
;;
;; Copyright (C) 2009 Georgia Tech Research Corporation
;; Copyright (C) 2010, 2011 Jan Moringen
;;
;; Author: Neil T. Dantam
;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(defpackage :binio
  (:use
   :cl
   :alexandria)

  ;; Octet vector type and constructors
  (:export
   :octet :octet-vector :make-octet-vector)

  ;; Fixed-width en- and decoders
  (:export
   :encode-bool      :decode-bool

   :encode-double-le :decode-double-le
   :encode-double-be :decode-double-be
   :encode-float-le  :decode-float-le
   :encode-float-be  :decode-float-be

   :encode-uint32-le :decode-uint32-le
   :encode-uint32-be :decode-uint32-be
   :encode-sint32-le :decode-sint32-le
   :encode-sint32-be :decode-sint32-be
   :encode-uint64-le :decode-uint64-le
   :encode-uint64-be :decode-uint64-be
   :encode-sint64-le :decode-sint64-le
   :encode-sint64-be :decode-sint64-be)

  ;; Variable-width en- and decoders
  (:export
   :encode-svarint   :decode-svarint :svarint-size
   :encode-uvarint   :decode-uvarint :uvarint-size

   :encode-utf8      :decode-utf8    :utf8-size)
  (:documentation
   "This package contains functions for binary conversion between
fundamental Lisp types and the corresponding protocol buffer types."))
