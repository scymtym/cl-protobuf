;;; types.lisp --- Types and Predicates used in the s-protobuf System.
;;
;; Copyright 2009-2010, Georgia Tech Research Corporation
;; All rights reserved.
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

(in-package :protocol-buffer)


;;; Wire-types
;;

(deftype wire-type () '(integer 0 5))


;;; Symbol-designated proto types
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +proto-types+ '(:bool
				:fixed32  :sfixed32
				:fixed64  :sfixed64
				:float    :double
				:int32    :uint32   :sint32
				:int64    :uint64   :sint64
				:string   :bytes)))

(deftype proto-type ()
  `(or symbol (member ,@+proto-types+)))

(defun enum-type-p (sym)
  (get sym 'enum))

(defun primitive-type-p (type)
  (or (find type +proto-types+ :test #'eq)
      (enum-type-p type)))

(defun fixed64-p (type)
  (member type '(:double :fixed64 :sfixed64)))

(defun fixed32-p (type)
  (member type '(:float :fixed32 :sfixed32)))

(defun fixed-p (type)
  (or (fixed32-p type)
      (fixed64-p type)))

(defun varint-p (type)
  (member type '(:bool
		 :int32 :sint32 :uint32
		 :int64 :sint64 :uint64
		 :enum)))

(defun varint-enum-p (type)
  (or (varint-p type)
      (enum-type-p type)))

(defun svarint-p (type)
  (member type '(:sint32 :sint64)))

(defun uvarint-p (type)
  (and (varint-p type)
       (not (svarint-p type))))

(defun integer-type-p (type)
  (or (varint-enum-p type)
      (fixed-p       type)))

(defun length-delim-p (type)
  (and (not (fixed64-p type))
       (not (fixed32-p type))
       (not (varint-p  type))))

(defun fixed-size (type)
  (cond
    ((fixed32-p type) 4)
    ((fixed64-p type) 8)
    (t                (error "Not a fixed type: ~A" type))))


;;; Type conversions
;;

(defun proto-type->wire-type (type &optional repeated? packed?)
  (check-type type proto-type "a protocol buffer type designator")

  (cond
    ((and repeated? packed?) 2)
    ((varint-enum-p type)    0)
    ((fixed64-p type)        1)
    ((fixed32-p type)        5)
    ((length-delim-p type)   2)
    (t                       2)))

(defun scalar-proto-type->lisp-type (type)
  (check-type type proto-type "a protocol buffer type designator")

  (case type
    ((:int32  :sfixed32 :sint32) '(cl:signed-byte 32))
    ((:uint32 :fixed32)          '(cl:unsigned-byte 32))
    ((:int64  :sfixed64 :sint64) '(cl:signed-byte 64))
    ((:uint64 :fixed64)          '(cl:unsigned-byte 64))
    (:bool                       'cl:boolean)
    (:double                     'cl:double-float)
    (:float                      'cl:single-float)
    (:string                     'cl:string)
    (:bytes                      '(cl:simple-array (cl:unsigned-byte 8) *))
    ((enum-type-p type)          type)   ;; enum
    (t                           type))) ;; message

(defun proto-type->lisp-type (type &optional repeated? optional?)
  (flet ((maybe-repeated (base-type)
	   (if repeated? `(array ,base-type *) base-type))
	 (maybe-optional (base-type)
	   (if optional? `(or null ,base-type) base-type)))
    (maybe-optional
     (maybe-repeated
      (scalar-proto-type->lisp-type type)))))
