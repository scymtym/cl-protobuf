;;; package.lisp --- Package definition for unit tests of the cl-protobuf system.
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

(cl:in-package :cl-user)

(defpackage :protocol-buffer.test
  (:use
   :cl
   :lift)
  (:export
   :root)
  (:documentation
   "This package contains unit tests for the cl-protobuf system"))

(in-package :protocol-buffer.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the cl-protobuf system."))

(deftestsuite data-suite ()
  ((data-1 (make-array 3
		       :element-type     '(unsigned-byte 8)
		       :initial-contents '(#x08 #x96 #x01)))
   (data-2 (make-array 9
		       :element-type     '(unsigned-byte 8)
		       :initial-contents '(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6e #x67)))
   (data-3 (make-array 5
		       :element-type     '(unsigned-byte 8)
		       :initial-contents '(#x1a #x03 #x08 #x96 #x01)))
   (data-4 (make-array 8
		       :element-type     '(unsigned-byte 8)
		       :initial-contents '(#x22 #x06 #x03 #x8E #x02 #x9E #xA7 #x05))))
  (:documentation
   "This class can be mixed into test suite classes that required
serialized protocol buffer data."))

(deftestsuite descriptor-suite ()
  ((descriptor-1 (make-instance 'developer-guide::test1
				:a 150))
   (descriptor-2 (make-instance 'developer-guide::test2
				:b "testing"))
   (descriptor-3 (make-instance 'developer-guide::test3
				:c (make-instance 'developer-guide::test1
						  :a 150)))
   (descriptor-4 (make-instance 'developer-guide::test4
				:d (make-array 3
					       :element-type     '(signed-byte 32)
					       :initial-contents '(3 270 86942)))))
  (:documentation
   "This class can be mixed into test suite classes that require
protocol buffer descriptor instances."))
