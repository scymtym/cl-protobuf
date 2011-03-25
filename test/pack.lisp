;;; pack.lisp --- Unit tests for the pack method.
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

(in-package :protocol-buffer.test)

(deftestsuite pack-root (root
			 descriptor-suite)
  ()
  (:documentation
   "Unit tests for the `pack' method."))

(addtest (pack-root
          :documentation
	  "Smoke test for the `pack' method.")
  smoke

  (ensure-same
   (pb:pack descriptor-1)
   (values 3 #(#x08 #x96 #x01))
   :test #'equalp)

  (ensure-same
   (pb:pack descriptor-2)
   (values 9 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6e #x67))
   :test #'equalp)

  (ensure-same
   (pb:pack descriptor-3)
   (values 5 #(#x1a #x03 #x08 #x96 #x01))
   :test #'equalp)

  (ensure-same
   (pb:pack descriptor-4)
   (values 8 #(#x22 #x06 #x03 #x8E #x02 #x9E #xA7 #x05))
   :test #'equalp))
