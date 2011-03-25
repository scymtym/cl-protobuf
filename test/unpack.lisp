;;; unpack.lisp --- Unit tests for the unpack method.
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

(deftestsuite unpack-root (root
			   data-suite)
  ()
  (:documentation
   "Unit tests for the `unpack' method."))

(addtest (unpack-root
          :documentation
	  "Smoke test for the `unpack' method.")
  smoke

  (multiple-value-bind (object consumed)
      (pb:unpack data-1 'developer-guide::test1)
    (ensure-same
     consumed 3
     :test #'=)
    (ensure-same
     (developer-guide::test1-a object) 150
     :test #'=))

  (multiple-value-bind (object consumed)
      (pb:unpack data-2 'developer-guide::test2)
    (ensure-same
     consumed 9
     :test #'=)
    (ensure-same
     (developer-guide::test2-b object) "testing"
     :test #'string=))

  ;; TODO default value of slot c in test3 is nil which is not a test1 instance
  ;; (multiple-value-bind (object consumed)
  ;;     (pb:unpack data-3 'developer-guide::test3)
  ;;   (ensure-same
  ;;    consumed 5
  ;;    :test #'=))

  ;; TODO default value of slot c in test3 is nil which is not a test1 instance
  ;; (multiple-value-bind (object consumed)
  ;;     (pb:unpack data-4 'developer-guide::test4)
  ;;   (ensure-same
  ;;    consumed 8
  ;;    :test #'=))
  )
