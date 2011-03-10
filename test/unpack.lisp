;;; unpack.lisp --- Unit tests for the unpack method.
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
