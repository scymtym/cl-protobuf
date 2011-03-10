;;; pack.lisp --- Unit tests for the pack method.
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
