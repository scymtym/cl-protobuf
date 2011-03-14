;;; stream-target-mixin.lisp --- Mixin for stream-writing emit targets.
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

(in-package :protocol-buffer.backend)

(defclass stream-target-mixin ()
  ((stream :initarg  :stream
	   :type     stream
	   :accessor target-stream
	   :documentation
	   "The stream to which the output of the emission process
should be written."))
  (:default-initargs
   :stream (missing-required-initarg 'stream-target-mixin :stream))
  (:documentation
   "This class can be mixed into target classes which write some
output onto a stream."))

(defmacro with-stream-emit-symbols (stream-var &body body)
  "Execute BODY with the additional symbols provided by
`with-emit-symbols' and STREAM-VAR bound to the stream associated to
the current emit target. "
  `(let ((,stream-var (target-stream (context-target *context*))))
     (with-emit-symbols
       ,@body)))
