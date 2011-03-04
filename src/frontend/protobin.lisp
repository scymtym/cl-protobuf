;;; protobin.lisp --- Loading descriptors in binary protocol buffer format.
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

(in-package :protocol-buffer.frontend)

;; TODO the object/class name should be optional
(defgeneric load/binary (source
			 &key
			 start
			 end
			 object)
  (:documentation
   "Note: This function is not intended for deserializing domain
protocol buffers, but for deserializing protocol buffer descriptors
which are stored as binary protocol buffers."))

(defmethod load/binary ((source t)
			&key
			(start 0)
			end
			(object (make-instance 'pb::file-set-desc))) ;; TODO call this unpack?
  "Load OBJECT from stream SOURCE."
  (apply #'pb::unpack source object start (when end (list end))))
