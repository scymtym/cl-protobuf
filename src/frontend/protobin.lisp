;;; protobin.lisp --- Loading descriptors in binary protocol buffer format.
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

(in-package :protocol-buffer.frontend)

(defgeneric load/binary (source
			 &key
			 start
			 end
			 object)
  (:documentation
   "Load protocol buffer descriptors from binary representation stored
in SOURCE. Return a `pb:file-set-desc' instance that contains the
complete description in its child nodes.
When non-nil, START specifies an offset into the data of SOURCE at
which the loading should start.
When non-nil, END specifies an offset into the data of SOURCE at which
the loading should stop.
When OBJECT is non-nil, the protocol buffer descriptors which are
loaded from SOURCE are stored in OBJECT. Usually, OBJECT should be an
instance of `pb:file-set-desc'.

Note: This function is not intended for deserializing data stored
in protocol buffer format, but for deserializing protocol buffer
descriptors which are stored as binary protocol buffers."))

(defmethod load/binary ((source t)
			&key
			(start 0)
			end
			(object (make-instance 'pb:file-set-desc)))
  "Load OBJECT from stream SOURCE."
  (prog1
      (apply #'pb:unpack source object start (when end (list end)))
    (pbb:emit object :relations)))

(defmethod load/binary ((source list)
			&key &allow-other-keys)
  "Load and merge descriptor from all files in the list SOURCE."
  (bind ((descs  (map 'list #'load/binary source))
	 (result (first descs))
	 ((:flet merge-one (file))
	  (vector-push-extend
	   file (pb::file-set-desc-file result)))
	 ((:flet merge-all (desc))
	  (map nil #'merge-one (pb::file-set-desc-file desc))))
    (map nil #'merge-all (rest descs))
    result))
