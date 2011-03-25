;;; target-serializer.lisp --- Unit test for the serializer target.
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

(in-package :protocol-buffer.backend.test)

(deftestsuite target-serializer-root (backend-root
				      emitter-suite)
  ()
  (:documentation
   "Unit tests for the serializer targets."))

(addtest (target-serializer-root
          :documentation
	  "Smoke test for the packed-size target.")
  smoke-packed-size

  (iter (for descriptor in descriptors)
	(let ((classes (apply #'append (prog1
					   (emit descriptor :class)
					 (emit descriptor :packed-size)))))
	  (iter (for class in classes)
		(let ((instance (make-instance class)))
		  (ensure
		   (typep (pb:packed-size instance)
			  'non-negative-integer)))))))

(addtest (target-serializer-root
          :documentation
	  "Smoke test for the serializer target.")
  smoke-pack

  (iter (for descriptor in descriptors)
	(let ((classes (apply #'append (prog1
					   (emit descriptor :class)
					 (emit descriptor :serializer)))))
	  (iter (for class in classes)
		(bind ((instance (make-instance class))
		       ((:values size buffer) (pb:pack instance)))
		  (ensure (typep size   'non-negative-integer))
		  (ensure (typep buffer 'binio:octet-vector)))))))

(addtest (target-serializer-root
          :documentation
	  "Smoke test for the deserializer target.")
  smoke-unpack

  (iter (for descriptor in descriptors)
	(let ((classes (apply #'append (prog1
					   (emit descriptor :class)
					 (emit descriptor :deserializer)))))
	  (iter (for class in classes)
		(let* ((instance  (make-instance class))
		       (roundtrip (pb:unpack (pb::pack1 instance) class)))
		  (ensure (typep roundtrip class)))))))
