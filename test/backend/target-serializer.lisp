;;; target-serializer.lisp --- Unit test for the serializer target.
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
