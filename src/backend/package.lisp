
(cl:in-package :cl-user)

(defpackage :protocol-buffer.backend
  (:nicknames :pbb)
  (:use
   :cl
   :alexandria
   :metabang-bind
   :iterate

   :protocol-buffer)

  ;; Conditions
  (:export
   :no-coder
   :no-coder-type)

  ;; Code generation
  (:export
   :generate-packed-size-method
   :generate-pack-method
   :generate-unpack-method)

  ;; Class generation
  (:export
   :generate-enum
   :generate-class)

  ;; Generic emitter
  (:export
   :emit)

  (:documentation
   "This package contains backends and backend-related functions of
the protocol buffer compiler."))
