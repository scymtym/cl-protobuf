;;; generator-macros.lisp --- Macros for code generation.
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

(defmacro do-fields (((buffer-var start end offset-var
		       &optional
		       (number-var    (gensym "NUMBER"))
		       (wire-type-var (gensym "WIRE-TYPE")))
		      &optional return)
		     &body body)
  "Iterate through packed fields in the range from START to END in the
buffer that is the value of BUFFER-VAR. For each field, execute BODY with
+ OFFSET-VAR bound the offset of the current field
+ NUMBER-VAR bound to the field number of the current field
+ WIRE-TYPE-VAR bound to the wire-type of the current field.
If supplied, the RETURN form determined the return value or values of
the whole form."
  (check-type buffer-var    symbol "a symbol")
  (check-type offset-var    symbol "a symbol")
  (check-type number-var    symbol "a symbol")
  (check-type wire-type-var symbol "a symbol")

  (with-unique-names (start-code-length)
    `(do ((,offset-var ,start))
	 ((>= ,offset-var ,end)
	  ,@(when return `(,return)))
       (declare (type non-negative-fixnum ,offset-var))

       (bind (((:values ,number-var ,wire-type-var ,start-code-length)
	       (pb::read-start-code ,buffer-var ,offset-var)))
	 (declare (type non-negative-fixnum ,number-var ,wire-type-var)
		  (ignorable ,number-var ,wire-type-var))
	 ;; Increase offset to account for consumed start-code.
	 (incf ,offset-var ,start-code-length)
	 ,@body))))
