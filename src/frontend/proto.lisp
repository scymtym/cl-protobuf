;;; proto.lisp --- Parser for textual protocol buffer descriptions.
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
;;
;; The grammar is based on:
;; http://groups.google.com/group/protobuf/browse_thread/thread/1cccfc624cd612da
;;
;; This part of the grammar is not implemented:
;;
;; extend     ::= "extend" userType messageBody
;; service    ::= "service" ident "{" ( option | rpc | ";" )* "}"
;; rpc        ::= "rpc" ident "(" userType ")" "returns" "(" userType ")" ";"
;; group      ::= modifier "group" camelIdent "=" intLit messageBody
;; extensions ::= "extensions" intLit "to" ( intLit | "max" ) ";"
;; # tag number must be 2^28-1 or lower

(in-package :protocol-buffer.frontend)


;;; Parser implementation
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +number-chars+ "01234567890.")

  (defvar +punctuation+ "=,;.{}[]")

  (defvar +keywords+ '(optional required repeated
		       syntax import package option
		       message enum service default))

  (defun %children-of-type (type list)
    (coerce (remove-if-not (of-type type) list) 'vector))

  (defun %find-option (name list)
    (find-if #'(lambda (element)
		 (and (starts-with 'option element)
		      (equal (second element) name)))
	     list))

  (defun %option-value (name list)
    (third (%find-option name list)))

  (defun make-file-desc (name parser-output)
    (let ((package  (second (find-if (curry #'starts-with 'package) parser-output)))
	  (includes (mapcar #'second
			    (remove-if-not (curry #'starts-with 'import)
					   parser-output))))
      (apply #'make-instance 'pb::file-desc
	     :name         name
	     :dependency   (coerce includes 'vector)
	     :message-type (%children-of-type 'pb::message-desc parser-output)
             :enum-type    (%children-of-type 'pb::enum-desc    parser-output)
	     (when package
	       (list :package package)))))

  (defun make-message (name children)
    (make-instance 'pb::message-desc
		   :name        name
		   :nested-type (%children-of-type 'pb::message-desc children)
		   :enum-type   (%children-of-type 'pb::enum-desc    children)
		   :field       (%children-of-type 'pb::field-desc   children)))

  (defun make-field (label type name number options)
    (bind (((default other-options) options))
      (apply #'make-instance 'pb::field-desc
	     :name      name
	     :number    number
	     :type      (if (stringp type) :enum type)
	     :type-name type
	     :label     label
	     :options   other-options
	     (when default
	       (list :default-value default)))))

  (defun make-field-options (children)
    (list
     (%option-value 'default children)
     (make-instance 'pb::field-options
		    :packed (%option-value "packed" children))))

  (defun make-enum (name values)
    (make-instance 'pb::enum-desc
		   :name  name
		   :value (coerce values 'vector)))

  (defun make-option (name value)
    (list 'option name value))

  (defun make-enum-value (name number)
    (make-instance 'pb::enum-value-desc
		   :name   name
		   :number number))

  (defun %index-filter (func &rest indices)
    #'(lambda (&rest args)
	(apply func (remove-if-not (rcurry #'member indices) args
				   :key (rcurry #'position args)))))

  (defun %cleaning-cons (first rest)
    (cond ((and first (first rest)) (cons first rest))
	  (first                    (list first))
	  (t                        rest)))

  (defun %intern-char (char)
    (intern (char-name char) :keyword))

  (defmacro define-parser (&body rules)
    (bind (((:flet list-rule? (rule))
	    (and (symbolp rule) (ends-with #\* (string rule))))
	   ((:flet expand-rule (rule))
	    (let* ((string (string rule))
		   (base   (subseq string 0 (1- (length string))))
		   (single (intern (concatenate 'string base "->") :pbf)))
	      `(,rule
		( ,single ,rule #'%cleaning-cons )
		( ,single )))))
      (iter (for rule next (find-if #'list-rule? rules))
	    (while rule)
	    (setf rules (substitute (expand-rule rule) rule rules)))
      `(yacc:define-parser *proto-parser*
	 (:start-symbol proto*)
	 (:terminals    (,@(map 'list #'%intern-char +punctuation+)
			 :type :ident :string :number :bool
			 ,@+keywords+))
	 ,@rules)))

  (define-parser
      proto*

    (proto->
     syntax-> #||# import-> #||# package-> #||# option-> #| ;extend-> |# message-> #||# enum->
     ( :SEMICOLON (constantly (values)) ))

    (syntax->
     ( syntax :EQUALS_SIGN :string :SEMICOLON ))

    (import->
     ( import :string :SEMICOLON (%index-filter #'list 0 1) ))

    (package->
     ( package :ident :SEMICOLON (%index-filter #'list 0 1) ))

    (option->
     ( option option-body-> :SEMICOLON (%index-filter #'identity 1) ))

    (option-body->
     ( :ident
					;( "." ident )*
       :EQUALS_SIGN constant-> (%index-filter #'make-option 0 2) ))

    (message->
     ( message :ident message-body-> (%index-filter #'make-message 1 2) ))

    (message-body->
     ( :LEFT_CURLY_BRACKET message-body-element* :RIGHT_CURLY_BRACKET
			   (%index-filter #'identity 1)) )

    message-body-element*

    (message-body-element->
     field-> #||# enum-> #||# message->
     #|;extend-> ;extensions-> ;group->|#
     option->
     ( :SEMICOLON (constantly (values)) ))

    (field->
     ( modifier-> type-> :ident :EQUALS_SIGN :number field-options-> :SEMICOLON
		  (%index-filter #'make-field 0 1 2 4 5) ))

    (modifier->
     required #||# optional #||# repeated)

    (type->
     :type #||# user-type->)

    (user-type->
     ( :FULL_STOP :ident (%index-filter (curry #'format nil ".~A") 1) )
     :ident)

    (field-options->
     ( :LEFT_SQUARE_BRACKET field-option-list-> :RIGHT_SQUARE_BRACKET
			    (%index-filter #'make-field-options 1) )
     ( (constantly (make-field-options nil)) ))

    (field-option-list->
     ( field-option-> :COMMA field-option-list-> (%index-filter #'cons 0 2) )
     ( field-option-> ))

    (field-option->
     option-body->
     ( default :EQUALS_SIGN constant-> (%index-filter #'make-option 0 2) ))

    (enum->
     ( enum :ident :LEFT_CURLY_BRACKET enum-body-element* :RIGHT_CURLY_BRACKET
	    (%index-filter #'make-enum 1 3) ))

    enum-body-element*

    (enum-body-element->
     option->
     ( :ident :EQUALS_SIGN :number :SEMICOLON
	      (%index-filter #'make-enum-value 0 2) )
     ( :SEMICOLON (constantly (values)) ))

    (constant->
     :ident #||# :string #||# :number #||# :bool)))

(defun make-stream-lexer (stream)
  (bind (((:flet skip-comment ())
	  (iter (for c next (read-char stream nil :eof))
		(until (member c '(#\Newline :eof)))))
	 ((:flet read-one ())
	  (iter (for c next (read-char stream nil :eof))
		(while (member c '(#\Space #\Tab #\Newline #\/)))
		(when (eq c #\/) (skip-comment))
		(finally (return c))))
	 ((:flet read-number ())
	  (read-from-string
	   (iter (for c next (read-char stream nil :eof))
		 (while (find c +number-chars+))
		 (collect c :result-type string)
		 (finally (unread-char c stream)))))
	 ;; numeric literals should be
	 ;; intLit ::= decInt | hexInt | octInt
	 ;;   decInt ::= /\d+/
	 ;;   hexInt ::= /0[xX]([A-Fa-f0-9])+/
	 ;;   octInt ::= /0[0-7]+/
	 ;; floatLit ::= /\d+(\.\d+)?([Ee][\+-]?\d+)?/ # allow_f_after_float_ is
	 ((:flet read-identifier-or-keyword-or-type-chars ())
	  (iter (for c next (read-char stream nil :eof))
		(until (member c '(#\Space #\Tab #\Newline #\/ #\; #\] #\,)))
		(collect c :result-type string)
		(finally (unread-char c stream))))
	 ((:flet read-identifier-or-keyword-or-type ())
	  (let* ((string  (read-identifier-or-keyword-or-type-chars))
		 (symbol  (find-symbol (string-upcase string) :pbf))
		 (keyword (find-symbol (string-upcase string) :keyword)))
	    (cond
	      ((find symbol +keywords+)
	       (values symbol symbol))
	      ((find keyword pb:+proto-types+)
	       (values :type keyword))
	      ((or (string= string "false") (string= string "true"))
	       (values :bool string))
	      (t
	       (values :ident string)))))) ;; should be /[A-Za-z_][\w_]*/
    #'(lambda ()
	(let ((c (read-one)))
	  (cond
	    ((eq c :eof) nil)
	    ((find c +punctuation+)
	     (values (%intern-char c) c))
	    ((find c +number-chars+)
	     (unread-char c stream)
	     (values :number (read-number)))
	    ((eq c #\")
	     (unread-char c stream)
	     (values :string (read stream)))
	    ;; string literals should be
	    ;; strLit ::= quote ( hexEscape | octEscape | charEscape | /[^\0\n]/ )* quote
	    ;;   quote ::= /["']/
	    ;;   hexEscape ::= /\\[Xx][A-Fa-f0-9]{1,2}/
	    ;;   octEscape ::= /\\0?[0-7]{1,3}/
	    ;;   charEscape ::= /\\[abfnrtv\\\?'"]/
	    (t
	     (unread-char c stream)
	     (read-identifier-or-keyword-or-type)))))))


;;; Public interface
;;

(defgeneric load/text (source)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description.
Return a `pb::file-set-desc' instance that contains the complete
description in its child nodes.
Note: Currently, the grammar accepts only a (quite usable) subset of
the textual protocol buffer description language.
Note: This function does not load domain protocol buffers, but textual
protocol buffer descriptors using the grammar described at
http://code.google.com/apis/protocolbuffers/docs/proto.html."))

(defmethod load/text ((source stream))
  (let* ((lexer  (make-stream-lexer source))
	 (parsed (yacc:parse-with-lexer lexer *proto-parser*))
	 (result (make-instance 'pb::file-set-desc)))
    (setf (pb::file-set-desc-file result)
	  (make-array 1
		      :initial-element (make-file-desc "<stream>" parsed)
		      :fill-pointer    1))
    result))

(defmethod load/text ((source pathname))
  (let* ((set  (with-input-from-file (stream source)
		 (load/text stream)))
	 (file (aref (pb::file-set-desc-file set) 0)))
    (setf (pb::file-desc-name file)
	  (concatenate 'string (pathname-name source) ".proto"))
    set))

(defmethod load/text ((source string))
  (load/text (parse-namestring source)))

(defmethod load/text ((source list))
  "This method read descriptions from all files in the list SOURCE and
collects the resulting `pb::file-desc' instance in one
`pb::file-set-desc'."
  (bind (((result &rest rest) (map 'list #'load/text source))
	 ((:flet merge-one (desc))
	  (vector-push-extend
	   (aref (pb::file-set-desc-file desc) 0)
	   (pb::file-set-desc-file result))))
    (map nil #'merge-one rest)
    result))
