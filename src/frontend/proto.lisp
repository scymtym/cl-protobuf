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
;; extend     ::= "extend" userType "{" ( field | group | ";" )* "}"
;; service    ::= "service" ident "{" ( option | rpc | ";" )* "}"
;; rpc        ::= "rpc" ident "(" userType ")" "returns" "(" userType ")" ";"
;; group      ::= modifier "group" camelIdent "=" intLit messageBody

(in-package :protocol-buffer.frontend)


;;; Parser implementation
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +ignored-chars+    (coerce '(#\Newline #\Space #\Tab #\/) 'string))
  (defvar +number-chars+     "e01234567890+-.")
  (defvar +identifier-chars+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
  (defvar +punctuation+      "=,;.{}[]")

  (defvar +keywords+ '(:to :max
		       :optional :required :repeated
		       :syntax :import :package :option :extensions
		       :message :enum :service :default))

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
    (let ((package  (second (find-if (curry #'starts-with :package) parser-output)))
	  (includes (mapcar #'second
			    (remove-if-not (curry #'starts-with :import)
					   parser-output))))
      (apply #'make-instance 'file-desc
	     :name         name
	     :dependency   (coerce includes 'vector)
	     :message-type (%children-of-type 'message-desc parser-output)
             :enum-type    (%children-of-type 'enum-desc    parser-output)
	     (when package
	       (list :package package)))))

  (defun make-message (name children)
    (make-instance 'message-desc
		   :name        name
		   :nested-type (%children-of-type 'message-desc children)
		   :enum-type   (%children-of-type 'enum-desc    children)
		   :field       (%children-of-type 'field-desc   children)))

  (defun make-field (label type name number options)
    (bind (((default other-options) options))
      (apply #'make-instance 'field-desc
	     :name      name
	     :number    number
	     :type      (if (stringp type) :enum type)
	     :type-name (string type)
	     :label     label
	     :options   other-options
	     (when default
	       (list :default-value (princ-to-string default))))))

  (defun make-field-options (children)
    (list
     (%option-value :default children)
     (make-instance 'field-options
		    :packed (string= (%option-value "packed" children)
				     "true"))))

  (defun make-enum (name values)
    (make-instance 'enum-desc
		   :name  name
		   :value (coerce values 'vector)))

  (defun make-option (name value)
    (list 'option name value))

  (defun make-enum-value (name number)
    (make-instance 'enum-value-desc
		   :name   name
		   :number number))

  (defun %index-filter (func &rest indices)
    #'(lambda (&rest args)
	(apply func (remove-if-not (rcurry #'member indices :test #'=) args
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
		   (single (symbolicate base "->")))
	      `(,rule
		( ,single ,rule #'%cleaning-cons )
		( )))))
      (iter (for rule next (find-if #'list-rule? rules))
	    (while rule)
	    (setf rules (substitute (expand-rule rule) rule rules)))
      `(yacc:define-parser *proto-parser*
	 (:start-symbol proto*)
	 (:terminals    (,@(map 'list #'%intern-char +punctuation+)
			 :type :ident :%string :%number :%bool
			 ,@+keywords+))
	 ,@rules)))

  (define-parser
      proto*

    (proto->
     syntax-> import-> package-> option-> message->  enum->
     ;extend->
     ( :SEMICOLON (constantly (values)) ))

    (syntax->
     ( :syntax :EQUALS_SIGN :%string :SEMICOLON ))

    (import->
     ( :import :%string :SEMICOLON (%index-filter #'list 0 1) ))

    (package->
     ( :package dotted-ident-> :SEMICOLON (%index-filter #'list 0 1) ))

    (option->
     ( :option option-body-> :SEMICOLON (%index-filter #'identity 1) ))

    (option-body->
     ( dotted-ident-> :EQUALS_SIGN constant->
       (%index-filter #'make-option 0 2) ))

    (message->
     ( :message :ident message-body->
       (%index-filter #'make-message 1 2) ))

    (message-body->
     ( :LEFT_CURLY_BRACKET message-body-element* :RIGHT_CURLY_BRACKET
       (%index-filter #'identity 1)) )

    message-body-element*

    (message-body-element->
     field-> enum-> message-> extensions->
     ; extend-> group->
     option->
     ( :SEMICOLON (constantly (values)) ))

    (field->
     ( modifier-> type-> :ident :EQUALS_SIGN :%number field-options-> :SEMICOLON
       (%index-filter #'make-field 0 1 2 4 5) ))

    (modifier->
     :required :optional :repeated)

    (type->
     :type user-type->)

    (user-type->
     dotted-ident->
     ( :FULL_STOP dotted-ident-> (%index-filter (curry #'format nil ".~A") 1) ))

    (field-options->
     ( :LEFT_SQUARE_BRACKET field-option-list-> :RIGHT_SQUARE_BRACKET
       (%index-filter #'make-field-options 1) )
     ( (constantly (make-field-options nil)) ))

    (field-option-list->
     ( field-option-> :COMMA field-option-list-> (%index-filter #'cons 0 2) )
     ( field-option-> #'list ))

    (field-option->
     option-body->
     ( :default :EQUALS_SIGN constant-> (%index-filter #'make-option 0 2) ))

    (extensions->
     ( :extensions extension-range-list-> :SEMICOLON (constantly nil) ))

    (extension-range-list->
     (  extension-range-> :COMMA extension-range-list-> )
     extension-range->)

    (extension-range->
     :%number
     ( :%number :to :%number )
     ( :%number :to :max ))

    (enum->
     ( :enum :ident :LEFT_CURLY_BRACKET enum-body-element* :RIGHT_CURLY_BRACKET
       (%index-filter #'make-enum 1 3) ))

    enum-body-element*

    (enum-body-element->
     option->
     ( :ident :EQUALS_SIGN :%number :SEMICOLON
       (%index-filter #'make-enum-value 0 2) )
     ( :SEMICOLON (constantly (values)) ))

    (constant->
     :ident :%string :%number :%bool)

    (dotted-ident->
     :ident
     ( :ident :FULL_STOP dotted-ident->
       (%index-filter (curry #'format nil "~A.~A") 0 2 )))))


;;; Lexer
;;

(defun make-char-reader (stream)
  "Return three values: a read-char function, an unread-char function
and a position function. The read-char function reads a single char
from STREAM, skipping over comments. The unread-char function behaves
as usual. The position function returns three values: the current
offset, line and column in STREAM. "
  (bind ((offset      0)
	 (line        1)
	 (column      0)
	 (in-comment? nil)
	 (did-unread? nil)
	 ((:flet read1 (&key (skip-comments? t)))
	  (iter (for c next (read-char stream nil :eof))
		(unless did-unread?
		  (incf offset)
		  (incf column)
		  (case c
		    (#\Newline (incf line)
			       (setf column 0)
			       (when (eq in-comment? :/)
				 (setf in-comment? nil)))
		    (#\/       (when skip-comments?
				 (case in-comment?
				   ((nil) (setf in-comment? t))
				   ((t)   (setf in-comment? :/))
				   (:**   (setf in-comment? nil)))))
		    (#\*       (case in-comment?
				 ((t) (setf in-comment? :*))
				 (:*  (setf in-comment? :**))))))
		(setf did-unread? nil)
		(while (and (not (eq c :eof)) in-comment?))
		(finally (return c))))
	 ((:flet unread (c))
	  (setf did-unread? t)
	  (unread-char c stream)))
    (values #'read1 #'unread #'(lambda () (values offset line column)))))

(defun make-stream-lexer (stream)
  (bind (((:values read unread position) (make-char-reader stream))
	 ((:flet read-while (allowed-chars &key invert? (comments? t)))
	  (iter (for c next (funcall read :skip-comments? comments?))
		(cond
		  ((eq c :eof) (terminate))
		  ((find c allowed-chars
			 :test (if invert? (complement #'eq) #'eq))
		               (collect c :result-type string))
		  (t           (funcall unread c)
			       (terminate)))))
	 ((:flet read-number ())
	  (values :%number (read-from-string (read-while +number-chars+))))
	 ((:flet read-string ())
	  (funcall read) ;; consume opening quote
	  (multiple-value-prog1
	      (values :%string (read-while "\"" :invert? t :comments? nil))
	    (funcall read))) ;; consume closing quote
	 ((:flet read-identifier-like ())
	  (let* ((string  (read-while +identifier-chars+))
		 (keyword (find-symbol (string-upcase string) :keyword)))
	    (cond
	      ((find keyword +keywords+)       (values keyword keyword))
	      ((find keyword pb:+proto-types+) (values :type   keyword))
	      ((eq keyword :true)	       (values :%bool  "true"))
	      ((eq keyword :false)	       (values :%bool  "false"))
	      (t                               (values :ident  string))))))
    (values
     #'(lambda ()
	 (read-while +ignored-chars+)
	 (let ((c (peek-char t stream nil :eof)))
	   (cond
	     ((eq c :eof)                 nil)
	     ((find c +punctuation+)      (progn
					    (funcall read)
					    (values (%intern-char c) c)))
	     ((find c +number-chars+
		    :start 1)             (read-number))
	     ((eq c #\")                  (read-string))
	     ((find c +identifier-chars+) (read-identifier-like))
	     (t                           (error "~@<Invalid character `~A'~@:>" c)))))
     position)))

;; numeric literals should be
;; intLit ::= decInt | hexInt | octInt
;;   decInt ::= /\d+/
;;   hexInt ::= /0[xX]([A-Fa-f0-9])+/
;;   octInt ::= /0[0-7]+/
;; floatLit ::= /\d+(\.\d+)?([Ee][\+-]?\d+)?/ # allow_f_after_float_ is

;; string literals should be
;; strLit ::= quote ( hexEscape | octEscape | charEscape | /[^\0\n]/ )* quote
;;   quote ::= /["']/
;;   hexEscape ::= /\\[Xx][A-Fa-f0-9]{1,2}/
;;   octEscape ::= /\\0?[0-7]{1,3}/
;;   charEscape ::= /\\[abfnrtv\\\?'"]/


;;;
;;

(defun parse (source)
  "The the contents of the stream SOURCE and return the resulting
partially post-processed syntax tree."
  (bind (((:values lexer position1) (make-stream-lexer source)))
    (handler-case
	(yacc:parse-with-lexer lexer *proto-parser*)
      ((or yacc:yacc-runtime-error simple-error) (condition)
	(bind (((:values offset line column) (funcall position1)))
	  (error 'proto-parse-error
		 :offset            offset
		 :line              line
		 :column            column
		 :causing-condition condition))))))


;;; Public interface
;;

(defgeneric load/text (source)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description.
Return a `file-set-desc' instance that contains the complete
description in its child nodes.

Note: Currently, the grammar accepts only a (quite usable) subset of
the textual protocol buffer description language.

Note: This function does not load data in a protocol buffer format,
but textual protocol buffer descriptors using the grammar described at
http://code.google.com/apis/protocolbuffers/docs/proto.html."))

(defmethod load/text ((source stream))
  (make-instance
   'file-set-desc
   :file (make-array 1
		     :initial-element (make-file-desc
				       "<stream>" (parse source))
		     :fill-pointer    1)))

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
collects the resulting `file-desc' instance in one `file-set-desc'."
  (bind (((result &rest rest) (map 'list #'load/text source))
	 ((:flet merge-one (desc))
	  (vector-push-extend
	   (aref (pb::file-set-desc-file desc) 0)
	   (pb::file-set-desc-file result))))
    (map nil #'merge-one rest)
    result))

(defmethod load/text ((source asdf::protocol-buffer-descriptor))
  "Load a textual protocol buffer description from the ASDF component
SOURCE."
  (load/text (asdf:component-pathname source)))
