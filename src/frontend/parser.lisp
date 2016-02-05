;;; parser.lisp --- Parser the textual protocol buffer descriptor syntax.
;;
;; Copyright (C) 2011, 2012, 2013, 2016 Jan Moringen
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

(eval-when (:compile-toplevel :load-toplevel :execute)

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
					   parser-output)))
	  (options  (%children-of-type '(cons (eql option) t) parser-output)))
      (apply #'make-instance 'file-desc
	     :name         name
	     :dependency   (coerce includes 'vector)
	     :message-type (%children-of-type 'message-desc parser-output)
             :enum-type    (%children-of-type 'enum-desc    parser-output)
	     (append
	      (unless (emptyp options)
		(list :options
		      (make-array 1
				  :initial-element (make-file-options options)
				  :adjustable      t
				  :fill-pointer    1)))
	      (when package
		(list :package package))))))

  (defun make-file-options (options)
    (let ((java-package         (%find-option "java_package" options))
	  (java-outer-classname (%find-option "java_outer_classname" options)))
     (apply #'make-instance 'file-options
	    (append
	     (when java-package
	       (list :java-package (third java-package)))
	     (when java-outer-classname
	       (list :java-outer-classname (third java-outer-classname)))))))

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
    (bind ((keywords-as-string
	    (iter (for keyword in (cons :type +keywords+))
		  (collect `(,keyword #'string))))
	   ((:flet list-rule? (rule))
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
      (iter (for rule next (find :keyword-as-string rules :test #'member))
	    (while rule)
	    (setf (cdr (nthcdr (1- (position :keyword-as-string rule)) rule)) ;; seriously?
		  keywords-as-string))
      `(yacc:define-parser *proto-parser*
	 (:start-symbol proto*)
	 (:terminals    (,@(map 'list #'%intern-char +punctuation+)
			 :type :ident :%string :%number :%bool
			 ,@+keywords+))
	 (:muffle-conflicts t)
	 ,@rules)))

  (define-parser
    proto*

    (proto->
     syntax-> import-> package-> option-> message->  enum->
     ;; extend->
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
     ( :message ident-even-if-keyword-> message-body->
       (%index-filter #'make-message 1 2) ))

    (message-body->
     ( :LEFT_CURLY_BRACKET message-body-element* :RIGHT_CURLY_BRACKET
       (%index-filter #'identity 1)) )

    message-body-element*

    (message-body-element->
     field-> enum-> message-> extensions->
     ;; extend-> group->
     option->
     ( :SEMICOLON (constantly (values)) ))

    (field->
     ( modifier-> type-> ident-even-if-keyword-> :EQUALS_SIGN :%number field-options-> :SEMICOLON
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
     ( :enum ident-even-if-keyword-> :LEFT_CURLY_BRACKET enum-body-element* :RIGHT_CURLY_BRACKET
       (%index-filter #'make-enum 1 3) ))

    enum-body-element*

    (enum-body-element->
     option->
     ( ident-even-if-keyword-> :EQUALS_SIGN :%number :SEMICOLON
       (%index-filter #'make-enum-value 0 2) )
     ( :SEMICOLON (constantly (values)) ))

    (constant->
     :ident :%string :%number :%bool
     :keyword-as-string) ;; interpreted by the macro

    (ident-even-if-keyword->
     :ident :keyword-as-string) ;; interpreted by the macro

    (dotted-ident->
     ident-even-if-keyword->
     ( ident-even-if-keyword-> :FULL_STOP dotted-ident->
       (%index-filter (curry #'format nil "~A.~A") 0 2 )))))

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
