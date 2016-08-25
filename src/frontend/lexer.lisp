;;; lexer.lisp --- Lexer for textual protocol buffer descriptor syntax.
;;
;; Copyright (C) 2011-2016 Jan Moringen
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

(defvar +ignored-chars+    (coerce '(#\Newline #\Return #\Space #\Tab #\/) 'string))

(defvar +number-chars+     "e01234567890+-.")

(defvar +identifier-chars+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

(defvar +punctuation+      "=,;.{}[]")

(defvar +keywords+ '(:to :max
		     :optional :required :repeated
		     :syntax :import :package :option :extensions
		     :message :enum :service :default))

(defun make-char-reader (stream)
  "Return three values: a read-char function, an unread-char function
and a position function. The read-char function reads a single char
from STREAM, skipping over comments. The unread-char function behaves
as usual. The position function returns three values: the current
offset, line and column in STREAM. "
  (bind (((offset line column) '(0 1 0))
	 ;; COMMENT stores a "comment state":
	 ;; + nil :: Not currently within comment
	 ;; + t   :: In comment, kind not yet decided
	 ;; + :/  :: In //-style comment
	 ;; + :*  :: In /* */-style comment, closing * not yet encountered
	 ;; + :** :: In /* */-style comment, closing * encountered
	 (comment              nil)
	 (did-unread?          nil)
	 ((:flet read1 (&key (skip-comments? t)))
	  (iter (for c next (read-char stream nil :eof))
		(unless did-unread?
		  (incf offset)
		  (incf column)
		  (case c
		    (#\Newline (incf line)
			       (setf column 0)
			       (when (eq comment :/)
				 (setf comment nil)))
		    (#\/       (when skip-comments?
				 (setf comment (case comment
						 ((nil) t)
						 ((t)   :/)
						 (:**   nil)
						 (t     comment)))))
		    (#\*       (setf comment (case comment
					       ((t) :*)
					       (:*  :**)
					       (t   comment))))
		    (t         (setf comment (case comment
					       (:**  :*)
					       (t   comment))))))
		(setf did-unread? nil)
		(while (and (not (eq c :eof)) comment))
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
;;   decInt ::= \d+
;;   hexInt ::= 0[xX]([A-Fa-f0-9])+
;;   octInt ::= 0[0-7]+
;; floatLit ::= \d+(\.\d+)?([Ee][\+-]?\d+)? # allow_f_after_float_ is

;; string literals should be
;; strLit ::= quote ( hexEscape | octEscape | charEscape | [^\0\n] )* quote
;;   quote ::= ["']
;;   hexEscape ::= \\[Xx][A-Fa-f0-9]{1,2}
;;   octEscape ::= \\0?[0-7]{1,3}/
;;   charEscape ::= \\[abfnrtv\\\?'"]
