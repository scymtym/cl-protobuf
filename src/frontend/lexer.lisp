;;; lexer.lisp --- Lexer for textual protocol buffer descriptor syntax.
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

(defvar +ignored-chars+    (coerce '(#\Newline #\Space #\Tab #\/) 'string))

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
