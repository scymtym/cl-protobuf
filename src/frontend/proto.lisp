;;; proto.lisp --- Interface for the proto parser.
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

(defgeneric load/text (source
		       &key
		       pathname
		       dependency-handler)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description.
Return a `file-set-desc' instance that contains the complete
description in its child nodes.

DEPENDENCY-HANDLER has to be a function of single argument that
accepts a source and loads the textual protocol buffer descriptor
designated by the source. If this fails for some reason, the supplied
function should signal an error of a subtype of `import-error' such as
`cannot-resolve-import'.

Note: Currently, the grammar accepts only a (quite usable) subset of
the textual protocol buffer description language.

Note: This function does not load serialized protocol buffer messages,
but textual protocol buffer descriptors using the grammar described at
http://code.google.com/apis/protocolbuffers/docs/proto.html."))


;;; Default implementation
;;

(defmethod load/text ((source stream)
		      &key
		      (pathname           "<stream>")
		      (dependency-handler #'load-from-path))
  (let* ((file (make-file-desc pathname (parse source)))
	 (set  (make-instance
		'file-set-desc
		:file (make-array 1
				  :initial-element file
				  :fill-pointer    1))))
    ;; Load all dependencies specified via "import" statements in
    ;; SOURCE.
    (map nil (compose dependency-handler #'parse-namestring)
	 (pb::file-desc-dependency file))
    ;; After dependencies have been resolved, try to register names
    ;; and relations.
    (pbb:emit set :relations)
    ;; Return the parsed and augmented descriptors.
    set))

(defmethod load/text ((source string)
		      &key
		      (pathname           "<string>")
		      (dependency-handler #'load-from-path))
  (with-input-from-string (stream source)
    (load/text stream
	       :pathname           pathname
	       :dependency-handler dependency-handler)))

(defmethod load/text ((source pathname)
		      &key
		      (pathname           (format nil "~A.~A"
						  (pathname-name source)
						  (pathname-type source)))
		      (dependency-handler #'load-from-path))
  (with-input-from-file (stream source)
    (load/text stream
	       :pathname           pathname
	       :dependency-handler dependency-handler)))

(defmethod load/text ((source sequence)
		      &rest args
		      &key &allow-other-keys)
  "This method read descriptions from all files in the list SOURCE and
collects the resulting `file-desc' instance in one `file-set-desc'."
  (bind (((result &rest rest)
	  (map 'list (apply #'rcurry #'load/text args) source))
	 ((:flet merge-one (desc))
	  (vector-push-extend
	   (aref (pb::file-set-desc-file desc) 0)
	   (pb::file-set-desc-file result))))
    (map nil #'merge-one rest)
    result))


;;; Default dependency resolution strategy
;;

(defun load-from-path (path
		       &key
		       (if-ambiguous :first))
  "Try to load the textual protocol buffer descriptor designated by
PATH. If PATH is an absolute pathname, it is used directly. Otherwise,
it is merged with the elements of `*proto-load-path*' to form absolute
pathnames of candidate files. An error of type `cannot-resolve-import'
is signaled if PATH cannot be resolved to an existing file.
IF-AMBIGUOUS can be used to control the behavior in case of multiple
candidate files. The value :FIRST (the default) causes the candidate
file that was produced by the leftmost item of `*proto-load-path*' to
be selected. The value :ERROR causes an error of type
`ambiguous-import' to be signaled."
  (check-type path pathname "a pathname")

  ;; If PATH is absolute, itself is the only location. Otherwise,
  ;; elements of *proto-load-path* are used.
  (bind ((locations (if (eq (first (pathname-directory path)) :absolute)
			(list path)
			(map 'list (curry #'merge-pathnames path)
			     *proto-load-path*)))
	 ((:flet find-on-load-path (path))
	  ;; Restrict locations to existing files and process
	  ;; candidate set.
	  (let ((candidates (remove-if-not #'probe-file locations)))
	    (cond
	      ((emptyp candidates)
	       (cannot-resolve-import path locations))
	      ((> (length candidates) 1)
	       (ecase if-ambiguous
		 (:first (first candidates))
		 (:error (ambiguous-import path candidates))))
	      (t (first candidates))))))
    (load/text (find-on-load-path path))))
