;;; ----------------------------------------------------------------------------
;;; generate-html.lisp
;;;
;;; Copyright (C) 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

#-liber-documentation
(push :liber-documentation *features*)

(asdf:load-system :liber/generate)
(asdf:load-system :cl-cffi-cairo :force t)

(defpackage :cairo-documentation
  (:use :common-lisp)
  (:export :generate-html
           :generate-html-single-page))

(in-package :cairo-documentation)

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-cairo)))
         (output (merge-pathnames "doc/" base)))
    (liber:generate-html-documentation
      '(:cairo)
      base
      output
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-cairo API documentation"
      :heading "cl-cffi-cairo"
      :css "crategus.css"
      :icon "lambda.icon"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil
      :delete-tmp-files-p t
      :verbose t)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-cairo)))
         (output (merge-pathnames "doc/single-page/" base)))
    (liber:generate-html-documentation
      '(:cairo)
      base
      output
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-cairo API documentation (single page)"
      :heading "cl-cffi-cairo"
      :css "crategus.css"
      :icon "lambda.icon"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil
      :delete-tmp-files-p t
      :verbose t)))

;;; --- End of file generate-html.lisp -----------------------------------------
