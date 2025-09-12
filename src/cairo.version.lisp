;;; ----------------------------------------------------------------------------
;;; cairo.version.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;;
;;; Version Information
;;;
;;;     Compile and run time version checks
;;;
;;; Functions
;;;
;;;     CAIRO_VERSION_ENCODE
;;;     CAIRO_VERSION_STRINGIZE                             not implemented
;;;
;;;     cairo_version
;;;     cairo_version_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_ENCODE
;;; ----------------------------------------------------------------------------

(defun version-encode (major minor micro)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[major]{an integer for the major component of the version number}
  @argument[minor]{an integer for the minor component of the version number}
  @argument[micro]{an integer for the micro component of the version number}
  @return{The integer for the encoded version.}
  @begin{short}
    This function encodes the given Cairo version into an integer.
  @end{short}
  Two encoded version numbers can be compared as integers. The encoding
  ensures that later versions compare greater than earlier versions.
  @see-function{cairo:version}"
  (parse-integer (format nil "~D~2,'0D~2,'0D" major minor micro)))

(export 'version-encode)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRINGIZE
;;;
;;; This macro encodes the given cairo version into an string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_version" version) :int
 #+liber-documentation
 "@version{2025-09-02}
  @return{The integer for the encoded version.}
  @begin{short}
    Returns the version of the Cairo library encoded in a single integer.
  @end{short}
  The encoding ensures that later versions compare greater than earlier
  versions.

  A run-time comparison to check that Cairo's version is greater than or
  equal to version x.y.z could be performed as follows:
  @begin{pre}
(>= (cairo:version) (cairo:version-encode x y z))
  @end{pre}
  See also the @fun{cairo:version-string} function.
  @see-function{cairo:version-string}")

(export 'version)

;;; ----------------------------------------------------------------------------
;;; cairo_version_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_version_string" version-string) :string
 #+liber-documentation
 "@version{2025-01-18}
  @return{The string containing the Cairo version.}
  @begin{short}
    Returns the version of the Cairo library as a human readable string of the
    form \"x.y.z\".
  @end{short}
  See also the @fun{cairo:version} function.
  @see-function{cairo:version}")

(export 'version-string)

;;; --- End of file cairo.version.lisp -----------------------------------------
