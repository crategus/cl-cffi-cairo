;;; ----------------------------------------------------------------------------
;;; cairo.version.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Version Information
;;;
;;;     Compile and run time version checks
;;;
;;; Types and Values
;;;
;;;     CAIRO_VERSION
;;;     CAIRO_VERSION_MAJOR
;;;     CAIRO_VERSION_MINOR
;;;     CAIRO_VERSION_MICRO
;;;     CAIRO_VERSION_STRING
;;;
;;; Functions
;;;
;;;     CAIRO_VERSION_ENCODE
;;;     CAIRO_VERSION_STRINGIZE
;;;
;;;     cairo_version
;;;     cairo_version_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION                                          not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MAJOR                                    not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MINOR                                    not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MICRO                                    not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRING                                   not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_ENCODE
;;; ----------------------------------------------------------------------------

(defun version-encode (major minor micro)
 #+liber-documentation
 "@version{2023-1-7}
  @argument[major]{an integer with the major component of the version number}
  @argument[minor]{an integer with the minor component of the version number}
  @argument[micro]{an integer with the micro component of the version number}
  @return{An integer with the encoded version.}
  @begin{short}
    This function encodes the given Cairo version into an integer.
  @end{short}
  Two encoded version numbers can be compared as integers. The encoding
  ensures that later versions compare greater than earlier versions.
  @see-function{cairo:version}"
  (parse-integer (format nil "~D~2,'0D~2,'0D" major minor micro)))

(export 'version-encode)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRINGIZE()
;;;
;;; #define CAIRO_VERSION_STRINGIZE(major, minor, micro)
;;;
;;; This macro encodes the given cairo version into an string. The numbers
;;; returned by CAIRO_VERSION_STRING and cairo_version_string() are encoded
;;; using this macro. The parameters to this macro must expand to numerical
;;; literals.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;;
;;; Returns :
;;;     a string literal containing the version.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_version ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_version" version) :int
 #+liber-documentation
 "@version{2023-1-7}
  @return{An integer with the encoded version.}
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
;;; cairo_version_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_version_string" version-string) :string
 #+liber-documentation
 "@version{2023-1-7}
  @return{A string containing the Cairo version.}
  @begin{short}
    Returns the version of the Cairo library as a human readable string of the
    form \"x.y.z\".
  @end{short}
  See also the @fun{cairo:version} function.
  @see-function{cairo:version}")

(export 'version-string)

;;; --- End of file cairo.version.lisp -----------------------------------------
