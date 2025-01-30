;;; ----------------------------------------------------------------------------
;;; cairo.types.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; Types
;;;
;;;     Generic data types
;;;
;;; Types and Values
;;;
;;;     cairo_rectangle_t                                   not exported
;;;     cairo_rectangle_list_t                              not exported
;;;     cairo_rectangle_int_t                               not exported
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-t
  (x :double)
  (y :double)
  (width :double)
  (height :double))

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_list_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-list-t
  (status status-t)
  (rectangles (:pointer (:pointer (:struct rectangle-t))))
  (num-rectangles :int))

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_int_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-int-t
  (x :int)
  (y :int)
  (width :int)
  (height :int))

;;; --- End of file cairo.types.lisp -------------------------------------------
