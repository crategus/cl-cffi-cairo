;;; ----------------------------------------------------------------------------
;;; cairo.types.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; Types
;;;
;;;     Generic data types
;;;
;;; Types and Values
;;;
;;;     cairo_bool_t
;;;     cairo_user_data_key_t
;;;
;;;     cairo_rectangle_t                        <-- cairo.context.lisp
;;;     cairo_rectangle_list_t                   <-- cairo.context.lisp
;;;     cairo_rectangle_int_t
;;;
;;; Functions
;;;
;;;     cairo-destroy-func-t
;;;
;;; Description
;;;
;;;      This section lists generic data types used in the cairo API.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_bool_t
;;;
;;; typedef int cairo_bool_t;
;;;
;;; cairo_bool_t is used for boolean values. Returns of type cairo_bool_t will
;;; always be either 0 or 1, but testing against these values explicitly is not
;;; encouraged; just use the value as a boolean condition.
;;; ----------------------------------------------------------------------------

;; This type is represented with the cffi :bool type.

;;; ----------------------------------------------------------------------------
;;; cairo_user_data_key_t
;;;
;;; typedef struct {
;;;     int unused;
;;; } cairo_user_data_key_t;
;;;
;;; cairo_user_data_key_t is used for attaching user data to cairo data
;;; structures. The actual contents of the struct is never used, and there is no
;;; need to initialize the object; only the unique address of a cairo_data_key_t
;;; object is used. Typically, you would just use the address of a static
;;; cairo_data_key_t object.
;;;
;;; int unused;
;;;     not used; ignore.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_t                                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-t
  (x :double)
  (y :double)
  (width :double)
  (height :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'rectangle-t)
      "CStruct"
      (liber:symbol-documentation 'rectangle-t)
 "@version{#2023-1-11}
  @begin{short}
    The @symbol{cairo:rectangle-t} structure is a data structure for holding a
    rectangle.
  @end{short}
  @begin{pre}
(cffi:defcstruct rectangle-t
  (x :double)
  (y :double)
  (width :double)
  (height :double))
  @end{pre}
  @begin[code]{table}
    @entry[x]{The x coordinate of the left side of the rectangle.}
    @entry[y]{The y coordinate of the top side of the rectangle.}
    @entry[width]{The width of the rectangle.}
    @entry[height]{The height of the rectangle.}
  @end{table}
  @see-symbol{cairo:rectangle-int-t}
  @see-symbol{cairo:rectangle-list-t}")

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_list_t                                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-list-t
  (status status-t)
  (rectangles (:pointer (:pointer (:struct rectangle-t))))
  (num-rectangles :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'rectangle-list-t)
      "CStruct"
      (liber:symbol-documentation 'rectangle-list-t)
 "@version{#2023-1-1}
  @begin{short}
    The @symbol{cairo:rectangle-list-t} structure is a data structure for
    holding a dynamically allocated array of rectangles.
  @end{short}
  @begin{pre}
(cffi:defcstruct rectangle-list-t
  (status status-t)
  (rectangles (:pointer (:pointer (:struct rectangle-t))))
  (num-rectangles :int))
  @end{pre}
  @begin[code]{table}
    @entry[status]{Error status of the rectangle list.}
    @entry[rectangles]{Array containing the rectangles.}
    @entry[num-rectangles]{The number of rectangles in this list.}
  @end{table}
  @see-symbol{cairo:rectangle-t}")

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_int_t                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rectangle-int-t
  (x :int)
  (y :int)
  (width :int)
  (height :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'rectangle-int-t)
      "CStruct"
      (liber:symbol-documentation 'rectangle-int-t)
 "@version{#2023-1-11}
  @begin{short}
    A data structure for holding a rectangle with integer coordinates.
  @end{short}
  @begin{pre}
(cffi:defcstruct rectangle-int-t
  (x :int)
  (y :int)
  (width :int)
  (height :int))
  @end{pre}
  @begin[code]{table}
    @entry[x]{An integer x coordinate of the left side of the rectangle.}
    @entry[y]{An integer y coordinate of the the top side of the rectangle.}
    @entry[width]{An integer with the width of the rectangle.}
    @entry[height]{An integer with the height of the rectangle.}
  @end{table}
  @see-symbol{cairo:rectangle-t}")

;;; ----------------------------------------------------------------------------
;;; cairo_destroy_func_t ()
;;;
;;; void (*cairo_destroy_func_t) (void *data);
;;;
;;; cairo_destroy_func_t the type of function which is called when a data
;;; element is destroyed. It is passed the pointer to the data element and
;;; should free any memory and resources allocated for it.
;;;
;;; data :
;;;     The data element being destroyed.
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.types.lisp -------------------------------------------
