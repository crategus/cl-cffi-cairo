;;; ----------------------------------------------------------------------------
;;; cairo.path.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; Paths
;;;
;;;     Creating paths and manipulating path data
;;;
;;; Types and Values
;;;
;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t
;;;
;;; Functions
;;;
;;;     cairo_copy_path
;;;     cairo_copy_path_flat
;;;     cairo_path_destroy
;;;     cairo_append_path
;;;     cairo_has_current_point
;;;     cairo_get_current_point
;;;     cairo_new_path
;;;     cairo_new_sub_path
;;;     cairo_close_path
;;;     cairo_arc
;;;     cairo_arc_negative
;;;     cairo_curve_to
;;;     cairo_line_to
;;;     cairo_move_to
;;;     cairo_rectangle
;;;     cairo_glyph_path
;;;     cairo_text_path
;;;     cairo_rel_curve_to
;;;     cairo_rel_line_to
;;;     cairo_rel_move_to
;;;     cairo_path_extents
;;;
;;; Description
;;;
;;;     Paths are the most basic drawing tools and are primarily used to
;;;     implicitly generate simple masks.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; enum cairo_path_data_type_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum path-data-type-t
  :move-to
  :line-to
  :curve-to
  :close-path)

#+liber-documentation
(setf (liber:alias-for-symbol 'path-data-type-t)
       "CEnum"
      (liber:symbol-documentation 'path-data-type-t)
 "@version{#2021-12-12}
  @begin{short}
    The @symbol{cairo:path-data-type-t} enumeration is used to describe the type
    of one portion of a path when represented as a @symbol{cairo:path-t}
    instance.
  @end{short}
  See the @symbol{cairo:path-data-t} structure for details.
  @begin{pre}
(cffi:defcenum path-data-type-t
  :move-to
  :line-to
  :curve-to
  :close-path)
  @end{pre}
  @begin[code]{table}
    @entry[:move-to]{A move-to operation.}
    @entry[:line-to]{A line-to operation.}
    @entry[:curve-to]{A curve-to operation.}
    @entry[:close-path]{A close-path operation.}
  @end{table}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:path-data-t}")

(export 'path-data-type-t)

;;; ----------------------------------------------------------------------------
;;; union cairo_path_data_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct header-t
  (data-type path-data-type-t)
  (length :int))

(cffi:defcstruct point-t
  (x :double)
  (y :double))

(cffi:defcstruct path-data-t
  (header (:pointer (:struct header-t)))
  (point (:pointer (:struct point-t))))

#+liber-documentation
(setf (liber:alias-for-symbol 'path-data-t)
       "CStruct"
      (liber:symbol-documentation 'path-data-t)
 "@version{#2021-12-12}
  @begin{short}
    The @symbol{cairo:path-data-t} structure is used to represent the path data
    inside a @symbol{cairo:path-t} instance.
  @end{short}

  The data structure is designed to try to balance the demands of efficiency
  and ease-of-use. A path is represented as an array of
  @symbol{cairo:path-data-t} instances, which is a union of headers and points.
  @begin{pre}
(cffi:defcstruct header-t
  (data-type path-data-type-t)
  (length :int))

(cffi:defcstruct point-t
  (x :double)
  (y :double))

(cffi:defcunion path-data-t
  (header (:pointer (:struct header-t)))
  (point (:pointer (:struct point-t))))
  @end{pre}
  Each portion of the path is represented by one or more elements in the array,
  (one header followed by 0 or more points). The length value of the header is
  the number of array elements for the current portion including the header,
  (i.e. length == 1 + # of points), and where the number of points for each
  element type is as follows:
  @begin{pre}
:move-to     1 point
:line-to     1 point
:curve-to    3 points
:close-path  0 points
  @end{pre}
  The semantics and ordering of the coordinate values are consistent with
  the @fun{cairo:move-to}, @fun{cairo:line-to}, @fun{cairo:curve-to}, and
  @fun{cairo:close-path} functions.

  Here is sample code for iterating through a @symbol{cairo:path-t} structure:
  @begin{pre}
(defun path-to-lisp (path)
  (iter (with count = 0)
        (with numdata = (cairo:path-numdata path))
        (with element = :path)
        (with data = (cairo:path-data path))
        (with size = (cffi:foreign-type-size '(:struct cairo:path-data-t)))
        (collect element)
        (while (< count numdata))
        (cond ((eq :move-to (cairo:header-data-type data))
               (setf element (list :move-to))
               (setf count (incf count (cairo:header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (cairo:point-x data) element)
               (push (cairo:point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :line-to (cairo:header-data-type data))
               (setf element (list :line-to))
               (setf count (incf count (cairo:header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (cairo:point-x data) element)
               (push (cairo:point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :curve-to (cairo:header-data-type data))
               (setf element (list :curve-to))
               (setf count (incf count (cairo:header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (cairo:point-x data) element)
               (push (cairo:point-y data) element)
               (setf data (cffi:inc-pointer data size))
               (push (cairo:point-x data) element)
               (push (cairo:point-y data) element)
               (setf data (cffi:inc-pointer data size))
               (push (cairo:point-x data) element)
               (push (cairo:point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :close-path (cairo:header-data-type data))
               (setf element (list :close-path))
               (setf count (incf count (cairo:header-length data)))
               (setf data (cffi:inc-pointer data size)))
              (t (error \"KEYWORD ~a not known to PATH-DATA-TYPE-T\"
                        (cairo:header-data-type data))))))
  @end{pre}
  As of Cairo 1.4, Cairo does not mind if there are more elements in a portion
  of the path than needed. Such elements can be used by users of the Cairo API
  to hold extra values in the path data structure. For this reason, it is
  recommended that applications always use data->header.length to iterate over
  the path data, instead of hardcoding the number of elements for each element
  type.
  @see-symbol{cairo:path-t}
  @see-function{cairo:move-to}
  @see-function{cairo:line-to}
  @see-function{cairo:curve-to}
  @see-function{cairo:close-path}")

(export 'path-data-t)

;;; ----------------------------------------------------------------------------

(defun path-data-header (pathdata)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[pathdata]{a @symbol{cairo:path-data-t} instance}
  @return{The @symbol{cairo:header-t} instance.}
  @begin{short}
    Accessor of the @code{header} slot of the @symbol{cairo:path-data-t}
    structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-pointer pathdata '(:struct path-data-t) 'header))

#+liber-documentation
(setf (liber:alias-for-function 'path-data-header) "Accessor")

(export 'path-data-header)

;;; ----------------------------------------------------------------------------

(defun path-data-point (pathdata)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[pathdata]{a @symbol{cairo:path-data-t} instance}
  @return{The @symbol{cairo:point-t} instance.}
  @begin{short}
    Accessor of the @code{point} slot of the @symbol{cairo:path-data-t}
    structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-pointer pathdata '(:struct path-data-t) 'point))

#+liber-documentation
(setf (liber:alias-for-function 'path-data-point) "Accessor")

(export 'path-data-point)

;;; ----------------------------------------------------------------------------

(defun header-data-type (header)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[header]{a @symbol{cairo:header-t} instance}
  @return{The @symbol{cairo:path-data-type-t} value.}
  @begin{short}
    Accessor of the @code{data-type} slot of the @symbol{cairo:header-t}
    structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-value header '(:struct header-t) 'data-type))

#+liber-documentation
(setf (liber:alias-for-function 'header-data-type) "Accessor")

(export 'header-data-type)

;;; ----------------------------------------------------------------------------

(defun header-length (header)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[header]{a @symbol{cairo:header-t} instance}
  @return{The integer with the length of the header.}
  @begin{short}
    Accessor of the @code{length} slot of the @symbol{cairo:header-t}
    structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-value header '(:struct header-t) 'length))

#+liber-documentation
(setf (liber:alias-for-function 'header-length) "Accessor")

(export 'header-length)

;;; ----------------------------------------------------------------------------

(defun point-x (point)
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[header]{a @symbol{cairo:point-t} instance}
  @return{The double float with the x coordinate of the point.}
  @begin{short}
    Accessor of the @code{x} slot of the @symbol{cairo:point-t} structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-value point '(:struct point-t) 'x))

(export 'point-x)

;;; ----------------------------------------------------------------------------

(defun point-y (point)
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[header]{a @symbol{cairo:point-t} instance}
  @return{The double float with the y coordinate of the point.}
  @begin{short}
    Accessor of the @code{y} slot of the @symbol{cairo:point-t} structure.
  @end{short}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-value point '(:struct point-t) 'y))

(export 'point-y)

;;; ----------------------------------------------------------------------------
;;; cairo_path_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct path-t
  (status status-t)
  (data :pointer)           ; (:pointer (:pointer (:struct path-data-t))))
  (numdata :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'path-t)
       "CStruct"
      (liber:symbol-documentation 'path-t)
 "@version{#2021-12-12}
  @begin{short}
    A data structure for holding a path.
  @end{short}
  This data structure serves as the return value for the @fun{cairo:copy-path}
  and @fun{cairo:copy-path-flat} functions as well the input value for the
  @fun{cairo:append-path} function. See the @symbol{cairo:path-data-t} structure
  for hints on how to iterate over the actual data within the path.

  The @arg{numdata} member gives the number of elements in the data array.
  This number is larger than the number of independent path portions, defined
  in the @symbol{cairo:path-data-type-t} structure, since the data includes both
  headers and coordinates for each portion.
  @begin{pre}
(cffi:defcstruct path-t
  (status status-t)
  (data (:pointer (:pointer (:struct path-data-t))))
  (numdata :int))
  @end{pre}
  @begin[code]{table}
    @entry[status]{The current @symbol{cairo:status-t} error status.}
    @entry[data]{The elements of type @symbol{cairo:path-data-t} in the path.}
    @entry[numdata]{An integer with the number of elements in the data array.}
  @end{table}
  @see-symbol{cairo:status-t}
  @see-symbol{cairo:path-data-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}
  @see-function{cairo:append-path}")

(export 'path-t)

;;; ----------------------------------------------------------------------------

(defun path-status (path)
 #+liber-documentation
 "@version{#2023-1-6}
  @argument[path]{a @symbol{cairo:path-t} instance}
  @return{The current @symbol{cairo:status-t} error status.}
  @begin{short}
    Accessor of the @code{status} slot of the @symbol{cairo:path-t} structure.
  @end{short}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:status-t}"
  (cffi:foreign-slot-value path '(:struct path-t) 'status))

#+liber-documentation
(setf (liber:alias-for-function 'path-status) "Accessor")

(export 'path-status)

;;; ----------------------------------------------------------------------------

(defun path-data (path)
 #+liber-documentation
 "@version{#2023-1-6}
  @argument[path]{a @symbol{cairo:path-t} instance}
  @return{The @symbol{cairo:path-data-t} instances in the path.}
  @begin{short}
    Accessor of the @code{data} slot of the @symbol{cairo:path-t} structure.
  @end{short}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:path-data-t}"
  (cffi:foreign-slot-value path '(:struct path-t) 'data))

#+liber-documentation
(setf (liber:alias-for-function 'path-data) "Accessor")

(export 'path-data)

;;; ----------------------------------------------------------------------------

(defun path-numdata (path)
 #+liber-documentation
 "@version{#2023-1-6}
  @argument[path]{a @symbol{cairo:path-t} instance}
  @return{The integer with the number of elements in the data array.}
  @begin{short}
    Accessor of the @code{data} slot of the @symbol{cairo:path-t} structure.
  @end{short}
  @see-symbol{cairo:path-t}"
  (cffi:foreign-slot-value path '(:struct path-t) 'numdata))

#+liber-documentation
(setf (liber:alias-for-function 'path-numdata) "Accessor")

(export 'path-numdata)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path () -> copy-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_path" copy-path) (:pointer (:struct path-t))
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    The copy of the @symbol{cairo:path-t} current path. The caller owns the
    returned object and should call the @fun{cairo:path-destroy} function when
    finished with it.
  @end{return}
  @begin{short}
    Creates a copy of the current path and returns it to the user as a
    @symbol{cairo:path-t} instance.
  @end{short}
  See the @symbol{cairo:path-data-t} structure for hints on how to iterate over
  the returned data structure.

  This function will always return a valid pointer, but the result will have no
  data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to @code{:no-memory}.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case path->status will
      contain the same status that would be returned by the @fun{cairo:status}
      function.
    @end{item}
  @end{itemize}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:path-data-t}
  @see-function{cairo:status}
  @see-function{cairo:path-destroy}"
  (cr (:pointer (:struct context-t))))

(export 'copy-path)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path_flat () -> copy-path-flat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_path_flat" copy-path-flat)
    (:pointer (:struct path-t))
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    The copy of the @symbol{cairo:path-t} current path. The caller owns the
    returned object and should call the @fun{cairo:path-destroy} function when
    finished with it.
  @end{return}
  @begin{short}
    Gets a flattened copy of the current path and returns it to the user as a
    @symbol{cairo:path-t} instance.
  @end{short}
  See the @symbol{cairo:path-data-t} documentation for hints on how to iterate
  over the returned data structure.

  This function is like the @fun{cairo:copy-path} function except that any
  curves in the path will be approximated with piecewise-linear approximations,
  accurate to within the current tolerance value. That is, the result is
  guaranteed to not have any elements of type @code{:curve-to} which will
  instead be replaced by a series of @code{:line-to} elements.

  This function will always return a valid pointer, but the result will have
  no data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to @code{:no-memory}.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case path->status will
      contain the same status that would be returned by the @fun{cairo:status}
      function.
    @end{item}
  @end{itemize}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:path-data-t}
  @see-function{cairo:status}
  @see-function{cairo:copy-path}
  @see-function{cairo:path-destroy}"
  (cr (:pointer (:struct context-t))))

(export 'copy-path-flat)

;;; ----------------------------------------------------------------------------
;;; cairo_path_destroy () -> path-destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_path_destroy" path-destroy) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[path]{a @symbol{cairo:path-t} instance previously returned by either
  the @fun{cairo:copy-path} or @fun{cairo:copy-path-flat} functions}
  @begin{short}
    Immediately releases all memory associated with @arg{path}.
  @end{short}
  After a call to the @fun{cairo:path-destroy} function the path pointer is no
  longer valid and should not be used further.
  @begin[Note]{dictionary}
    The @fun{cairo:path-destroy} function should only be called with a pointer
    to a @symbol{cairo:path-t} instance returned by a Cairo function. Any path
    that is created manually, i.e. outside of Cairo, should be destroyed
    manually as well.
  @end{dictionary}
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}"
  (path (:pointer (:struct path-t))))

(export 'path-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_append_path () -> append-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_append_path" append-path) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[path]{a @symbol{cairo:path-t} instance to be appended}
  @begin{short}
    Append @arg{path} onto the current path.
  @end{short}
  The path may be either the return value from one of the @fun{cairo:copy-path}
  or @fun{cairo:copy-path-flat} functions or it may be constructed manually.
  See the @symbol{cairo:path-t} documentation for details on how the path data
  structure should be initialized, and note that the status of the path must be
  initialized to @code{:success}.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}"
  (cr (:pointer (:struct context-t)))
  (path (:pointer (:struct path-t))))

(export 'append-path)

;;; ----------------------------------------------------------------------------
;;; cairo_has_current_point () -> has-current-point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_has_current_point" has-current-point) :bool
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @return{Whether a current point is defined.}
  @begin{short}
    Returns whether a current point is defined on the current path.
  @end{short}
  See the @fun{cairo:current-point} function for details on the current point.
  @see-symbol{cairo:context-t}
  @see-function{cairo:current-point}"
  (cr (:pointer (:struct context-t))))

(export 'has-current-point)

;;; ----------------------------------------------------------------------------
;;; cairo_get_current_point () -> current-point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_current_point" %current-point) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun current-point (cr)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @arg{x} -- a double float x coordinate of the current point @br{}
    @arg{y} -- a double float y coordinate of the current point
  @end{return}
  @begin{short}
    Gets the current point of the current path, which is conceptually the final
    point reached by the path so far.
  @end{short}

  The current point is returned in the user-space coordinate system. If there
  is no defined current point or if @arg{cr} is in an error status, x and y
  will both be set to 0.0. It is possible to check this in advance with the
  @fun{cairo:has-current-point} function.

  Most path construction functions alter the current point. See the following
  functions for details on how they affect the current point:
  @begin{pre}
new-path          new-sub-path      append-path
close-path        move-to           line-to
curve-to          rel-move-to       rel-line-to
rel-curve-to      arc               arc-negative
rectangle         text-path         glyph-path
  @end{pre}
  Some functions use and alter the current point but do not otherwise change
  current path:
  @begin{pre}
show-text
  @end{pre}
  Some functions unset the current path and as a result, the current point:
  @begin{pre}
fill     stroke
  @end{pre}
  @see-symbol{cairo:context-t}
  @see-function{cairo:has-current-point}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (%current-point cr x y)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'current-point)

;;; ----------------------------------------------------------------------------
;;; cairo_new_path () -> new-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_new_path" new-path) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{short}
    Clears the current path.
  @end{short}
  After this call there will be no path and no current point.
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct context-t))))

(export 'new-path)

;;; ----------------------------------------------------------------------------
;;; cairo_new_sub_path () -> new-sub-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_new_sub_path" new-sub-path) :void
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  Note that the existing path is not affected. After this call there will be no
  current point. In many cases, this call is not needed since new sub-paths are
  frequently started with the @fun{cairo:move-to} function.

  A call to the @fun{cairo:new-sub-path} function is particularly useful when
  beginning a new sub-path with one of the @fun{cairo:arc} calls. This makes
  things easier as it is no longer necessary to manually compute the initial
  coordinates of the arc for a call to the @fun{cairo:move-to} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}
  @see-function{cairo:arc}"
  (cr (:pointer (:struct context-t))))

(export 'new-sub-path)

;;; ----------------------------------------------------------------------------
;;; cairo_close_path () -> close-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_close_path" close-path) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{short}
    Adds a line segment to the path from the current point to the beginning of
    the current sub-path, the most recent point passed to the
    @fun{cairo:move-to} function, and closes this sub-path.
  @end{short}
  After this call the current point will be at the joined endpoint of the
  sub-path.

  The behavior of the @fun{cairo:close-path} function is distinct from simply
  calling the @fun{cairo:line-to} function with the equivalent coordinate in
  the case of stroking. When a closed sub-path is stroked, there are no caps on
  the ends of the sub-path. Instead, there is a line join connecting the final
  and initial segments of the sub-path.

  If there is no current point before the call to the @fun{cairo:close-path}
  function, this function will have no effect.
  @begin[Note]{dictionary}
    As of Cairo version 1.2.4 any call to the @fun{cairo:close-path} function
    will place an explicit @code{:move-to} element into the path immediately
    after the @code{:close-path} element, which can be seen in the
    @fun{cairo:copy-path} function for example. This can simplify path
    processing in some cases as it may not be necessary to save the \"last
    @code{:move-to} point\" during processing as the @code{:move-to} immediately
    after the @code{:close-path} will provide that point.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}
  @see-function{cairo:line-to}
  @see-function{cairo:copy-path}"
  (cr (:pointer (:struct context-t))))

(export 'close-path)

;;; ----------------------------------------------------------------------------
;;; cairo_arc () -> arc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_arc" %arc) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(defun arc (cr x y radius angle1 angle2)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number with the x position of the center of the arc}
  @argument[y]{a number with the y position of the center of the arc}
  @argument[radius]{a number with the radius of the arc}
  @argument[angle1]{a number with the start angle, in radians}
  @argument[angle2]{a number with the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at @code{(@arg{x}, @arg{y})}, begins at @arg{angle1}
  and proceeds in the direction of increasing angles to end at @arg{angle2}. If
  @arg{angle2} is less than @arg{angle1} it will be progressively increased by
  2*PI until it is greater than @arg{angle1}.

  If there is a current point, an initial line segment will be added to the
  path to connect the current point to the beginning of the arc. If this
  initial line is undesired, it can be avoided by calling the
  @fun{cairo:new-sub-path} function before calling the @fun{cairo:arc} function.

  Angles are measured in radians. An angle of 0 is in the direction of the
  positive x axis (in user space). An angle of PI/2 radians (90 degrees) is in
  the direction of the positive y axis (in user space). Angles increase in the
  direction from the positive x axis toward the positive y axis. So with the
  default transformation matrix, angles increase in a clockwise direction.

  This function gives the arc in the direction of increasing angles. See the
  @fun{cairo:arc-negative} function to get the arc in the direction of
  decreasing angles.
  @begin[Example]{dictionary}
    The arc is circular in user space. To achieve an elliptical arc, you can
    scale the current transformation matrix by different amounts in the x and y
    directions. For example, to draw an ellipse in the box given by @arg{x},
    @arg{y}, @arg{width}, @arg{height}:
    @begin{pre}
(cairo:save cr)
(cairo:translate cr (+ x (/ width 2)) (+ y (/ height 2)))
(cairo:scale cr (/ width 2) (/ height 2))
(cairo:arc cr 0 0 1 0 (* 2 pi))
(cairo:restore cr)
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:new-sub-path}
  @see-function{cairo:arc-negative}"
  (%arc cr (coerce x 'double-float)
           (coerce y 'double-float)
           (coerce radius 'double-float)
           (coerce angle1 'double-float)
           (coerce angle2 'double-float)))

(export 'arc)

;;; ----------------------------------------------------------------------------
;;; cairo_arc_negative () -> arc-negative
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_arc_negative" %arc-negative) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(defun arc-negative (cr x y radius angle1 angle2)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number with the x position of the center of the arc}
  @argument[y]{a number with the y position of the center of the arc}
  @argument[radius]{a number with the radius of the arc}
  @argument[angle1]{a number with the start angle, in radians}
  @argument[angle2]{a number with the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at (@arg{x}, @arg{y}), begins at @arg{angle1} and
  proceeds in the direction of decreasing angles to end at @arg{angle2}. If
  @arg{angle2} is greater than @arg{angle1} it will be progressively decreased
  by @code{2*PI} until it is less than @arg{angle1}.

  See the @fun{cairo:arc} function for more details. This function differs only
  in the direction of the arc between the two angles.
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:arc}"
  (%arc-negative cr (coerce x 'double-float)
                    (coerce y 'double-float)
                    (coerce radius 'double-float)
                    (coerce angle1 'double-float)
                    (coerce angle2 'double-float)))

(export 'arc-negative)

;;; ----------------------------------------------------------------------------
;;; cairo_curve_to () -> curve-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_curve_to" %curve-to) :void
  (cr (:pointer (:struct context-t)))
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double)
  (x3 :double)
  (y3 :double))

(defun curve-to (cr x1 y1 x2 y2 x3 y3)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x1]{a number with the x coordinate of the first control point}
  @argument[y1]{a number with the y coordinate of the first control point}
  @argument[x2]{a number with the x coordinate of the second control point}
  @argument[y2]{a number with the y coordinate of the second control point}
  @argument[x3]{a number with the x coordinate of the third control point}
  @argument[y3]{a number with the x coordinate of the third control point}
  @begin{short}
    Adds a cubic Bezier spline to the path from the current point to position
    (@arg{x3}, @arg{y3}) in user-space coordinates, using (@arg{x1}, @arg{y1})
    and (@arg{x2}, @arg{y2}) as the control points.
  @end{short}
  After this call the current point will be (@arg{x3}, @arg{y3}).

  If there is no current point before the call to the @fun{cairo:curve-to}
  function this function will behave as if preceded by a call to:
  @begin{pre}
(cairo:move-to cr x1 y1)
  @end{pre}
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}"
  (%curve-to cr (coerce x1 'double-float) (coerce y1 'double-float)
                (coerce x2 'double-float) (coerce y2 'double-float)
                (coerce x3 'double-float) (coerce y3 'double-float)))

(export 'curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_line_to () -> line-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_line_to" %line-to) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double))

(defun line-to (cr x y)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    end of the new line}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    end of the new line}
  @begin{short}
    Adds a line to the path from the current point to position
   (@arg{x}, @arg{y}) in user-space coordinates.
  @end{short}
  After this call the current point will be (@arg{x}, @arg{y}).

  If there is no current point before the call to the @fun{cairo:line-to}
  function this function will behave as:
  @begin{pre}
(cairo:move-to cr x y)
  @end{pre}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}"
  (%line-to cr (coerce x 'double-float)
               (coerce y 'double-float)))

(export 'line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_move_to () -> move-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_move_to" %move-to) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double))

(defun move-to (cr x y)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    new position}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    new position}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will be (@arg{x}, @arg{y}).
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-to}"
  (%move-to cr (coerce x 'double-float)
               (coerce y 'double-float)))

(export 'move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rectangle" %rectangle) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun rectangle (cr x y width height)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number with the x coordinate of the top left corner of the
    rectangle}
  @argument[y]{a number with the y coordinate to the top left corner of the
    rectangle}
  @argument[width]{a number with the width of the rectangle}
  @argument[height]{a number with the height of the rectangle}
  @begin{short}
    Adds a closed sub-path rectangle of the given size to the current path at
    position (@arg{x}, @arg{y}) in user-space coordinates.
  @end{short}

  This function is logically equivalent to:
  @begin{pre}
(cairo:move-to cr x y)
(cairo:rel-line-to cr width 0)
(cairo:rel-line-to cr 0 height)
(cairo:rel-line-to cr (- width) 0)
(cairo:close-path cr)
  @end{pre}
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}
  @see-function{cairo:rel-line-to}
  @see-function{cairo:close-path}"
  (%rectangle cr (coerce x 'double-float)
                 (coerce y 'double-float)
                 (coerce width 'double-float)
                 (coerce height 'double-float)))

(export 'rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_path () -> glyph-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_glyph_path" glyph-path) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[glyphs]{an array of @symbol{cairo:glyph-t} glyphs to show}
  @argument[num]{an integer with the number of glyphs to show}
  @begin{short}
    Adds closed paths for the glyphs to the current path.
  @end{short}
  The generated path if filled, achieves an effect similar to that of the
  @fun{cairo:show-glyphs} function.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:glyph-t}
  @see-function{cairo:show-glyphs}"
  (cr (:pointer (:struct context-t)))
  (glyphs (:pointer (:pointer (:struct glyph-t))))
  (num :int))

(export 'glyph-path)

;;; ----------------------------------------------------------------------------
;;; cairo_text_path () -> text-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_text_path" text-path) :void
 #+liber-documentation
 "@version{2023-1-15}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[utf8]{a string of text encoded in UTF-8, or @code{nil}}
  @begin{short}
    Adds closed paths for text to the current path.
  @end{short}
  The generated path if filled, achieves an effect similar to that of the
  @fun{cairo:show-text} function. Text conversion and positioning is done
  similar to the @fun{cairo:show-text} function.

  Like the @fun{cairo:show-text} function, after this call the current point is
  moved to the origin of where the next glyph would be placed in this same
  progression. That is, the current point will be at the origin of the final
  glyph offset by its advance values. This allows for chaining multiple calls
  to to the @fun{cairo:text-path} function without having to set current point
  in between.
  @begin[Note]{dictionary}
    The @fun{cairo:text-path} function call is part of what the Cairo designers
    call the \"toy\" text API. It is convenient for short demos and simple
    programs, but it is not expected to be adequate for serious text-using
    applications. See the @fun{cairo:glyph-path} function for the \"real\" text
    path API in Cairo.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:show-text}
  @see-function{cairo:glyph-path}"
  (cr (:pointer (:struct context-t)))
  (uft8 :string))

(export 'text-path)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_curve_to () -> rel-curve-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rel_curve_to" %rel-curve-to) :void
  (cr (:pointer (:struct context-t)))
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double)
  (dx3 :double)
  (dy3 :double))

(defun rel-curve-to (cr dx1 dy1 dx2 dy2 dx3 dy3)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[dx1]{a number with the x offset to the first control point}
  @argument[dy1]{a number with the y offset to the first control point}
  @argument[dx2]{a number with the x offset to the second control point}
  @argument[dy2]{a number with the y offset to the second control point}
  @argument[dx3]{a number with the x offset to the end of the curve}
  @argument[dy3]{a number with the y offset to the end of the curve}
  @begin{short}
    Relative-coordinate version of the @fun{cairo:curve-to} function.
  @end{short}
  All offsets are relative to the current point. Adds a cubic Bézier spline to
  the path from the current point to a point offset from the current point by
  (@arg{dx3}, @arg{dy3}), using points offset by (@arg{dx1}, @arg{dy1}) and
  (@arg{dx2}, @arg{dy2}) as the control points. After this call the current
  point will be offset by (@arg{dx3}, @arg{dy3}).

  Given a current point of (@arg{x}, @arg{y}),
  @begin{pre}
(cairo:rel-curve-to cr dx1 dy1 dx2 dy2 dx3 dy3)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo:curve-to cr (+ x dx1) (+ y dy1)
                   (+ x dx2) (+ y dy2)
                   (+ x dx3) (+ y dy3))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:curve-to}"
  (%rel-curve-to cr (coerce dx1 'double-float)
                    (coerce dy1 'double-float)
                    (coerce dx2 'double-float)
                    (coerce dy2 'double-float)
                    (coerce dx3 'double-float)
                    (coerce dy3 'double-float)))

(export 'rel-curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_line_to () -> rel-line-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rel_line_to" %rel-line-to) :void
  (cr (:pointer (:struct context-t)))
  (dx :double)
  (dy :double))

(defun rel-line-to (cr dx dy)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[dx]{a number with the x offset to the end of the new line}
  @argument[dy]{a number with the y offset to the end of the new line}
  @begin{short}
    Relative-coordinate version of the @fun{cairo:line-to} function.
  @end{short}
  Adds a line to the path from the current point to a point that is offset from
  the current point by (@arg{dx}, @arg{dy}) in user space. After this call the
  current point will be offset by (@arg{dx}, @arg{dy}).

  Given a current point of (x, y),
  @begin{pre}
(cairo:rel-line-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo:line-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-to}"
  (%rel-line-to cr (coerce dx 'double-float)
                   (coerce dy 'double-float)))

(export 'rel-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_move_to () -> rel-move-to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rel_move_to" %rel-move-to) :void
  (cr (:pointer (:struct context-t)))
  (dx :double)
  (dy :double))

(defun rel-move-to (cr dx dy)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[dx]{a number with the x offset}
  @argument[dy]{a number with the y offset}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will offset by (x, y).

  Given a current point of (x, y),
  @begin{pre}
(cairo:rel-move-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo:move-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @begin[Note]{dictionary}
    The numbers of the arguments are coerced to double float values before
    being passed to the C function.
  @end{dictionary}
  @see-class{cairo:context-t}
  @see-function{cairo:move-to}"
  (%rel-move-to cr (coerce dx 'double-float)
                   (coerce dy 'double-float)))

(export 'rel-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_path_extents () -> path-extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_path_extents" %path-extents) :void
  (cr (:pointer (:struct context-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun path-extents (cr)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @arg{x1} -- a double float for the left of the resulting extents @br{}
    @arg{y1} -- a double float for the top of the resulting extents @br{}
    @arg{x2} -- a double float for the right of the resulting extents @br{}
    @arg{y2} -- a double float for the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user-space coordinates covering the points on
    the current path.
  @end{short}
  If the current path is empty, returns an empty rectangle ((0,0), (0,0)).
  Stroke parameters, fill rule, surface dimensions and clipping are not taken
  into account.

  Contrast with the @fun{cairo:fill-extents} and @fun{cairo:stroke-extents}
  functions which return the extents of only the area that would be \"inked\"
  by the corresponding drawing operations.

  The result of the @fun{cairo:path-extents} function is defined as equivalent
  to the limit of the @fun{cairo:stroke-extents} function with @code{:round} as
  the line width approaches 0.0, but never reaching the empty-rectangle
  returned by the @fun{cairo:stroke-extents} function for a line width of 0.0.

  Specifically, this means that zero-area sub-paths such as @fun{cairo:move-to},
  @fun{cairo:line-to} segments, even degenerate cases where the coordinates to
  both calls are identical, will be considered as contributing to the extents.
  However, a lone @fun{cairo:move-to} will not contribute to the results of
  the @fun{cairo:path-extents} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill-extents}
  @see-function{cairo:stroke-extents}
  @see-function{cairo:move-to}
  @see-function{cairo:line-to}"
  (cffi:with-foreign-objects ((x1 :double)
                              (y1 :double)
                              (x2 :double)
                              (y2 :double))
    (%path-extents cr x1 y1 x2 y2)
    (values (cffi:mem-ref x1 :double)
            (cffi:mem-ref y1 :double)
            (cffi:mem-ref x2 :double)
            (cffi:mem-ref y2 :double))))

(export 'path-extents)

;;; --- End of file cairo.path.lisp --------------------------------------------
