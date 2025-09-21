;;; ----------------------------------------------------------------------------
;;; cairo.path.lisp
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
;;;     cairo_path_extents
;;;     cairo_move_to
;;;     cairo_rel_move_to
;;;     cairo_line_to
;;;     cairo_rel_line_to
;;;     cairo_curve_to
;;;     cairo_rel_curve_to
;;;     cairo_rectangle
;;;     cairo_arc
;;;     cairo_arc_negative
;;;     cairo_glyph_path
;;;     cairo_text_path
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_path_data_type_t
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
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum path-data-type-t
  :move-to
  :line-to
  :curve-to
  :close-path)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:move-to]{A move-to operation.}
      @entry[:line-to]{A line-to operation.}
      @entry[:curve-to]{A curve-to operation.}
      @entry[:close-path]{A close-path operation.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:path-data-type-t} enumeration is used to describe the type of
    one portion of a path when represented as a @sym{cairo:path-t} instance.
  @end{short}
  See the @sym{cairo:path-data-t} structure for details.
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
 "@version{2025-09-02}
  @begin{declaration}
(cffi:defcstruct header-t
  (data-type path-data-type-t)
  (length :int))

(cffi:defcstruct point-t
  (x :double)
  (y :double))

(cffi:defcunion path-data-t
  (header (:pointer (:struct header-t)))
  (point (:pointer (:struct point-t))))
  @end{declaration}
  @begin{short}
    The @sym{cairo:path-data-t} structure is used to represent the path data
    inside a @sym{cairo:path-t} instance.
  @end{short}

  The data structure is designed to try to balance the demands of efficiency
  and ease-of-use. A path is represented as an array of
  @sym{cairo:path-data-t} instances, which is a union of headers and points.

  Each portion of the path is represented by one or more elements in the array,
  one header followed by 0 or more points. The length value of the header is
  the number of array elements for the current portion including the header,
  that is @code{length == 1 + #} of points, and where the number of points for
  each element type is as follows:
  @begin{pre}
:move-to     1 point
:line-to     1 point
:curve-to    3 points
:close-path  0 points
  @end{pre}
  The semantics and ordering of the coordinate values are consistent with
  the @fun{cairo:move-to}, @fun{cairo:line-to}, @fun{cairo:curve-to}, and
  @fun{cairo:close-path} functions.
  @begin[Notes]{dictionary}
    The Lisp API has the @fun{cairo:path-data-to-list} function, that returns
    a @sym{cairo:path-data-t} instance as a Lisp list. There are no functions
    available, that allows to create a @sym{cairo:path-data-t} instance from
    scratch.
  @end{dictionary}
  @see-symbol{cairo:path-t}
  @see-function{cairo:move-to}
  @see-function{cairo:line-to}
  @see-function{cairo:curve-to}
  @see-function{cairo:close-path}")

(export 'path-data-t)

;;; ----------------------------------------------------------------------------

;; The following accessors are not exported and for internal use only.

(defun path-data-header (pathdata)
  (cffi:foreign-slot-pointer pathdata '(:struct path-data-t) 'header))

(defun path-data-point (pathdata)
  (cffi:foreign-slot-pointer pathdata '(:struct path-data-t) 'point))

(defun header-data-type (header)
  (cffi:foreign-slot-value header '(:struct header-t) 'data-type))

(defun header-length (header)
  (cffi:foreign-slot-value header '(:struct header-t) 'length))

(defun point-x (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'x))

(defun point-y (point)
  (cffi:foreign-slot-value point '(:struct point-t) 'y))

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
 "@version{2025-09-02}
  @begin{declaration}
(cffi:defcstruct path-t
  (status status-t)
  (data (:pointer (:pointer (:struct path-data-t))))
  (numdata :int))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[status]{The current @sym{cairo:status-t} value for the error
        status.}
      @entry[data]{The @sym{cairo:path-data-t} instances in the path.}
      @entry[numdata]{The integer for the number of elements in the data array.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The data structure for holding a path.
  @end{short}
  This data structure serves as the return value for the @fun{cairo:copy-path}
  and @fun{cairo:copy-path-flat} functions as well the input value for the
  @fun{cairo:append-path} function. See the @sym{cairo:path-data-t}
  documentation for more information.

  The @arg{numdata} member gives the number of elements in the data array.
  This number is larger than the number of independent path portions, defined
  in the @sym{cairo:path-data-type-t} structure, since the data includes both
  headers and coordinates for each portion.
  @see-symbol{cairo:status-t}
  @see-symbol{cairo:path-data-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}
  @see-function{cairo:append-path}")

(export 'path-t)

;;; ----------------------------------------------------------------------------

(defun path-status (path)
 #+liber-documentation
 "@version{2025-09-20}
  @argument[path]{a @sym{cairo:path-t} instance}
  @return{The current @sym{cairo:status-t} value for the error status.}
  @begin{short}
    The accessor for the @code{status} slot of the @sym{cairo:path-t} structure.
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
 "@version{2025-09-20}
  @argument[path]{a @sym{cairo:path-t} instance}
  @return{The @sym{cairo:path-data-t} instances in the path.}
  @begin{short}
    The accessor for the @code{data} slot of the @sym{cairo:path-t} structure.
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
 "@version{2025-09-20}
  @argument[path]{a @sym{cairo:path-t} instance}
  @return{The integer for the number of elements in the data array.}
  @begin{short}
    The accessor for the @code{numdata} slot of the @sym{cairo:path-t}
    structure.
  @end{short}
  @see-symbol{cairo:path-t}"
  (cffi:foreign-slot-value path '(:struct path-t) 'numdata))

#+liber-documentation
(setf (liber:alias-for-function 'path-numdata) "Accessor")

(export 'path-numdata)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_path" copy-path) (:pointer (:struct path-t))
 #+liber-documentation
 "@version{2025-09-21}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @begin{return}
    The @sym{cairo:path-t} instance for the copy of the  current path. The
    caller owns the returned object and should call the @fun{cairo:path-destroy}
    function when finished with it.
  @end{return}
  @begin{short}
    Creates a copy of the current path and returns it to the user as a
    @sym{cairo:path-t} instance.
  @end{short}
  See the @sym{cairo:path-data-t} documentation for more information.

  This function will always return a valid path, but the result will have no
  data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to the @val[cairo:status-t]{:no-memory} value.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case the
      @fun{cairo:path-status} function will return the same status that would
      be returned by the @fun{cairo:status} function.
    @end{item}
  @end{itemize}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:path-t}
  @see-symbol{cairo:path-data-t}
  @see-function{cairo:path-status}
  @see-function{cairo:status}
  @see-function{cairo:path-destroy}"
  (cr (:pointer (:struct context-t))))

(export 'copy-path)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path_flat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_path_flat" copy-path-flat)
    (:pointer (:struct path-t))
 #+liber-documentation
 "@version{2025-09-21}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @begin{return}
    The @sym{cairo:path-t} instance for the copy of the  current path. The
    caller owns the returned object and should call the @fun{cairo:path-destroy}
    function when finished with it.
  @end{return}
  @begin{short}
    Gets a flattened copy of the current path and returns it to the user as a
    @sym{cairo:path-t} instance.
  @end{short}
  See the @sym{cairo:path-data-t} documentation for more information.

  This function is like the @fun{cairo:copy-path} function except that any
  curves in the path will be approximated with piecewise-linear approximations,
  accurate to within the current tolerance value. That is, the result is
  guaranteed to not have any @val[cairo:path-data-type-t]{:curve-to} elements
  which will instead be replaced by a series of
  @val[cairo:path-data-type-t]{:line-to} elements.

  This function will always return a valid path, but the result will have
  no data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to the @val[cairo:status-t]{:no-memory} value.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case the
      @fun{cairo:path-status} function will return the same status that would
      be returned by the @fun{cairo:status} function.
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
;;; cairo_path_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_path_destroy" path-destroy) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[path]{a @sym{cairo:path-t} instance previously returned by either
  the @fun{cairo:copy-path} or @fun{cairo:copy-path-flat} functions}
  @begin{short}
    Immediately releases all memory associated with @arg{path}.
  @end{short}
  After a call to the @fun{cairo:path-destroy} function the path is no longer
  valid and should not be used further.
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}"
  (path (:pointer (:struct path-t))))

(export 'path-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo:path-data-lo-list
;;; ----------------------------------------------------------------------------

;; TODO: Improve the implementation to get a nicer loop. Add documentation for
;; the symbols in the list for the path data.

(defun path-data-to-list (path)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[path]{a @sym{cairo:path-t} instance}
  @return{The list with the path.}
  @begin{short}
    Creates a list with the path information from a @sym{cairo:path-t} instance.
  @end{short}
  See the @fun{cairo:copy-path} and @fun{cairo:copy-path-flat} functions.
  @begin[Notes]{dictionary}
    There is no Lisp API exported that gives direct access to the internal
    path data, nor has the Lisp API functions to create from scratch path data
    information.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(cairo:with-recording-surface (surface :color)
  (cairo:with-context (context surface)
    (cairo:new-path context)
    (cairo:rectangle context 10 20 30 40)
    (let ((path (cairo:copy-path-flat context)))
      (prog1
        (cairo:path-data-to-list path)
        (cairo:path-destroy path)))))
=> (:PATH
    (:MOVE-TO 10.0d0 20.0d0)
    (:LINE-TO 40.0d0 20.0d0)
    (:LINE-TO 40.0d0 60.0d0)
    (:LINE-TO 10.0d0 60.0d0)
    (:CLOSE-PATH)
    (:MOVE-TO 10.0d0 20.0d0))
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}"
  (iter (with count = 0)
        (with numdata = (path-numdata path))
        (with element = :path)
        (with data = (path-data path))
        (with size = (cffi:foreign-type-size '(:struct path-data-t)))
        (collect element)
        (while (< count numdata))
        (cond ((eq :move-to (header-data-type data))
               (setf element (list :move-to))
               (setf count (incf count (header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (point-x data) element)
               (push (point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :line-to (header-data-type data))
               (setf element (list :line-to))
               (setf count (incf count (header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (point-x data) element)
               (push (point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :curve-to (header-data-type data))
               (setf element (list :curve-to))
               (setf count (incf count (header-length data)))
               (setf data (cffi:inc-pointer data size))
               (push (point-x data) element)
               (push (point-y data) element)
               (setf data (cffi:inc-pointer data size))
               (push (point-x data) element)
               (push (point-y data) element)
               (setf data (cffi:inc-pointer data size))
               (push (point-x data) element)
               (push (point-y data) element)
               (setf element (reverse element))
               (setf data (cffi:inc-pointer data size)))
              ((eq :close-path (header-data-type data))
               (setf element (list :close-path))
               (setf count (incf count (header-length data)))
               (setf data (cffi:inc-pointer data size)))
              (t (error "KEYWORD ~a not known to PATH-DATA-TYPE-T"
                        (header-data-type data))))))

(export 'path-data-to-list)

;;; ----------------------------------------------------------------------------
;;; cairo_append_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_append_path" append-path) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[path]{a @sym{cairo:path-t} instance to be appended}
  @begin{short}
    Append @arg{path} onto the current path.
  @end{short}
  The path may be either the return value from one of the @fun{cairo:copy-path}
  or @fun{cairo:copy-path-flat} functions.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}
  @see-function{cairo:copy-path-flat}"
  (cr (:pointer (:struct context-t)))
  (path (:pointer (:struct path-t))))

(export 'append-path)

;;; ----------------------------------------------------------------------------
;;; cairo_has_current_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_has_current_point" has-current-point) :bool
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @return{The boolean whether a current point is defined.}
  @begin{short}
    Returns whether a current point is defined on the current path.
  @end{short}
  See the @fun{cairo:current-point} function for details on the current point.
  @see-symbol{cairo:context-t}
  @see-function{cairo:current-point}"
  (cr (:pointer (:struct context-t))))

(export 'has-current-point)

;;; ----------------------------------------------------------------------------
;;; cairo_get_current_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_current_point" %current-point) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun current-point (cr)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @begin{return}
    @arg{x} -- a double float for the x coordinate of the current point @br{}
    @arg{y} -- a double float for the y coordinate of the current point
  @end{return}
  @begin{short}
    Gets the current point of the current path, which is conceptually the final
    point reached by the path so far.
  @end{short}

  The current point is returned in the user-space coordinate system. If there
  is no defined current point or if @arg{cr} is in an error status, @arg{x} and
  @arg{y} will both be set to 0.0. It is possible to check this in advance with
  the @fun{cairo:has-current-point} function.

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
  the current path:
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
;;; cairo_new_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_new_path" new-path) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @begin{short}
    Clears the current path.
  @end{short}
  After this call there will be no path and no current point.
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct context-t))))

(export 'new-path)

;;; ----------------------------------------------------------------------------
;;; cairo_new_sub_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_new_sub_path" new-sub-path) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
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
;;; cairo_close_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_close_path" close-path) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
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
  @begin[Notes]{dictionary}
    Any call to the @fun{cairo:close-path} function will place an explicit
    @val[cairo:path-data-type-t]{:move-to} element into the path immediately
    after the @val[cairo:path-data-type-t]{:close-path} element, which can be
    seen in the @fun{cairo:copy-path} function for example. This can simplify
    path processing in some cases as it may not be necessary to save the \"last
    @val[cairo:path-data-type-t]{:move-to} point\" during processing as the
    @val[cairo:path-data-type-t]{:move-to} element immediately after the
    @val[cairo:path-data-type-t]{:close-path} element will provide that point.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}
  @see-function{cairo:line-to}
  @see-function{cairo:copy-path}"
  (cr (:pointer (:struct context-t))))

(export 'close-path)

;;; ----------------------------------------------------------------------------
;;; cairo_path_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_path_extents" %path-extents) :void
  (cr (:pointer (:struct context-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun path-extents (cr)
 #+liber-documentation
 "@version{2025-09-20}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @begin{return}
    @arg{x1} -- a number for the left of the resulting extents @br{}
    @arg{y1} -- a number for the top of the resulting extents @br{}
    @arg{x2} -- a number for the right of the resulting extents @br{}
    @arg{y2} -- a number for the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user-space coordinates covering the points on
    the current path.
  @end{short}
  If the current path is empty, returns an empty rectangle. Stroke parameters,
  fill rule, surface dimensions and clipping are not taken into account.

  Contrast with the @fun{cairo:fill-extents} and @fun{cairo:stroke-extents}
  functions which return the extents of only the area that would be \"inked\"
  by the corresponding drawing operations.

  The result of the @fun{cairo:path-extents} function is defined as equivalent
  to the limit of the @fun{cairo:stroke-extents} function with the
  @val[cairo:line-cap-t]{:round} value as the line width approaches 0.0, but
  never reaching the empty-rectangle returned by the @fun{cairo:stroke-extents}
  function for a line width of 0.0.

  Specifically, this means that zero-area sub-paths such as @fun{cairo:move-to},
  @fun{cairo:line-to} segments, even degenerate cases where the coordinates to
  both calls are identical, will be considered as contributing to the extents.
  However, a lone @fun{cairo:move-to} will not contribute to the results of
  the @fun{cairo:path-extents} function.
  @begin[Notes]{dictionary}
    The numbers returned are double floats.
  @end{dictionary}
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

;;; ----------------------------------------------------------------------------
;;; cairo_move_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_move_to" %move-to) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double))

(defun move-to (cr x y)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x coordinate of the new position}
  @argument[y]{a number for the y coordinate of the new position}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will be @code{(x,y)}.
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-to}"
  (%move-to cr (coerce x 'double-float)
               (coerce y 'double-float)))

(export 'move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_move_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rel_move_to" %rel-move-to) :void
  (cr (:pointer (:struct context-t)))
  (dx :double)
  (dy :double))

(defun rel-move-to (cr dx dy)
 #+liber-documentation
 "@version{2025-09-21}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[dx]{a number for the x offset}
  @argument[dy]{a number for the y offset}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will offset by @code{(dx,dy)}.

  Given a current point of @code{(x,y)},
  @begin{pre}
(cairo:rel-move-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo:move-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with an @val[cairo:status-t]{:no-current-point}
  error status.
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
  @end{dictionary}
  @see-class{cairo:context-t}
  @see-function{cairo:move-to}"
  (%rel-move-to cr (coerce dx 'double-float)
                   (coerce dy 'double-float)))

(export 'rel-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_line_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_line_to" %line-to) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double))

(defun line-to (cr x y)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x coordinate of the end of the new line}
  @argument[y]{a number for the y coordinate of the end of the new line}
  @begin{short}
    Adds a line to the path from the current point to position @code{(x,y)} in
    user-space coordinates.
  @end{short}
  After this call the current point will be @code{(x,y)}.

  If there is no current point before the call to the @fun{cairo:line-to}
  function this function will behave as:
  @begin{pre}
(cairo:move-to cr x y)
  @end{pre}
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}"
  (%line-to cr (coerce x 'double-float)
               (coerce y 'double-float)))

(export 'line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_line_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rel_line_to" %rel-line-to) :void
  (cr (:pointer (:struct context-t)))
  (dx :double)
  (dy :double))

(defun rel-line-to (cr dx dy)
 #+liber-documentation
 "@version{2025-09-21}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[dx]{a number for the x offset to the end of the new line}
  @argument[dy]{a number for the y offset to the end of the new line}
  @begin{short}
    Relative-coordinate version of the @fun{cairo:line-to} function.
  @end{short}
  Adds a line to the path from the current point to a point that is offset from
  the current point by @code{(dx,dy)} in user space. After this call the
  current point will be offset by @code{(dx,dy)}.

  Given a current point of @code{(x,y)},
  @begin{pre}
(cairo:rel-line-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo:line-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with an @val[cairo:status-t]{:no-current-point}
  error status.
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-to}"
  (%rel-line-to cr (coerce dx 'double-float)
                   (coerce dy 'double-float)))

(export 'rel-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_curve_to
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
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x1]{a number for the x coordinate of the first control point}
  @argument[y1]{a number for the y coordinate of the first control point}
  @argument[x2]{a number for the x coordinate of the second control point}
  @argument[y2]{a number for the y coordinate of the second control point}
  @argument[x3]{a number for the x coordinate of the third control point}
  @argument[y3]{a number for the y coordinate of the third control point}
  @begin{short}
    Adds a cubic Bezier spline to the path from the current point to position
    @code{(x3,y3)} in user-space coordinates, using @code{(x1,y1)} and
    @code{(x2,y2)} as the control points.
  @end{short}
  After this call the current point will be @code{(x3,y3)}.

  If there is no current point before the call to the @fun{cairo:curve-to}
  function this function will behave as if preceded by a call to:
  @begin{pre}
(cairo:move-to cr x1 y1)
  @end{pre}
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:move-to}"
  (%curve-to cr (coerce x1 'double-float) (coerce y1 'double-float)
                (coerce x2 'double-float) (coerce y2 'double-float)
                (coerce x3 'double-float) (coerce y3 'double-float)))

(export 'curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_curve_to
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
 "@version{2025-09-21}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[dx1]{a number for the x offset to the first control point}
  @argument[dy1]{a number for the y offset to the first control point}
  @argument[dx2]{a number for the x offset to the second control point}
  @argument[dy2]{a number for the y offset to the second control point}
  @argument[dx3]{a number for the x offset to the end of the curve}
  @argument[dy3]{a number for the y offset to the end of the curve}
  @begin{short}
    Relative-coordinate version of the @fun{cairo:curve-to} function.
  @end{short}
  All offsets are relative to the current point. Adds a cubic Bézier spline to
  the path from the current point to a point offset from the current point by
  @code{(dx3,dy3)}, using points offset by @code{(dx1,dy1)} and @code{(dx2,dy2)}
  as the control points. After this call the current point will be offset by
  @code{(dx3,dy3)}.

  Given a current point of @code{(x,y)},
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
  cause @arg{cr} to shutdown with an @val[cairo:status-t]{:no-current-point}
  error status.
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
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
;;; cairo_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rectangle" %rectangle) :void
  (cr (:pointer (:struct context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun rectangle (cr x y width height)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x coordinate of the top left corner of the
    rectangle}
  @argument[y]{a number for the y coordinate to the top left corner of the
    rectangle}
  @argument[width]{a number for the width of the rectangle}
  @argument[height]{a number for the height of the rectangle}
  @begin{short}
    Adds a closed sub-path rectangle of the given size to the current path at
    position @code{(x,y)} in user-space coordinates.
  @end{short}

  This function is logically equivalent to:
  @begin{pre}
(cairo:move-to cr x y)
(cairo:rel-line-to cr width 0)
(cairo:rel-line-to cr 0 height)
(cairo:rel-line-to cr (- width) 0)
(cairo:close-path cr)
  @end{pre}
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
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
;;; cairo_arc
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
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x position of the center of the arc}
  @argument[y]{a number for the y position of the center of the arc}
  @argument[radius]{a number for the radius of the arc}
  @argument[angle1]{a number for the start angle, in radians}
  @argument[angle2]{a number for the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at @code{(x,y)}, begins at @arg{angle1} and proceeds in
  the direction of increasing angles to end at @arg{angle2}. If @arg{angle2} is
  less than @arg{angle1} it will be progressively increased by @code{2*PI} until
  it is greater than @arg{angle1}.

  If there is a current point, an initial line segment will be added to the
  path to connect the current point to the beginning of the arc. If this
  initial line is undesired, it can be avoided by calling the
  @fun{cairo:new-sub-path} function before calling the @fun{cairo:arc} function.

  Angles are measured in radians. An angle of 0 is in the direction of the
  positive X axis (in user space). An angle of @code{PI/2} radians (90 degrees)
  is in the direction of the positive Y axis (in user space). Angles increase
  in the direction from the positive X axis toward the positive y axis. So with
  the default transformation matrix, angles increase in a clockwise direction.

  This function gives the arc in the direction of increasing angles. See the
  @fun{cairo:arc-negative} function to get the arc in the direction of
  decreasing angles.
  @begin[Examples]{dictionary}
    The arc is circular in user space. To achieve an elliptical arc, you can
    scale the current coordinate transformation matrix (CTM) by different
    amounts in the x and y directions. For example, to draw an ellipse in the
    box given by @arg{x}, @arg{y}, @arg{width}, @arg{height}:
    @begin{pre}
(cairo:save cr)
(cairo:translate cr (+ x (/ width 2)) (+ y (/ height 2)))
(cairo:scale cr (/ width 2) (/ height 2))
(cairo:arc cr 0 0 1 0 (* 2 pi))
(cairo:restore cr)
    @end{pre}
  @end{dictionary}
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
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
;;; cairo_arc_negative
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
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x position of the center of the arc}
  @argument[y]{a number for the y position of the center of the arc}
  @argument[radius]{a number for the radius of the arc}
  @argument[angle1]{a number for the start angle, in radians}
  @argument[angle2]{a number for the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at @code{(x,y)}, begins at @arg{angle1} and proceeds in
  the direction of decreasing angles to end at @arg{angle2}. If @arg{angle2} is
  greater than @arg{angle1} it will be progressively decreased by @code{2*PI}
  until it is less than @arg{angle1}.

  See the @fun{cairo:arc} function for more details. This function differs only
  in the direction of the arc between the two angles.
  @begin[Notes]{dictionary}
    The numbers for the arguments are coerced to double floats before being
    passed to the foreign C function.
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
;;; cairo_glyph_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_glyph_path" %glyph-path) :void
  (cr (:pointer (:struct context-t)))
  (glyphs (:pointer (:pointer (:struct glyph-t))))
  (n :int))

(defun glyph-path (cr glyphs)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[glyphs]{a list of glyphs, each glyph is represented by an item that
    is a list with the @code{(index x y)} glyph values}
  @argument[index]{an unsigned integer for the glyph index in the font}
  @argument[x]{a number coerced to a double float for the offset in the x
    direction between the origin used for drawing the string and the
    orgin of this glyph}
  @argument[y]{a number coerced to a double float for the y direction
    between the orgin used for drawing the string and the origin of this glyph}
  @begin{short}
    Adds closed paths for the glyphs to the current path.
  @end{short}
  The generated path if filled, achieves an effect similar to that of the
  @fun{cairo:show-glyphs} function.
  @begin[Examples]{dictionary}
    Get and return the path for the glyph representing the @code{#\\0}
    character.
    @begin{pre}
(cairo:with-context-for-recording-surface (context :color)
  (cairo:glyph-path context '((20 0 10))) ; #\\0
  (cairo:path-data-to-list (cairo:copy-path context)))
=>
(:PATH (:MOVE-TO 3.7265625d0 10.0d0)
       (:LINE-TO 2.84765625d0 10.0d0)
       (:LINE-TO 2.84765625d0 4.3984375d0)
       (:CURVE-TO 2.63671875d0 4.6015625d0
                  2.359375d0 4.8046875d0
                  2.015625d0 5.00390625d0)
       (:CURVE-TO 1.671875d0 5.20703125d0
                  1.36328125d0 5.359375d0
                  1.08984375d0 5.4609375d0)
       (:LINE-TO 1.08984375d0 4.609375d0)
       (:CURVE-TO 1.58203125d0 4.37890625d0
                  2.01171875d0 4.09765625d0
                  2.37890625d0 3.76953125d0)
       (:CURVE-TO 2.74609375d0 3.44140625d0
                  3.0078125d0 3.12109375d0
                  3.16015625d0 2.8125d0)
       (:LINE-TO 3.7265625d0 2.8125d0)
       (:CLOSE-PATH)
       (:MOVE-TO 3.7265625d0 10.0d0))
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:glyph-t}
  @see-function{cairo:show-glyphs}"
  (let ((n (length glyphs)))
    (cffi:with-foreign-object (glyphs-ptr '(:struct glyph-t) n)
      (iter (for i from 0 below n)
            (for (index x y) in glyphs)
            (for ptr = (cffi:mem-aptr glyphs-ptr '(:struct glyph-t) i))
            (setf (cffi:foreign-slot-value ptr '(:struct glyph-t) 'index)
                  index
                  (cffi:foreign-slot-value ptr '(:struct glyph-t) 'x)
                  (coerce x 'double-float)
                  (cffi:foreign-slot-value ptr '(:struct glyph-t) 'y)
                  (coerce y 'double-float)))
      (%glyph-path cr glyphs-ptr n))))

(export 'glyph-path)

;;; ----------------------------------------------------------------------------
;;; cairo_text_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_text_path" text-path) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[utf8]{a string for text encoded in UTF-8, or @code{nil}}
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
  to to the @fun{cairo:text-path} function without having to set the current
  point in between.
  @begin[Notes]{dictionary}
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

;;; --- End of file cairo.path.lisp --------------------------------------------
