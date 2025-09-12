;;; ----------------------------------------------------------------------------
;;; cairo.region.lisp
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
;;; Regions
;;;
;;;     Representing a pixel-aligned area
;;;
;;; Types and Values
;;;
;;;     cairo_region_t
;;;     cairo_region_overlap_t
;;;
;;; Functions
;;;
;;;     cairo_region_create
;;;     cairo_region_create_rectangle
;;;     cairo_region_create_rectangles
;;;     cairo_region_copy
;;;     cairo_region_reference
;;;     cairo_region_destroy
;;;     cairo_region_status
;;;     cairo_region_get_extents
;;;     cairo_region_num_rectangles
;;;     cairo_region_get_rectangle
;;;     cairo_region_is_empty
;;;     cairo_region_contains_point
;;;     cairo_region_contains_rectangle
;;;     cairo_region_equal
;;;
;;;     cairo_region_translate
;;;     cairo_region_intersect
;;;     cairo_region_intersect_rectangle
;;;     cairo_region_subtract
;;;     cairo_region_subtract_rectangle
;;;     cairo_region_union
;;;     cairo_region_union_rectangle
;;;     cairo_region_xor
;;;     cairo_region_xor_rectangle
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_region_overlap_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum region-overlap-t
  :in
  :out
  :part)

#+liber-documentation
(setf (liber:alias-for-symbol 'region-overlap-t)
      "Enum"
      (liber:symbol-documentation 'region-overlap-t)
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum region-overlap-t
  :in
  :out
  :part)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:in]{The contents are entirely inside the region.}
      @entry[:out]{The contents are entirely outside the region.}
      @entry[:part]{The contents are partially inside and partially outside the
        region.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used as the return value for the @fun{cairo:region-contains-rectangle}
    function.
  @end{short}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-contains-rectangle}")

(export 'region-overlap-t)

;;; ----------------------------------------------------------------------------
;;; cairo_region_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct region-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'region-t)
      "CStruct"
      (liber:symbol-documentation 'region-t)
 "@version{2025-09-02}
  @begin{short}
    The @sym{cairo:region-t} structure represents a set of integer aligned
    rectangles.
  @end{short}
  It allows operations like the @fun{cairo:region-union} and
  @fun{cairo:region-intersect} functions to be performed on them.

  Memory management of the @sym{cairo:region-t} structure is done with the
  @fun{cairo:region-reference} and @fun{cairo:region-destroy} functions.
  @see-constructor{cairo:region-create}
  @see-constructor{cairo:region-create-rectangle}
  @see-constructor{cairo:region-create-rectangles}
  @see-constructor{cairo:region-copy}
  @see-function{cairo:region-union}
  @see-function{cairo:region-intersect}
  @see-function{cairo:region-reference}
  @see-function{cairo:region-destroy}")

(export 'region-t)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_create" region-create)
    (:pointer (:struct region-t))
 #+liber-documentation
 "@version{2025-09-02}
  @return{The newly allocated @sym{cairo:region-t} instance.}
  @begin{short}
    Allocates a new empty region instance.
  @end{short}
  Free with the @fun{cairo:region-destroy} function. This function always
  returns a valid instance. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:region-status} function.
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:region-status}")

(export 'region-create)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_create_rectangle" %region-create-rectangle)
    (:pointer (:struct region-t))
  (rect :pointer)) ; pointer to type rectangle-int-t

(defun region-create-rectangle (x y width height)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[x]{an integer for the x coordinate of the left side of the
    rectangle}
  @argument[y]{an integer for the y coordinate of the top side of the
    rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @return{The newly allocated @sym{cairo:region-t} instance.}
  @begin{short}
    Allocates a new @sym{cairo:region-t} instance containing the given
    rectangle.
  @end{short}
  Free with the @fun{cairo:region-destroy} function. This function always
  returns a valid instance. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:region-status} function.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar rect (cairo:region-create-rectangle 10 20 100 200))
=> RECT
(cairo:region-status rect)
=> :SUCCESS
(cairo:region-extents rect)
=> (10 20 100 200)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:region-status}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
    (%region-create-rectangle rect)))

(export 'region-create-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangles
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_create_rectangles" %region-create-rectangles)
    (:pointer (:struct region-t))
  (rects (:pointer (:pointer (:struct rectangle-int-t))))
  (count :int))

(defun region-create-rectangles (&rest rects)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[rects]{rectangles, each rectangle is represented as a
    @code{(x y width height)} list with the values for the rectangle}
  @begin{return}
    The newly allocated @sym{cairo:region-t} instance.
  @end{return}
  @begin{short}
    Allocates a new @sym{cairo:region-t} instance containing the union of all
    given rectangles.
  @end{short}
  Free with the @fun{cairo:region-destroy} function. This function always
  returns a valid instance. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:region-status} function.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar rect (cairo:region-create-rectangles '(0 0 10 20) '(10 20 50 60)))
=> RECT
(cairo:region-status rect)
=> :SUCCESS
(cairo:region-extents rect)
=> (0 0 60 80)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-create-rectangle}
  @see-function{cairo:region-destroy}
  @see-function{cairo:status}"
  (let ((n (length rects)))
    (cffi:with-foreign-object (rects-ptr '(:struct rectangle-int-t) n)
      (iter (for i from 0)
            (for (x1 y1 width1 height1) in rects)
            (for rect = (cffi:mem-aptr rects-ptr '(:struct rectangle-int-t) i))
            (cffi:with-foreign-slots ((x y width height)
                                      rect (:struct rectangle-int-t))
              (setf x x1
                    y y1
                    width width1
                    height height1)))
      (%region-create-rectangles rects-ptr n))))

(export 'region-create-rectangles)

;;; ----------------------------------------------------------------------------
;;; cairo_region_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_copy" region-copy) (:pointer (:struct region-t))
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @return{The newly allocated @sym{cairo:region-t} instance.}
  @begin{short}
    Allocates a new region instance copying the area from @arg{region}.
  @end{short}
  Free with the @fun{cairo:region-destroy} function. This function always
  returns a valid instance. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:region-status} function.
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:region-status}"
  (region (:pointer (:struct region-t))))

(export 'region-copy)

;;; ----------------------------------------------------------------------------
;;; cairo_region_reference
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_reference" region-reference)
    (:pointer (:struct region-t))
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @return{The referenced @sym{cairo:region-t} instance.}
  @begin{short}
    Increases the reference count on @arg{region} by one.
  @end{short}
  This prevents @arg{region} from being destroyed until a matching call to
  the @fun{cairo:region-destroy} function is made.
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}"
  (region (:pointer (:struct region-t))))

(export 'region-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_region_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_destroy" region-destroy) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @short{Destroys a @sym{cairo:region-t} instance.}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-reference}"
  (region (:pointer (:struct region-t))))

(export 'region-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_region_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_status" region-status) status-t
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @begin{return}
    The @sym{cairo:status-t} value that is @code{:success} or @code{:no-memory}.
  @end{return}
  @begin{short}
    Checks whether an error has previous occurred for this region instance.
  @end{short}
  @see-symbol{cairo:region-t}
  @see-symbol{cairo:status-t}"
  (region (:pointer (:struct region-t))))

(export 'region-status)

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_get_extents" %region-extents) :void
  (region (:pointer (:struct region-t)))
  (extents (:pointer (:struct rectangle-int-t))))

(defun region-extents (region)
 #+liber-documentation
 "@version{2025-09-01}
  @syntax{(cairo:region-extents region) => (list x y width height)}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate of the left side of the
    rectangle}
  @argument[y]{an integer for the y coordinate of the top side of the rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The @code{(x y width height)} list with the coordinates of the bounding
    rectangle.
  @end{return}
  @begin{short}
    Gets the bounding rectangle of @arg{region} as a list of the coordinates.
  @end{short}
  @see-symbol{cairo:region-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (%region-extents region rect)
    (cffi:with-foreign-slots ((x y width height)
                              rect (:struct rectangle-int-t))
    (values (list x y width height)))))

(export 'region-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_region_num_rectangles
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_num_rectangles" region-num-rectangles) :int
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @return{The integer for the number of rectangles contained in @arg{region}.}
  @begin{short}
    Returns the number of rectangles contained in @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (region (:pointer (:struct region-t))))

(export 'region-num-rectangles)

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_get_rectangle" %region-rectangle) :void
  (region (:pointer (:struct region-t)))
  (nth :int)
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-rectangle (region nth)
 #+liber-documentation
 "@version{2025-09-01}
  @syntax{(cairo:region-rectangle region nth) => (list x y width height)}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[nth]{an integer indicating which rectangle should be returned}
  @argument[x]{an integer for the x coordinate of the left side of the
    rectangle}
  @argument[y]{an integer for the y coordinate of the top side of the rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The @code{(x y width height)} list with the coordinates of the @arg{nth}
    rectangle.
  @end{return}
  @begin{short}
    Returns the @arg{nth} rectangle from the region as a list with the
    coordinates of the rectangle.
  @end{short}
  @see-symbol{cairo:region-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (%region-rectangle region nth rect)
    (cffi:with-foreign-slots ((x y width height)
                              rect (:struct rectangle-int-t))
    (values (list x y width height)))))

(export 'region-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_is_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_is_empty" region-is-empty) :bool
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @return{@em{True} if @arg{region} is empty, @em{false} if it is not.}
  @short{Checks whether @arg{region} is empty.}
  @see-symbol{cairo:region-t}"
  (region (:pointer (:struct region-t))))

(export 'region-is-empty)

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_contains_point" region-contains-point) :bool
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate of a point}
  @argument[y]{an integer for the y coordinate of a point}
  @begin{return}
    @em{True} if @code{(x,y)} is contained in @arg{region}, @em{false} if it
    is not.
  @end{return}
  @short{Checks whether @code{(x,y)} is contained in @arg{region}.}
  @see-symbol{cairo:region-t}"
  (region (:pointer (:struct region-t)))
  (x :int)
  (y :int))

(export 'region-contains-point)

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_contains_rectangle" %region-contains-rectangle)
    region-overlap-t
  (region (:pointer (:struct region-t)))
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-contains-rectangle (region x y width height)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate of the left side of the
    rectangle}
  @argument[y]{an integer for the y coordinate of the top side of the
    rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The @sym{cairo:region-overlap-t} value.
  @end{return}
  @begin{short}
    Checks whether the given rectangle is inside, outside or partially contained
    in @arg{region}.
  @end{short}
  Returns the @code{:in} value if @arg{rectangle} is entirely inside
  @arg{region}, the @code{:out} value if @arg{rectangle} is entirely outside
  @arg{region}, or the @code{:part} value if @arg{rectangle} is partially inside
  and partially outside @arg{region}.
  @see-symbol{cairo:region-t}
  @see-symbol{cairo:region-overlap-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
      (%region-contains-rectangle region rect)))

(export 'region-contains-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_equal" region-equal) :bool
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region1]{a @sym{cairo:region-t} instance}
  @argument[region2]{a @sym{cairo:region-t} instance}
  @begin{return}
    @em{True} if both regions contained the same coverage, @em{false} if it is
    not or any region is in an error status.
  @end{return}
  @begin{short}
    Compares whether @arg{region1} is equivalent to @arg{region2}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (region1 (:pointer (:struct region-t)))
  (region2 (:pointer (:struct region-t))))

(export 'region-equal)

;;; ----------------------------------------------------------------------------
;;; cairo_region_translate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_translate" region-translate) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[dx]{an integer for the amount to translate in the x direction}
  @argument[dy]{an integer for the amount to translate in the y direction}
  @short{Translates the region by @code{(dx, dy)}.}
  @see-symbol{cairo:region-t}"
  (region (:pointer (:struct region-t)))
  (dx :int)
  (dy :int))

(export 'region-translate)

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_intersect" %region-intersect) status-t
  (region (:pointer (:struct region-t)))
  (other (:pointer (:struct region-t))))

(defun region-intersect (region other)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[other]{another @sym{cairo:region-t} instance}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the intersection or
    @code{nil} if an error occured.
  @end{return}
  @begin{short}
    Computes the intersection of @arg{region} with @arg{other} and places the
    result in @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (when (eq :success
            (%region-intersect region other))
    region))

(export 'region-intersect)

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_intersect_rectangle" %region-intersect-rectangle)
    status-t
  (region (:pointer (:struct region-t)))
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-intersect-rectangle (region x y width height)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate with the left side of the
    rectangle}
  @argument[y]{an integer for the y coordinate with the top side of the
    rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the intersection or
    @code{nil} if an error occured.
  @end{return}
  @begin{short}
    Computes the intersection of @arg{region} with the given rectangle and
    places the result in @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
    (when (eq :success
              (%region-intersect-rectangle region rect))
      region)))

(export 'region-intersect-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_subtract" %region-subtract) status-t
  (region (:pointer (:struct region-t)))
  (other (:pointer (:struct region-t))))

(defun region-subtract (region other)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[other]{another @sym{cairo:region-t} instance}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Subtracts @arg{other} from @arg{region} and places the result in
    @arg{region.}
  @end{short}
  @see-symbol{cairo:region-t}"
  (when (eq :success
            (%region-subtract region other))
    region))

(export 'region-subtract)

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_subtract_rectangle" %region-subtract-rectangle)
    status-t
  (region (:pointer (:struct region-t)))
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-subtract-rectangle (region x y width height)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate}
  @argument[y]{an integer for the y coordinate}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Subtracts @arg{rectangle} from @arg{region} and places the result in
    @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
    (when (eq :success
              (%region-subtract-rectangle region rect))
      region)))

(export 'region-subtract-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_union
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_union" %region-union) status-t
  (region (:pointer (:struct region-t)))
  (other (:pointer (:struct region-t))))

(defun region-union (region other)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[other]{another @sym{cairo:region-t} instance}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Computes the union of @arg{region} with @arg{other} and places the result
    in @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (when (eq :success
            (%region-union region other))
    region))

(export 'region-union)

;;; ----------------------------------------------------------------------------
;;; cairo_region_union_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_union_rectangle" %region-union-rectangle) status-t
  (region (:pointer (:struct region-t)))
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-union-rectangle (region x y width height)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate}
  @argument[y]{an integer for the y coordinate}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Computes the union of @arg{region} with @arg{rectangle} and places the
    result in @arg{region}.
  @end{short}
  @see-symbol{cairo:region-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
    (when (eq :success
              (%region-union-rectangle region rect))
      region)))

(export 'region-union-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_xor" %region-xor) status-t
  (region (:pointer (:struct region-t)))
  (other (:pointer (:struct region-t))))

(defun region-xor (region other)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[other]{another @sym{cairo:region-t} instance}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Computes the exclusive difference of @arg{region} with @arg{other} and
    places the result in @arg{region}.
  @end{short}
  That is, @arg{region} will be set to contain all areas that are either in
  @arg{region} or in @arg{other}, but not in both.
  @see-symbol{cairo:region-t}"
  (when (eq :success
            (%region-xor region other))
    region))

(export 'region-xor)

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_region_xor_rectangle" %region-xor-rectangle) status-t
  (region (:pointer (:struct region-t)))
  (rectangle (:pointer (:struct rectangle-int-t))))

(defun region-xor-rectangle (region x y width height)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[region]{a @sym{cairo:region-t} instance}
  @argument[x]{an integer for the x coordinate}
  @argument[y]{an integer for the y coordinate}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{return}
    The passed in @sym{cairo:region-t} instance for the result or @code{nil}
    if an error occured.
  @end{return}
  @begin{short}
    Computes the exclusive difference of @arg{region} with @arg{rectangle} and
    places the result in @arg{region}.
  @end{short}
  That is, @arg{region} will be set to contain all areas that are either in
  @arg{region} or in @arg{rectangle}, but not in both.
  @see-symbol{cairo:region-t}
  @see-symbol{cairo:rectangle-int-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-int-t))
    (setf (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'x) x
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'y) y
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'width)
          width
          (cffi:foreign-slot-value rect '(:struct rectangle-int-t) 'height)
          height)
    (when (eq :success
              (%region-xor-rectangle region rect))
      region)))

(export 'region-xor-rectangle)

;;; --- End of file cairo.region.lisp ------------------------------------------
