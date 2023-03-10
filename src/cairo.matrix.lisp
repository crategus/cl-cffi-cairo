;;; ----------------------------------------------------------------------------
;;; cairo.matrix.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; cairo_matrix_t
;;;
;;;     Generic matrix operations
;;;
;;; Types and Values
;;;
;;;     cairo_matrix_t
;;;
;;; Functions
;;;
;;;     cairo_matrix_init
;;;     cairo_matrix_init_identity
;;;     cairo_matrix_init_translate
;;;     cairo_matrix_init_scale
;;;     cairo_matrix_init_rotate
;;;     cairo_matrix_translate
;;;     cairo_matrix_scale
;;;     cairo_matrix_rotate
;;;     cairo_matrix_invert
;;;     cairo_matrix_multiply
;;;     cairo_matrix_transform_distance
;;;     cairo_matrix_transform_point
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_t
;;; ----------------------------------------------------------------------------

(defcstruct matrix-t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'matrix-t)
      "CStruct"
      (liber:symbol-documentation 'matrix-t)
 "@version{#2022-10-4}
  @begin{short}
    A @sym{cairo:matrix-t} structure holds an affine transformation, such as a
    scale, rotation, shear, or a combination of those.
  @end{short}
  The transformation of a point (x, y) is given by:
  @begin{pre}
xnew = xx * x + xy * y + x0
ynew = yx * x + yy * y + y0
  @end{pre}
  The current transformation matrix of a @symbol{cairo:context-t} context,
  represented as a @sym{cairo:matrix-t} structure, defines the transformation
  from user space coordinates to device space coordinates. See the
  @fun{cairo:matrix} function.
  @begin{pre}
(defcstruct matrix-t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))
  @end{pre}
  @begin[code]{table}
    @entry[xx]{A double float xx component of the affine transformation.}
    @entry[yx]{A double float yx component of the affine transformation.}
    @entry[xy]{A double float xy component of the affine transformation.}
    @entry[yy]{A double float yy component of the affine transformation.}
    @entry[x0]{A double float x translation component of the affine
      transformation.}
    @entry[y0]{A double float y translation component of the affine
      transformation.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:matrix}")

(export 'matrix-t)

;;; ----------------------------------------------------------------------------

;; Lisp function to get the values of a Cairo matrix-t structure

(defun matrix-to-float (matrix)
 #+liber-documentation
 "@version{#2022-10-7}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @return{A list with the floating point values.}
  @begin{short}
    Converts the matrix to a list of floating point values.
  @end{short}
  @begin[Note]{dictionary}
    This function is a Lisp extension and not present in the C library.
  @end{dictionary}
  @see-symbol{cairo:matrix-t}"
  (with-foreign-slots ((xx yx xy yy x0 y0) matrix (:struct matrix-t))
    (list xx yx xy yy x0 y0)))

(export 'matrix-to-float)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init ()
;;; ----------------------------------------------------------------------------

(defun matrix-init (matrix xx yx xy yy x0 y0)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[xx]{a number coerced to a double float with the xx component of
    the affine transformation}
  @argument[yx]{a number coerced to a double float with the yx component of
    the affine transformation}
  @argument[xy]{a number coerced to a double float with the xy component of
    the affine transformation}
  @argument[yy]{a number coerced to a double float with the yy component of
    the affine transformation}
  @argument[x0]{a number coerced to a double float with the x translation
    component of the affine transformation}
  @argument[y0]{a number coerced to a double float with the y translation
    component of the affine transformation}
  @return{A initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Sets the matrix to be the affine transformation given by xx, yx, xy, yy,
    x0, y0.
  @end{short}
  The transformation is given by:
  @begin{pre}
xnew = xx * x + xy * y + x0
ynew = yx * x + yy * y + y0
  @end{pre}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce xx 'double-float)
                        :double (coerce yx 'double-float)
                        :double (coerce xy 'double-float)
                        :double (coerce yy 'double-float)
                        :double (coerce x0 'double-float)
                        :double (coerce y0 'double-float)
                        :void)
  matrix)

(export 'matrix-init)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_identity ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-identity (matrix)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @return{A @symbol{cairo:matrix-t} instance set to be an identity transformation.}
  @begin{short}
    Modifies the matrix to be an identity transformation.
  @end{short}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init_identity"
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(export 'matrix-init-identity)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_translate ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-translate (matrix tx ty)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[tx]{a number coerced to a double float with the amount to
    tanslate in the x direction}
  @argument[ty]{a number coerced to a double float with the amount to
    tanslate in the y direction}
  @return{A initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that translates by  @arg{tx}
    and @arg{ty} in the x and y dimensions, respectively.
  @end{short}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init_translate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce tx 'double-float)
                        :double (coerce ty 'double-float)
                        :void)
  matrix)

(export 'matrix-init-translate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_scale ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-scale (matrix sx sy)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[sx]{a number coerced to a double float with the scale factor
    in the x direction}
  @argument[sy]{a number coerced to a double float with the scale factor
    in the y direction}
  @return{A initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that scales by @arg{sx} and
    @arg{sy} in the x and y dimensions, respectively.
  @end{short}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init_scale"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce sx 'double-float)
                        :double (coerce sy 'double-float)
                        :void)
  matrix)

(export 'matrix-init-scale)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_rotate ()
;;; ----------------------------------------------------------------------------

(defun matrix-init-rotate (matrix radians)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[radians]{a number coerced to a double float with the angle of
    rotation, in radians}
  @return{A initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that rotates by @arg{radians}.
  @end{short}
  The direction of rotation is defined such that positive angles rotate in the
  direction from the positive x axis toward the positive y axis. With the
  default axis orientation of Cairo, positive angles rotate in a clockwise
  direction.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce radians 'double-float)
                        :void)
  matrix)

(export 'matrix-init-rotate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_translate ()
;;; ----------------------------------------------------------------------------

(defun matrix-translate (matrix tx ty)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[tx]{a number coerced to a double float with the amount to
    tanslate in the x direction}
  @argument[ty]{a number coerced to a double float with the amount to
    tanslate in the y direction}
  @return{A @symbol{cairo:matrix-t} instance with the applied translation.}
  @begin{short}
    Applies a translation by @arg{tx}, @arg{ty} to the transformation in
    the matrix.
  @end{short}
  The effect of the new transformation is to first translate the coordinates
  by @arg{tx} and @arg{ty}, then apply the original transformation to the
  coordinates.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_translate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce tx 'double-float)
                        :double (coerce ty 'double-float)
                        :void)
  matrix)

(export 'matrix-translate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_scale ()
;;; ----------------------------------------------------------------------------

(defun matrix-scale (matrix sx sy)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[sx]{a number coerced to a double float with the scale factor
    in the x direction}
  @argument[sy]{a number coerced to a double float with the scale factor
    in the y direction}
  @return{A @symbol{cairo:matrix-t} instance with the applied scaling.}
  @begin{short}
    Applies scaling by @arg{sx} and @arg{sy} to the transformation in the
    matrix.
  @end{short}
  The effect of the new transformation is to first scale the coordinates by
  @arg{sx} and @arg{sy}, then apply the original transformation to the
  coordinates.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_scale"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce sx 'double-float)
                        :double (coerce sy 'double-float)
                        :void)
  matrix)

(export 'matrix-scale)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_rotate ()
;;; ----------------------------------------------------------------------------

(defun matrix-rotate (matrix radians)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[radians]{a number coerced to a double float with the angle of
    rotation, in radians}
  @return{A @symbol{cairo:matrix-t} instance with the applied rotation.}
  @begin{short}
    Applies rotation by @arg{radians} to the transformation in the matrix.
  @end{short}
  The effect of the new transformation is to first rotate the coordinates by
  @arg{radians}, then apply the original transformation to the coordinates.

  The direction of rotation is defined such that positive angles rotate in the
  direction from the positive x axis toward the positive y axis. With the
  default axis orientation of Cairo, positive angles rotate in a clockwise
  direction.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce radians 'double-float)
                        :void)
  matrix)

(export 'matrix-rotate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_invert ()
;;; ----------------------------------------------------------------------------

(defun matrix-invert (matrix)
 #+liber-documentation
 "@version{#2022-10-4}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @return{An inversed @symbol{cairo:matrix-t} instance if @arg{matrix} has an
    inverse, otherwise @em{false}.}
  @begin{short}
    Changes the matrix to be the inverse of its original value.
  @end{short}
  Not all transformation matrices have inverses. If the matrix collapses points
  together, it is degenerate, then it has no inverse and this function will
  fail.
  @see-symbol{cairo:matrix-t}"
  (when (eq :success
            (cffi:foreign-funcall "cairo_matrix_invert"
                                  (:pointer (:struct matrix-t)) matrix
                                  status-t))
    matrix))

(export 'matrix-invert)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_multiply ()
;;; ----------------------------------------------------------------------------

(defun matrix-multiply (result a b)
 #+liber-documentation
 "@version{#2022-10-7}
  @argument[result]{a @symbol{cairo:matrix-t} instance for the result}
  @argument[a]{a @symbol{cairo:matrix-t} instance}
  @argument[b]{a @symbol{cairo:matrix-t} instance}
  @return{A @symbol{cairo:matrix-t} instance with the result.}
  @begin{short}
    Multiplies the affine transformations in  @arg{a} and @arg{b} together
  @end{short}
  The effect of the resulting transformation is to first apply the
  transformation in  @arg{a} to the coordinates and then apply the
  transformation in @arg{b} to the coordinates.

  It is allowable for @arg{result} to be identical to either @arg{a} or @arg{b}.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_multiply"
                        (:pointer (:struct matrix-t)) result
                        (:pointer (:struct matrix-t)) a
                        (:pointer (:struct matrix-t)) b
                        :void)
  result)

(export 'matrix-multiply)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_transform_distance ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-distance (matrix dx dy)
 #+liber-documentation
 "@version{#2022-10-7}
  @syntax[]{(cairo:transform-distance matrix dx dy) => tdx, tdy}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[dx]{a number coerced to a double float with the x component of
    a distance vector}
  @argument[dy]{a number coerced to a double float with the y component of
    a distance vector}
  @argument[tdx]{a double float with the transformed x component of a
    distance vector}
  @argument[tdy]{a double float with the transformed y component of a
    distance vector}
  @begin{short}
    Transforms the distance vector (@arg{dx},@arg{dy}) by @arg{matrix}.
  @end{short}
  This is similar to the @fun{cairo:matrix-transform-point} function except that
  the translation components of the transformation are ignored. The calculation
  of the returned vector is as follows:
  @begin{pre}
tdx = dx * a + dy * c
tdy = dx * b + dy * d
  @end{pre}
  Affine transformations are position invariant, so the same vector always
  transforms to the same vector. If @code{(x1,y1)} transforms to
  @code{(x2,y2)} then @code{(x1+dx1,y1+dy1)} will transform to
  @code{(x1+dx2,y1+dy2)} for all values of @code{x1} and @code{x2}.
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:matrix-transform-point}"
  (with-foreign-objects ((tdx :double) (tdy :double))
    (setf (cffi:mem-ref tdx :double) (coerce dx 'double-float))
    (setf (cffi:mem-ref tdy :double) (coerce dy 'double-float))
    (cffi:foreign-funcall "cairo_matrix_transform_distance"
                          (:pointer (:struct matrix-t)) matrix
                          (:pointer :double) tdx
                          (:pointer :double) tdy
                          :void)
    (values (cffi:mem-ref tdx :double) (cffi:mem-ref tdy :double))))

(export 'matrix-transform-distance)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_transform_point ()
;;; ----------------------------------------------------------------------------

(defun matrix-transform-point (matrix x y)
 #+liber-documentation
 "@version{#2022-10-7}
  @syntax[]{(transform-distance matrix x y) => tx, ty}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[x]{a number coerced to a double float with the x position}
  @argument[y]{a number coerced to a double float with the y position}
  @argument[tx]{a double float with the transformed x position}
  @argument[ty]{a double float with the transformed y position}
  @begin{short}
    Transforms the point (@arg{dx},@arg{dy}) by @arg{matrix}.
  @end{short}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:matrix-transform-distance}"
  (with-foreign-objects ((tx :double) (ty :double))
    (setf (cffi:mem-ref tx :double) (coerce x 'double-float))
    (setf (cffi:mem-ref ty :double) (coerce y 'double-float))
    (cffi:foreign-funcall "cairo_matrix_transform_point"
                          (:pointer (:struct matrix-t)) matrix
                          (:pointer :double) tx
                          (:pointer :double) ty
                          :void)
    (values (cffi:mem-ref tx :double) (cffi:mem-ref ty :double))))

(export 'matrix-transform-point)

;;; --- End of file cairo.matrix.lisp ------------------------------------------
