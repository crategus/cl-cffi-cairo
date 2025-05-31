;;; ----------------------------------------------------------------------------
;;; cairo.matrix.lisp
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

(cffi:defcstruct matrix-t
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
 "@version{2025-1-18}
  @begin{declaration}
(cffi:defcstruct matrix-t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[xx]{The double float with the xx component.}
      @entry[yx]{The double float with the yx component.}
      @entry[xy]{The double float with the xy component.}
      @entry[yy]{The double float with the yy component.}
      @entry[x0]{The double float with the x translation component.}
      @entry[y0]{The double float with the y translation component.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:matrix-t} structure holds an affine transformation, such
    as a scale, rotation, shear, or a combination of those.
  @end{short}
  The transformation of a point @code{(x,y)} is given by:
  @begin{pre}
xnew = xx * x + xy * y + x0
ynew = yx * x + yy * y + y0
  @end{pre}
  The current transformation matrix (CTM) of a @symbol{cairo:context-t}
  instance, represented as a @symbol{cairo:matrix-t} structure, defines the
  transformation from user space coordinates to device space coordinates. See
  also the @fun{cairo:matrix} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:matrix}")

(export 'matrix-t)

;;; ----------------------------------------------------------------------------

(defmacro with-matrix ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-05-09}
  @syntax{(cairo:with-matrix (matrix) body) => result}
  @syntax{(cairo:with-matrix (matrix rad) body) => result}
  @syntax{(cairo:with-matrix (matrix :translate tx ty) body) => result}
  @syntax{(cairo:with-matrix (matrix :scale sx sy) body) => result}
  @syntax{(cairo:with-matrix (matrix xx yx xy yy x0 y0) body) => result}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to create and initialize}
  @argument[rad]{a number coerced to a double float for the angle of rotation,
    in radians}
  @argument[tx]{a number for the amount to tanslate in the x direction}
  @argument[ty]{a number for the amount to tanslate in the y direction}
  @argument[sx]{a number for the scale factor in the x direction}
  @argument[sy]{a number for the scale factor in the y direction}
  @argument[xx]{a number for the xx component of the transformation}
  @argument[yx]{a number for the yx component of the transformation}
  @argument[xy]{a number for the xy component of the transformation}
  @argument[yy]{a number for the yy component of the transformation}
  @argument[x0]{a number for the x translation component of the transformation}
  @argument[y0]{a number for the y translation component of the transformation}
  @begin{short}
    The @fun{cairo:with-matrix} macro allocates a new @symbol{cairo:matrix-t}
    instance, initializes the matrix with the given values and executes the
    body that uses the matrix.
  @end{short}
  After execution of the body the allocated memory for the matrix is released.

  If no argument is given, the matrix is initialized to the identity
  transformation with the @fun{cairo:matrix-init-identity} function. The
  initialization with one argument initializes a rotation with the
  @fun{cairo:matrix-init-rotate} function. The initialization with three
  arguments initializes a translation with the @fun{cairo:matrix-init-translate}
  function or a transformation which scales with the
  @fun{cairo:matrix-init-scale} function. When six numbers are given the matrix
  is initialized with the @fun{cairo:matrix-init} function.
  @begin[Notes]{dictionary}
    The arguments are coerced to double floats before being passed to the
    foreign C functions.
  @end{dictionary}
  @see-symbol{cairo:matrix-t}
  @see-macro{cairo:with-matrices}
  @see-function{cairo:matrix-init}
  @see-function{cairo:matrix-init-identity}
  @see-function{cairo:matrix-init-rotate}
  @see-function{cairo:matrix-init-translate}
  @see-function{cairo:matrix-init-scale}"
  (cond ((null args)
         ;; No arguments, initialize with the identity transformation
         `(cffi:with-foreign-object (,var '(:struct matrix-t))
            (matrix-init-identity ,var)
            (progn ,@body)))
        ((null (second args))
         ;; One argument, initialize a rotation
         `(cffi:with-foreign-object (,var '(:struct matrix-t))
            (matrix-init-rotate ,var ,@args)
            (progn ,@body)))
        ((null (fourth args))
         ;; Three arguments, translation or scale
         (destructuring-bind (type arg1 arg2) args
           (cond ((eq :scale type)
                  `(cffi:with-foreign-object (,var '(:struct matrix-t))
                     (matrix-init-scale ,var ,arg1 ,arg2)
                     (progn ,@body)))
                 ((eq :translate type)
                  `(cffi:with-foreign-object (,var '(:struct matrix-t))
                     (matrix-init-translate ,var ,arg1 ,arg2)
                     (progn ,@body)))
                 (t
                  (error "Syntax error in CAIRO:WITH-MATRIX")))))
        ((null (seventh args))
         ;; Six arguments for initialization
         `(cffi:with-foreign-object (,var '(:struct matrix-t))
             (matrix-init ,var ,@args)
             (progn ,@body)))
        (t
         (error "Syntax error in CAIRO:WITH-MATRIX"))))

(export 'with-matrix)

(defmacro with-matrices (vars &body body)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:with-matrices (matrix1 ... matrixn) body) => result}
  @argument[matrix1 ... matrixn]{newly created @symbol{cairo:matrix-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{matrix1 ... matrixn}}
  @begin{short}
    The @fun{cairo:with-matrices} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @code{let*} macro.

  Each matrix can be initialized with values using the syntax for the
  @fun{cairo:with-matrix} macro. See also the @fun{cairo:with-matrix}
  documentation.
  @see-symbol{cairo:matrix-t}
  @see-macro{cairo:with-matrix}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-matrix ,var
           (with-matrices ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-matrices)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init
;;; ----------------------------------------------------------------------------

(defun matrix-init (matrix xx yx xy yy x0 y0)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[xx]{a number with the xx component of the transformation}
  @argument[yx]{a number with the yx component of the transformation}
  @argument[xy]{a number with the xy component of the transformation}
  @argument[yy]{a number with the yy component of the transformation}
  @argument[x0]{a number with the x translation component of the transformation}
  @argument[y0]{a number with the y translation component of the transformation}
  @return{The initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Sets the matrix to be the affine transformation given by the @arg{xx},
    @arg{yx}, @arg{xy}, @arg{yy}, @arg{x0}, @arg{y0} arguments.
  @end{short}
  The transformation is given by:
  @begin{pre}
xnew = xx * x + xy * y + x0
ynew = yx * x + yy * y + y0
  @end{pre}
  The @fun{cairo:with-matrix} and @fun{cairo:with-matrices} macros are more
  convenient for defining and initialising a matrix in one step.
  @begin[Examples]{dictionary}
    The @symbol{cairo:matrix-t} structure is a foreign CFFI type. Therefore, to
    create a matrix, we must define a foreign object:
    @begin{pre}
(cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
  (cairo:matrix-init matrix 0.5 0.0 0.0 1.0 2.0 3.0)
  (cairo:matrix-to-float matrix))
=> (0.5d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
    @end{pre}
    Defining and initialising a matrix with the @fun{cairo:with-matrix} macro.
    @begin{pre}
(cairo:with-matrix (matrix 1/2 0 0 1 2 3)
  (cairo:matrix-to-float matrix))
=> (0.5d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
    @end{pre}
  @end{dictionary}
  @begin[Notes]{dictionary}
    The arguments are coerced to double floats before being passed to the
    foreign C functions.
  @end{dictionary}
  @see-symbol{cairo:matrix-t}
  @see-macro{cairo:with-matrix}
  @see-macro{cairo:with-matrices}"
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
;;; cairo_matrix_init_identity
;;; ----------------------------------------------------------------------------

(defun matrix-init-identity (matrix)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @return{The @symbol{cairo:matrix-t} instance set to be an identity
    transformation.}
  @begin{short}
    Modifies the matrix to be an identity transformation.
  @end{short}
  @begin[Examples]{dictionary}
    The @fun{cairo:with-matrix} initialises the matrix to the identity
    transformation when no values are given.
    @begin{pre}
(cairo:with-matrix (matrix)
  (cairo:matrix-to-float matrix))
=> (1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:with-matrix}"
  (cffi:foreign-funcall "cairo_matrix_init_identity"
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(export 'matrix-init-identity)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_translate
;;; ----------------------------------------------------------------------------

(defun matrix-init-translate (matrix tx ty)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[tx]{a number coerced to a double float for the amount to
    tanslate in the x direction}
  @argument[ty]{a number coerced to a double float for the amount to
    tanslate in the y direction}
  @return{The initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that translates by @arg{tx}
    and @arg{ty} in the x and y dimensions, respectively.
  @end{short}
  @begin[Examples]{dictionary}
    The @fun{cairo:with-matrix} macro uses this function for initialising the
    matrix for a translation.
    @begin{pre}
(cairo:with-matrix (matrix :translate 1/2 2)
  (cairo:matrix-to-float matrix))
=> (1.0d0 0.0d0 0.0d0 1.0d0 0.5d0 2.0d0)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:with-matrix}"
  (cffi:foreign-funcall "cairo_matrix_init_translate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce tx 'double-float)
                        :double (coerce ty 'double-float)
                        :void)
  matrix)

(export 'matrix-init-translate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_scale
;;; ----------------------------------------------------------------------------

(defun matrix-init-scale (matrix sx sy)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[sx]{a number coerced to a double float for the scale factor
    in the x direction}
  @argument[sy]{a number coerced to a double float for the scale factor
    in the y direction}
  @return{The initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that scales by @arg{sx} and
    @arg{sy} in the x and y dimensions, respectively.
  @end{short}
  @begin[Examples]{dictionary}
    The @fun{cairo:with-matrix} macro uses this function for initialising the
    matrix for a transformation that scales.
    @begin{pre}
(cairo:with-matrix (matrix :scale  1/2 2)
  (cairo:matrix-to-float matrix))
=> (0.5d0 0.0d0 0.0d0 2.0d0 0.0d0 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_init_scale"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce sx 'double-float)
                        :double (coerce sy 'double-float)
                        :void)
  matrix)

(export 'matrix-init-scale)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_rotate
;;; ----------------------------------------------------------------------------

(defun matrix-init-rotate (matrix angle)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance to initialize}
  @argument[angle]{a number coerced to a double float for the angle of rotation,
    in radians}
  @return{The initialized @symbol{cairo:matrix-t} instance.}
  @begin{short}
    Initializes the matrix to a transformation that rotates by @arg{angle}.
  @end{short}
  The direction of rotation is defined such that positive angles rotate in the
  direction from the positive X axis toward the positive Y axis. With the
  default axis orientation of Cairo, positive angles rotate in a clockwise
  direction.
  @begin[Examples]{dictionary}
    The @fun{cairo:with-matrix} macro uses this function for initialising the
    matrix for a transformation that rotates.
    @begin{pre}
(cairo:with-matrix (matrix (/ pi 2))
  (cairo:matrix-to-float matrix))
=> (6.123233995736766d-17 1.0d0 -1.0d0 6.123233995736766d-17 0.0d0 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:with-matrix}"
  (cffi:foreign-funcall "cairo_matrix_init_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce angle 'double-float)
                        :void)
  matrix)

(export 'matrix-init-rotate)

;;; ----------------------------------------------------------------------------
;;; cairo:matrix-to-float
;;; ----------------------------------------------------------------------------

(defun matrix-to-float (matrix)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @return{The list with double floats for the components of the matrix.}
  @begin{short}
    Converts the matrix to a list of double floats.
  @end{short}
  @begin[Notes]{dictionary}
    This function is a Lisp extension and not present in the C library.
  @end{dictionary}
  @see-symbol{cairo:matrix-t}"
  (cffi:with-foreign-slots ((xx yx xy yy x0 y0) matrix (:struct matrix-t))
    (list xx yx xy yy x0 y0)))

(export 'matrix-to-float)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_translate
;;; ----------------------------------------------------------------------------

(defun matrix-translate (matrix tx ty)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[tx]{a number coerced to a double float for the amount to
    tanslate in the x direction}
  @argument[ty]{a number coerced to a double float for the amount to
    tanslate in the y direction}
  @return{The @symbol{cairo:matrix-t} instance with the applied translation.}
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
;;; cairo_matrix_scale
;;; ----------------------------------------------------------------------------

(defun matrix-scale (matrix sx sy)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[sx]{a number coerced to a double float for the scale factor
    in the x direction}
  @argument[sy]{a number coerced to a double float for the scale factor
    in the y direction}
  @return{The @symbol{cairo:matrix-t} instance with the applied scaling.}
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
;;; cairo_matrix_rotate
;;; ----------------------------------------------------------------------------

(defun matrix-rotate (matrix angle)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[angle]{a number coerced to a double float for the angle of rotation,
    in radians}
  @return{The @symbol{cairo:matrix-t} instance with the applied rotation.}
  @begin{short}
    Applies rotation by @arg{angle} to the transformation in the matrix.
  @end{short}
  The effect of the new transformation is to first rotate the coordinates by
  @arg{angle}, then apply the original transformation to the coordinates.

  The direction of rotation is defined such that positive angles rotate in the
  direction from the positive X axis toward the positive Y axis. With the
  default axis orientation of Cairo, positive angles rotate in a clockwise
  direction.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_rotate"
                        (:pointer (:struct matrix-t)) matrix
                        :double (coerce angle 'double-float)
                        :void)
  matrix)

(export 'matrix-rotate)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_invert
;;; ----------------------------------------------------------------------------

(defun matrix-invert (matrix)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @return{The inversed @symbol{cairo:matrix-t} instance if @arg{matrix} has an
    inverse, otherwise @code{nil}.}
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
;;; cairo_matrix_multiply
;;; ----------------------------------------------------------------------------

(defun matrix-multiply (matrix a b)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance for the result}
  @argument[a]{a @symbol{cairo:matrix-t} instance}
  @argument[b]{a @symbol{cairo:matrix-t} instance}
  @return{The @symbol{cairo:matrix-t} instance with the result.}
  @begin{short}
    Multiplies the affine transformations in @arg{a} and @arg{b} together,
  @end{short}
  The effect of the resulting transformation is to first apply the
  transformation in  @arg{a} to the coordinates and then apply the
  transformation in @arg{b} to the coordinates.

  It is allowable for @arg{matrix} to be identical to either @arg{a} or @arg{b}.
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_matrix_multiply"
                        (:pointer (:struct matrix-t)) matrix
                        (:pointer (:struct matrix-t)) a
                        (:pointer (:struct matrix-t)) b
                        :void)
  matrix)

(export 'matrix-multiply)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_transform_distance
;;; ----------------------------------------------------------------------------

(defun matrix-transform-distance (matrix dx dy)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:transform-distance matrix dx dy) => tdx, tdy}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[dx]{a number coerced to a double float for the x component of
    a distance vector}
  @argument[dy]{a number coerced to a double float for the y component of
    a distance vector}
  @argument[tdx]{a double float for the transformed x component of a
    distance vector}
  @argument[tdy]{a double float for the transformed y component of a
    distance vector}
  @begin{short}
    Transforms the distance vector @code{(dx,dy)} by @arg{matrix}.
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
  (cffi:with-foreign-objects ((tdx :double) (tdy :double))
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
;;; cairo_matrix_transform_point
;;; ----------------------------------------------------------------------------

(defun matrix-transform-point (matrix x y)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:transform-distance matrix x y) => tx, ty}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @argument[x]{a number coerced to a double float for the x position}
  @argument[y]{a number coerced to a double float for the y position}
  @argument[tx]{a double float for the transformed x position}
  @argument[ty]{a double float for the transformed y position}
  @begin{short}
    Transforms the point @code{(dx,dy)} by @arg{matrix}.
  @end{short}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:matrix-transform-distance}"
  (cffi:with-foreign-objects ((tx :double) (ty :double))
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
