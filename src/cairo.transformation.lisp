;;; ----------------------------------------------------------------------------
;;; cairo.transformation.lisp
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
;;; Transformations
;;;
;;;     Manipulating the current transformation matrix
;;;
;;; Functions
;;;
;;;     cairo_translate
;;;     cairo_scale
;;;     cairo_rotate
;;;     cairo_transform
;;;     cairo_set_matrix
;;;     cairo_get_matrix
;;;     cairo_identity_matrix
;;;     cairo_user_to_device
;;;     cairo_user_to_device_distance
;;;     cairo_device_to_user
;;;     cairo_device_to_user_distance
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_translate
;;; ----------------------------------------------------------------------------

(defun translate (cr tx ty)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[tx]{a number coerced to a double float for the amount to translate
    in the x direction}
  @argument[ty]{a number coerced to a double float for the amount to translate
    in the y direction}
  @begin{short}
    Modifies the current transformation matrix (CTM) by translating the
    user-space origin by @code{(tx,ty)}.
  @end{short}
  This offset is interpreted as a user-space coordinate according to the CTM in
  place before the new call to the @fun{cairo:translate} function. In other
  words, the translation of the user-space origin takes place after any existing
  transformation.
  @see-symbol{cairo:context-t}"
  (cffi:foreign-funcall "cairo_translate"
                        (:pointer (:struct context-t)) cr
                        :double (coerce tx 'double-float)
                        :double (coerce ty 'double-float)
                        :void))

(export 'translate)

;;; ----------------------------------------------------------------------------
;;; cairo_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_scale" %scale) :void
  (cr (:pointer (:struct context-t)))
  (sx :double)
  (sy :double))

(defun scale (cr sx sy)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[sx]{a number coerced to a double float for the scale factor for
    the x dimension}
  @argument[sy]{a number coerced to a double float for the scale factor for
    the y dimension}
  @begin{short}
    Modifies the current transformation matrix (CTM) by scaling the x and y
    user-space axes by @arg{sx} and @arg{sy} respectively.
  @end{short}
  The scaling of the axes takes place after any existing transformation of user
  space.
  @see-symbol{cairo:context-t}"
  (%scale cr (coerce sx 'double-float) (coerce sy 'double-float)))

(export 'scale)

;;; ----------------------------------------------------------------------------
;;; cairo_rotate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rotate" %rotate) :void
  (cr (:pointer (:struct context-t)))
  (angle :double))

(defun rotate (cr angle)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[angle]{a number coerced to a double float for an angle in radians
    by which the user-space axes will be rotated}
  @begin{short}
    Modifies the current transformation matrix (CTM) by rotating the user-space
    axes by @arg{angle} radians.
  @end{short}
  The rotation of the axes takes places after any existing transformation of
  user space. The rotation direction for positive angles is from the positive
  x axis toward the positive y axis.
  @see-symbol{cairo:context-t}"
  (%rotate cr (coerce angle 'double-float)))

(export 'rotate)

;;; ----------------------------------------------------------------------------
;;; cairo_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_transform" transform) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance  for the transformation
    to be applied to the user-space axes}
  @begin{short}
    Modifies the current transformation matrix (CTM) by applying @arg{matrix}
    as an additional transformation.
  @end{short}
  The new transformation of user space takes place after any existing
  transformation.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:matrix-t}"
  (cr (:pointer (:struct context-t)))
  (matrix (:pointer (:struct matrix-t))))

(export 'transform)

;;; ----------------------------------------------------------------------------
;;; cairo_get_matrix
;;; cairo_set_matrix
;;; ----------------------------------------------------------------------------

(defun (setf matrix) (matrix cr)
  (cffi:foreign-funcall "cairo_set_matrix"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(defun matrix (cr matrix)
 #+liber-documentation
 "@version{2025-1-29}
  @syntax{(cairo:matrix cr matrix) => matrix}
  @syntax{(setf (cairo:matrix cr) matrix)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} transformation matrix from user
    space to device space}
  @begin{short}
    The @fun{cairo:matrix} function gets the current transformation matrix
    (CTM).
  @end{short}
  The @setf{cairo:matrix} function modifies the current transformation matrix
  (CTM) by setting it equal to @arg{matrix}.
  @begin[Notes]{dictionary}
    The @symbol{cairo:matrix-t} structure is a foreign CFFI structure,
    therefore we pass in a valid @symbol{cairo:matrix-t} instance to the
    @fun{cairo:matrix} function which is filled with the current transformation
    matrix.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_get_matrix"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(export 'matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_identity_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_identity_matrix" identity-matrix) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Resets the current transformation matrix (CTM) by setting it equal to the
    identity matrix.
  @end{short}
  That is, the user-space and device-space axes will be aligned and one
  user-space unit will transform to one device-space unit.
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct context-t))))

(export 'identity-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_user_to_device
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_user_to_device" %user-to-device) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun user-to-device (cr x y)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{x} -- a double float with the x value of the coordinate @br{}
    @arg{y} -- a double float with the y value of the coordinate
  @end{return}
  @begin{short}
    Transform a coordinate from user space to device space by multiplying the
    given point by the current transformation matrix (CTM).
  @end{short}
  @see-symbol{cairo:context-t}
  @see-function{cairo:device-to-user}"
  (cffi:with-foreign-objects ((xnew :double) (ynew :double))
    (setf (cffi:mem-ref xnew :double)
          (coerce x 'double-float)
          (cffi:mem-ref ynew :double)
          (coerce y 'double-float))
    (%user-to-device cr xnew ynew)
    (values (cffi:mem-ref xnew :double)
            (cffi:mem-ref ynew :double))))

(export 'user-to-device)

;;; ----------------------------------------------------------------------------
;;; cairo_user_to_device_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_user_to_device_distance" %user-to-device-distance) :void
  (cr (:pointer (:struct context-t)))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun user-to-device-distance (cr dx dy)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{dx} -- a double float with the x component of a distance vector @br{}
    @arg{dy} -- a double float with the y component of a distance vector
  @end{return}
  @begin{short}
    Transform a distance vector from user space to device space.
  @end{short}
  This function is similar to the @fun{cairo:user-to-device} function except
  that the translation components of the CTM will be ignored when transforming
  @code{(dx,dy)}.
  @see-symbol{cairo:context-t}
  @see-function{cairo:user-to-device}"
  (cffi:with-foreign-objects ((dxnew :double) (dynew :double))
    (setf (cffi:mem-ref dxnew :double)
          (coerce dx 'double-float)
          (cffi:mem-ref dynew :double)
          (coerce dy 'double-float))
    (%user-to-device-distance cr dxnew dynew)
    (values (cffi:mem-ref dxnew :double)
            (cffi:mem-ref dynew :double))))

(export 'user-to-device-distance)

;;; ----------------------------------------------------------------------------
;;; cairo_device_to_user
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_to_user" %device-to-user) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun device-to-user (cr x y)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{x} -- a double float with the x value of the coordinate @br{}
    @arg{y} -- a double float with the y value of the coordinate
  @end{return}
  @begin{short}
    Transform a coordinate from device space to user space by multiplying the
    given point by the inverse of the current transformation matrix (CTM).
  @end{short}
  @see-symbol{cairo:context-t}
  @see-function{cairo:user-to-device}"
  (cffi:with-foreign-objects ((x-new :double) (y-new :double))
    (setf (cffi:mem-ref x-new :double)
          (coerce x 'double-float)
          (cffi:mem-ref y-new :double)
          (coerce y 'double-float))
    (%device-to-user cr x-new y-new)
    (values (cffi:mem-ref x-new :double)
            (cffi:mem-ref y-new :double))))

(export 'device-to-user)

;;; ----------------------------------------------------------------------------
;;; cairo_device_to_user_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_to_user_distance" %device-to-user-distance) :void
  (cr (:pointer (:struct context-t)))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun device-to-user-distance (cr dx dy)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{dx} -- a double float with the x component of the distance vector @br{}
    @arg{dy} -- a double float with the y component of the distance vector
  @end{return}
  @begin{short}
    Transform a distance vector from device space to user space.
  @end{short}
  This function is similar to the @fun{cairo:device-to-user} function except
  that the translation components of the inverse CTM will be ignored when
  transforming @code{(dx,dy)}.
  @see-symbol{cairo:context-t}
  @see-function{cairo:device-to-user}"
  (cffi:with-foreign-objects ((dxnew :double) (dynew :double))
    (setf (cffi:mem-ref dxnew :double)
          (coerce dx 'double-float)
          (cffi:mem-ref dynew :double)
          (coerce dy 'double-float))
    (%device-to-user-distance cr dxnew dynew)
    (values (cffi:mem-ref dxnew :double)
            (cffi:mem-ref dynew :double))))

(export 'device-to-user-distance)

;;; --- End of file cairo.transformation.lisp ----------------------------------
