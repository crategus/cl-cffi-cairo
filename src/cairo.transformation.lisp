;;; ----------------------------------------------------------------------------
;;; cairo.transformation.lisp
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
;;;
;;; Description
;;;
;;; The current transformation matrix, ctm, is a two-dimensional affine
;;; transformation that maps all coordinates and other drawing instruments from
;;; the user space into the surface's canonical coordinate system, also known
;;; as the device space.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_translate () -> translate
;;; ----------------------------------------------------------------------------

(defun translate (cr tx ty)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[tx]{a double float with the amount to translate in the x direction}
  @argument[ty]{a double float with the amount to translate in the y direction}
  @begin{short}
    Modifies the current transformation matrix (CTM) by translating the
    user-space origin by (@arg{tx}, @arg{ty}).
  @end{short}
  This offset is interpreted as a user-space coordinate according to the CTM in
  place before the new call to the @sym{cairo:translate} function. In other
  words, the translation of the user-space origin takes place after any existing
  transformation.
  @see-symbol{cairo:context-t}"
  (cffi:foreign-funcall "cairo_translate"
                        (:pointer (:struct context-t)) cr
                        :double (coerce tx 'double-float)
                        :double (coerce ty 'double-float)
                        :void)
  cr)

(export 'translate)

;;; ----------------------------------------------------------------------------
;;; cairo_scale () -> scale
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scale" %scale) :void
  (cr (:pointer (:struct context-t)))
  (sx :double)
  (sy :double))

(defun scale (cr sx sy)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[sx]{a double float with the scale factor for the x dimension}
  @argument[sy]{a double float with the scale factor for the y dimension}
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
;;; cairo_rotate () -> rotate
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_rotate" %rotate) :void
  (cr (:pointer (:struct context-t)))
  (angle :double))

(defun rotate (cr angle)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[angle]{a double float with an angle in radians by which the
    user-space axes will be rotated}
  @begin{short}
    Modifies the current transformation matrix (CTM) by rotating the user-space
    axes by @arg{angle} radians.
  @end{short}
  The rotation of the axes takes places after any existing transformation of
  user space. The rotation direction for positive angles is from the positive x
  axis toward the positive y axis.
  @see-symbol{cairo:context-t}"
  (%rotate cr (coerce angle 'double-float)))

(export 'rotate)

;;; ----------------------------------------------------------------------------
;;; cairo_transform () -> transform
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_transform" transform) :void
 #+liber-documentation
 "@version{#2020-12-15}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[matrix]{a @symbol{cairo:matrix-t} transformation to be applied to
    the user-space axes}
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
;;; cairo_get_matrix ()
;;; cairo_set_matrix () -> matrix
;;; ----------------------------------------------------------------------------

(defun (setf matrix) (matrix cr)
  (cffi:foreign-funcall "cairo_set_matrix"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(defun matrix (cr matrix)
 #+liber-documentation
 "@version{#2020-12-15}
  @syntax[]{(ctm-matrix cr matrix) => matrix}
  @syntax[]{(setf (ctm-matrix cr) matrix)}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[matrix]{a @symbol{cairo:matrix-t} transformation matrix from user
    space to device space}
  @begin{short}
    The @sym{cairo:matrix} function gets the current transformation matrix
    (CTM).
  @end{short}
  The @sym{(setf cairo:matrix)} function modifies the current transformation
  matrix (CTM) by setting it equal to @arg{matrix}.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:matrix-t}"
  (cffi:foreign-funcall "cairo_get_matrix"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(export 'matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_identity_matrix () -> identity-matrix
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_identity_matrix" identity-matrix) :void
 #+liber-documentation
 "@version{#2020-12-15}
  @argument[cr]{a @symbol{cairo:context-t} context}
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
;;; cairo_user_to_device () -> user-to-device
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_user_to_device" %user-to-device) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun user-to-device (cr x y)
 #+liber-documentation
 "@version{#2020-12-26}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @code{x} -- a double float x value of coordinate (in/out parameter) @br{}
    @code{y} -- a double float y value of coordinate (in/out parameter)
  @end{return}
  @begin{short}
    Transform a coordinate from user space to device space by multiplying the
    given point by the current transformation matrix (CTM).
  @end{short}
  @see-symbol{cairo:context-t}
  @see-function{cairo:device-to-user}"
  (with-foreign-objects ((x-new :double) (y-new :double))
    (setf (cffi:mem-ref x-new :double)
          (coerce x 'double-float)
          (cffi:mem-ref y-new :double)
          (coerce y 'double-float))
    (%user-to-device cr x-new y-new)
    (values (cffi:mem-ref x-new :double)
            (cffi:mem-ref y-new :double))))

(export 'user-to-device)

;;; ----------------------------------------------------------------------------
;;; cairo_user_to_device_distance () -> user-to-device-distance
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_user_to_device_distance" %user-to-device-distance) :void
  (cr (:pointer (:struct context-t)))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun user-to-device-distance (cr dx dy)
 #+liber-documentation
 "@version{#2020-12-26}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @code{dx} -- a double float x component of a distance vector
      (in/out parameter) @br{}
    @code{dy} -- a double float y component of a distance vector
      (in/out parameter)
  @end{return}
  @begin{short}
    Transform a distance vector from user space to device space.
  @end{short}
  This function is similar to the @fun{cairo:user-to-device} function except
  that the translation components of the CTM will be ignored when transforming
  (@code{dx}, @code{dy}).
  @see-symbol{cairo:context-t}
  @see-function{cairo:user-to-device}"
  (with-foreign-objects ((dx-new :double) (dy-new :double))
    (setf (cffi:mem-ref dx-new :double)
          (coerce dx 'double-float)
          (cffi:mem-ref dy-new :double)
          (coerce dy 'double-float))
    (%user-to-device-distance cr dx-new dy-new)
    (values (cffi:mem-ref dx-new :double)
            (cffi:mem-ref dy-new :double))))

(export 'user-to-device-distance)

;;; ----------------------------------------------------------------------------
;;; cairo_device_to_user () -> device-to-user
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_to_user" %device-to-user) :void
  (cr (:pointer (:struct context-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun device-to-user (cr x y)
 #+liber-documentation
 "@version{#2020-12-26}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @code{x} -- a double float x value of coordinate (in/out parameter) @br{}
    @code{y} -- a double float y value of coordinate (in/out parameter)
  @end{return}
  @begin{short}
    Transform a coordinate from device space to user space by multiplying the
    given point by the inverse of the current transformation matrix (CTM).
  @end{short}
  @see-symbol{cairo:context-t}
  @see-function{cairo:user-to-device}"
  (with-foreign-objects ((x-new :double) (y-new :double))
    (setf (cffi:mem-ref x-new :double)
          (coerce x 'double-float)
          (cffi:mem-ref y-new :double)
          (coerce y 'double-float))
    (%device-to-user cr x-new y-new)
    (values (cffi:mem-ref x-new :double)
            (cffi:mem-ref y-new :double))))

(export 'device-to-user)

;;; ----------------------------------------------------------------------------
;;; cairo_device_to_user_distance () -> device-to-user-distance
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_to_user_distance" %device-to-user-distance) :void
  (cr (:pointer (:struct context-t)))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun device-to-user-distance (cr dx dy)
 #+liber-documentation
 "@version{#2020-12-26}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    @code{dx} -- a double float x component of a distance vector
      (in/out parameter) @br{}
    @code{dy} -- a double float y component of a distance vector
      (in/out parameter)
  @end{return}
  @begin{short}
    Transform a distance vector from device space to user space.
  @end{short}
  This function is similar to the @fun{cairo:device-to-user} function except
  that the translation components of the inverse CTM will be ignored when
  transforming (@code{dx},@code{dy}).
  @see-symbol{cairo:context-t}
  @see-function{cairo:device-to-user}"
  (with-foreign-objects ((dx-new :double) (dy-new :double))
    (setf (cffi:mem-ref dx-new :double)
          (coerce dx 'double-float)
          (cffi:mem-ref dy-new :double)
          (coerce dy 'double-float))
    (%device-to-user-distance cr dx-new dy-new)
    (values (cffi:mem-ref dx-new :double)
            (cffi:mem-ref dy-new :double))))

(export 'device-to-user-distance)

;;; --- End of file cairo.transformation.lisp ----------------------------------
