;;; ----------------------------------------------------------------------------
;;; cairo.font-face.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2014 - 2025 Dieter Kaiser
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
;;; cairo_font_face_t
;;;
;;;     Base class for font faces
;;;
;;; Types and Values
;;;
;;;     cairo_font_face_t
;;;     cairo_font_type_t
;;;
;;; Functions
;;;
;;;     cairo_font_face_reference
;;;     cairo_font_face_destroy
;;;     cairo_font_face_status
;;;
;;;     cairo_font_face_get_type
;;;     cairo_font_face_get_reference_count
;;;     cairo_font_face_set_user_data                      not implemented
;;;     cairo_font_face_get_user_data                      not implemented
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_font_type_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum (font-type-t :int :allow-undeclared-values t)
  :toy
  :ft
  :win32
  :quartz
  :user
  #+cairo-1-18
  :dwrite)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-type-t)
      "CEnum"
      (liber:symbol-documentation 'font-type-t)
 "@version{2025-09-02}
  @begin{declaration}
(cffi:defcenum font-type-t
  :toy
  :ft
  :win32
  :quartz
  :user
  :dwrite)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:toy]{The font was created using Cairo's toy font API.}
      @entry[:ft]{The font is of type FreeType.}
      @entry[:win32]{The font is of type Win32.}
      @entry[:quartz]{The font is of type Quartz.}
      @entry[:user]{The font was created using Cairo's user font API.}
      @entry[:dwrite]{The font is of type Win32 DWrite. Since 1.18}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:font-type-t} enumeration is used to describe the type of a
    given font face or scaled font.
  @end{short}
  The font types are also known as \"font backends\" within Cairo.

  The type of a font face is determined by the function used to create it,
  which will generally be of the form @code{type-font-face-create}. The font
  face type can be queried with the @fun{cairo:font-face-type} function.

  The various @code{cairo:font-face-t} functions can be used with a font face
  of any type.

  The type of a scaled font is determined by the type of the font face passed
  to the @fun{cairo:scaled-font-create} function. The scaled font type can be
  queried with the @fun{cairo:scaled-font-type} function.

  The various @code{cairo:scaled-font-t} functions can be used with scaled
  fonts of any type, but some font backends also provide type-specific functions
  that must only be called with a scaled font of the appropriate type. These
  functions have names that begin with @code{type-scaled-font} such as the
  @fun{cairo:ft-scaled-font-lock-face} function.

  The behavior of calling a type-specific function with a scaled font of the
  wrong type is undefined.
  @see-symbol{cairo:font-face-t}
  @see-function{cairo:font-face-type}
  @see-function{cairo:scaled-font-create}
  @see-function{cairo:scaled-font-type}")

(export 'font-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct font-face-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-face-t)
      "CStruct"
      (liber:symbol-documentation 'font-face-t)
 "@version{2025-09-02}
  @begin{short}
    The @sym{cairo:font-face-t} structure specifies all aspects of a font other
    than the size or font matrix.
  @end{short}
  A font matrix is used to distort a font by sheering it or scaling it unequally
  in the two directions. A font face can be set on a Cairo context by using the
  @fun{cairo:font-face} function. The size and font matrix are set with the
  @fun{cairo:set-font-size} and @fun{cairo:font-matrix} functions.

  There are various types of font faces, depending on the font backend they
  use. The type of a font face can be queried using the
  @fun{cairo:font-face-type} function.

  Memory management of the @sym{cairo:font-face-t} structure is done with the
  @fun{cairo:font-face-reference} and @fun{cairo:font-face-destroy} functions.
  @see-symbol{cairo:context-t}
  @see-fun{cairo:font-face}
  @see-function{cairo:set-font-size}
  @see-function{cairo:font-matrix}
  @see-function{cairo:font-face-type}")

(export 'font-face-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_reference
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_face_reference" %font-face-reference)
    (:pointer (:struct font-face-t))
  (face (:pointer (:struct font-face-t))))

(defun font-face-reference (face)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[face]{a @sym{cairo:font-face-t} instance, may be @code{nil} in which
    case this function does nothing}
  @return{The referenced @sym{cairo:font-face-t} instance.}
  @begin{short}
    Increases the reference count on @arg{face} by one.
  @end{short}
  This prevents the font face from being destroyed until a matching call to the
  @fun{cairo:font-face-destroy} function is made.

  The number of references to a @sym{cairo:font-face-t} instance can be get
  using the @fun{cairo:font-face-reference-count} function.
  @see-symbol{cairo:font-face-t}
  @see-function{cairo:font-face-destroy}
  @see-function{cairo:font-face-reference-count}"
  (when (and face (not (cffi:null-pointer-p face)))
    (%font-face-reference face)))

(export 'font-face-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_reference_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_face_get_reference_count" font-face-reference-count)
    :uint
 #+liber-documentation
 "@version{2025-09-02}
  @argument[face]{a @sym{cairo:font-face-t} instance}
  @begin{return}
    The unsigned integer for the current reference count of @arg{face}. If the
    object is a \"nil\" object, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of the font face.
  @end{short}
  @see-symbol{cairo:font-face-t}"
  (face (:pointer (:struct font-face-t))))

(export 'font-face-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_face_destroy" font-face-destroy) :void
  #+liber-documentation
  "@version{2025-09-02}
  @argument[face]{a @sy{cairo:font-face-t} instance}
  @begin{short}
    Decreases the reference count on @arg{face} by one.
  @end{short}
  If the result is zero, then @arg{face} and all associated resources are freed.
  See the @fun{cairo:font-face-reference} function.
  @see-symbol{cairo:font-face-t}
  @see-function{cairo:font-face-reference}"
  (face (:pointer (:struct font-face-t))))

(export 'font-face-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_face_status" font-face-status) status-t
 #+liber-documentation
 "@version{2025-09-20}
  @argument[face]{a @sym{cairo:font-face-t} instance}
  @begin{return}
    The @val[cairo:status-t]{:success} value of the @sym{cairo:status-t}
    enumeration or another error such as the @val[cairo:status-t]{:no-memory}
    value.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this font face.
  @end{short}
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:status-t}"
  (face (:pointer (:struct font-face-t))))

(export 'font-face-status)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_face_get_type" font-face-type) font-type-t
 #+liber-documentation
 "@version{2025-09-02}
  @argument[face]{a @sym{cairo:font-face-t} font face}
  @return{The @sym{cairo:font-type-t} value for the type of @arg{face}.}
  @begin{short}
    This function returns the type of the backend used to create a font face.
  @end{short}
  See the @sym{cairo:font-type-t} enumeration for available types.
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:font-type-t}"
  (face (:pointer (:struct font-face-t))))

(export 'font-face-type)

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_set_user_data
;;;
;;; Attach user data to font_face.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_face_get_user_data
;;;
;;; Return user data previously attached to font_face using the specified key.
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.font-face.lisp ---------------------------------------
