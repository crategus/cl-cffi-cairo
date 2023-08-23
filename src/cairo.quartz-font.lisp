;;; ----------------------------------------------------------------------------
;;; cairo.quartz-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
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
;;; Quartz (CGFont) Fonts
;;;
;;;     Font support via CGFont on OS X
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_QUARTZ_FONT
;;;
;;; Functions
;;;
;;;     cairo_quartz_font_face_create_for_cgfont
;;;     cairo_quartz_font_face_create_for_atsu_font_id
;;;
;;; Description
;;;
;;; The Quartz font backend is primarily used to render text on Apple MacOS X
;;; systems. The CGFont API is used for the internal implementation of the font
;;; backend methods.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_QUARTZ_FONT
;;;
;;; #define CAIRO_HAS_QUARTZ_FONT 1
;;;
;;; Defined if the Quartz font backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_font_face_create_for_cgfont ()
;;;
;;; cairo_font_face_t *
;;; cairo_quartz_font_face_create_for_cgfont (CGFontRef font);
;;;
;;; Creates a new font for the Quartz font backend based on a CGFontRef. This
;;; font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create().
;;;
;;; font :
;;;     a CGFontRef obtained through a method external to cairo.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_font_face_create_for_atsu_font_id ()
;;;
;;; cairo_font_face_t *
;;; cairo_quartz_font_face_create_for_atsu_font_id (ATSUFontID font_id);
;;;
;;; Creates a new font for the Quartz font backend based on an ATSUFontID. This
;;; font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create().
;;;
;;; font_id :
;;;     an ATSUFontID for the font.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.quartz-font.lisp -------------------------------------
