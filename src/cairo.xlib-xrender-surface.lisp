;;; ----------------------------------------------------------------------------
;;; cairo.xlib-xrender-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2024 Dieter Kaiser
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
;;; XLib-XRender Backend
;;;
;;;     X Window System rendering using XLib and the X Render extension
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_XLIB_XRENDER_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_xlib_surface_create_with_xrender_format
;;;     cairo_xlib_surface_get_xrender_format
;;;
;;; Description
;;;
;;;     The XLib surface is used to render cairo graphics to X Window System
;;;     windows and pixmaps using the XLib and Xrender libraries.
;;;
;;;     Note that the XLib surface automatically takes advantage of X Render
;;;     extension if it is available.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_XLIB_XRENDER_SURFACE
;;;
;;; #define CAIRO_HAS_XLIB_XRENDER_SURFACE 1
;;;
;;; Defined if the XLib/XRender surface functions are available. This macro can
;;; be used to conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_create_with_xrender_format ()
;;;
;;; cairo_surface_t *
;;; cairo_xlib_surface_create_with_xrender_format
;;;                                (Display *dpy,
;;;                                 Drawable drawable,
;;;                                 Screen *screen,
;;;                                 XRenderPictFormat *format,
;;;                                 int width,
;;;                                 int height);
;;;
;;; Creates an Xlib surface that draws to the given drawable. The way that
;;; colors are represented in the drawable is specified by the provided picture
;;; format.
;;;
;;; Note: If drawable is a Window, then the function
;;; cairo_xlib_surface_set_size() must be called whenever the size of the
;;; window changes.
;;;
;;; dpy :
;;;     an X Display
;;;
;;; drawable :
;;;     an X Drawable, (a Pixmap or a Window)
;;;
;;; screen :
;;;     the X Screen associated with drawable
;;;
;;; format :
;;;     the picture format to use for drawing to drawable . The depth of format
;;;     must match the depth of the drawable.
;;;
;;; width :
;;;     the current width of drawable .
;;;
;;; height :
;;;     the current height of drawable .
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_xrender_format ()
;;;
;;; XRenderPictFormat *
;;; cairo_xlib_surface_get_xrender_format (cairo_surface_t *surface);
;;;
;;; Gets the X Render picture format that surface uses for rendering with the X
;;; Render extension. If the surface was created by
;;; cairo_xlib_surface_create_with_xrender_format() originally, the return
;;; value is the format passed to that constructor.
;;;
;;; surface :
;;;     an xlib surface
;;;
;;; Returns :
;;;     the XRenderPictFormat* associated with surface , or NULL if the surface
;;;     is not an xlib surface or if the X Render extension is not available.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.xlib-xrender-surface.lisp ----------------------------
