;;; ----------------------------------------------------------------------------
;;; cairo.quartz-surface.lisp
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
;;; Quartz Surfaces
;;;
;;;     Rendering to Quartz surfaces
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_QUARTZ_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_quartz_surface_create
;;;     cairo_quartz_surface_create_for_cg_context
;;;     cairo_quartz_surface_get_cg_context
;;;
;;; Description
;;;
;;; The Quartz surface is used to render cairo graphics targeting the Apple OS X
;;; Quartz rendering system.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_QUARTZ_SURFACE
;;;
;;; #define CAIRO_HAS_QUARTZ_SURFACE 1
;;;
;;; Defined if the Quartz surface backend is available. This macro can be used
;;; to conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_quartz_surface_create (cairo_format_t format,
;;;                              unsigned int width,
;;;                              unsigned int height);
;;;
;;; Creates a Quartz surface backed by a CGBitmap. The surface is created using
;;; the Device RGB (or Device Gray, for A8) color space. All Cairo operations,
;;; including those that require software rendering, will succeed on this
;;; surface.
;;;
;;; format :
;;;     format of pixels in the surface to create
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_create_for_cg_context ()
;;;
;;; cairo_surface_t *
;;; cairo_quartz_surface_create_for_cg_context
;;;                                (CGContextRef cgContext,
;;;                                 unsigned int width,
;;;                                 unsigned int height);
;;;
;;; Creates a Quartz surface that wraps the given CGContext. The CGContext is
;;; assumed to be in the standard Cairo coordinate space (that is, with the
;;; origin at the upper left and the Y axis increasing downward). If the
;;; CGContext is in the Quartz coordinate space (with the origin at the bottom
;;; left), then it should be flipped before this function is called. The flip
;;; can be accomplished using a translate and a scale; for example:
;;;
;;; CGContextTranslateCTM (cgContext, 0.0, height);
;;; CGContextScaleCTM (cgContext, 1.0, -1.0);
;;;
;;; All Cairo operations are implemented in terms of Quartz operations, as long
;;; as Quartz-compatible elements are used (such as Quartz fonts).
;;;
;;; cgContext :
;;;     the existing CGContext for which to create the surface
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created Cairo surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_surface_get_cg_context ()
;;;
;;; CGContextRef
;;; cairo_quartz_surface_get_cg_context (cairo_surface_t *surface);
;;;
;;; Returns the CGContextRef that the given Quartz surface is backed by.
;;;
;;; A call to cairo_surface_flush() is required before using the CGContextRef
;;; to ensure that all pending drawing operations are finished and to restore
;;; any temporary modification cairo has made to its state. A call to
;;; cairo_surface_mark_dirty() is required after the state or the content of
;;; the CGContextRef has been modified.
;;;
;;; surface :
;;;     the Cairo Quartz surface
;;;
;;; Returns :
;;;     the CGContextRef for the given surface.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; End of file cairo.quartz-surface.lisp --------------------------------------
