;;; ----------------------------------------------------------------------------
;;; cairo.surface-observer.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;; Surface Observer
;;;
;;;     Observing other surfaces
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_OBSERVER_SURFACE
;;;     cairo_surface_observer_mode_t
;;;
;;; Functions
;;;
;;;     cairo_surface_create_observer
;;;     cairo_surface_observer_add_fill_callback
;;;     cairo_surface_observer_add_finish_callback
;;;     cairo_surface_observer_add_flush_callback
;;;     cairo_surface_observer_add_glyphs_callback
;;;     cairo_surface_observer_add_mask_callback
;;;     cairo_surface_observer_add_paint_callback
;;;     cairo_surface_observer_add_stroke_callback
;;;     cairo_surface_observer_callback_t
;;;     cairo_surface_observer_elapsed
;;;     cairo_surface_observer_print
;;;
;;; Description
;;;
;;;     A surface that exists solely to watch what another surface is doing.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_OBSERVER_SURFACE
;;;
;;; #define CAIRO_HAS_OBSERVER_SURFACE
;;;
;;; Defined if the observer surface backend is available. This macro can be
;;; used to conditionally compile backend-specific code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_surface_observer_mode_t
;;;
;;; Whether operations should be recorded.
;;;
;;; CAIRO_SURFACE_OBSERVER_NORMAL
;;;     no recording is done
;;;
;;; CAIRO_SURFACE_OBSERVER_RECORD_OPERATIONS
;;;     operations are recorded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_observer ()
;;;
;;; cairo_surface_t *
;;; cairo_surface_create_observer (cairo_surface_t *target,
;;;                                cairo_surface_observer_mode_t mode);
;;;
;;; Create a new surface that exists solely to watch another is doing. In the
;;; process it will log operations and times, which are fast, which are slow,
;;; which are frequent, etc.
;;;
;;; The mode parameter can be set to either CAIRO_SURFACE_OBSERVER_NORMAL or
;;; CAIRO_SURFACE_OBSERVER_RECORD_OPERATIONS, to control whether or not the
;;; internal observer should record operations.
;;;
;;; target
;;;     an existing surface for which the observer will watch
;;;
;;; mode
;;;     sets the mode of operation (normal vs. record)
;;;
;;; Returns
;;;     a pointer to the newly allocated surface. The caller owns the surface
;;;     and should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if other is already in an error state or any
;;;     other error occurs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_fill_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_fill_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for fill operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for fill operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_finish_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_finish_callback
;;;                               (cairo_surface_t *abstract_surface,
;;;                                cairo_surface_observer_callback_t func,
;;;                                void *data);
;;;
;;; Adds a callback for finish operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for the finish operation
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_flush_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_flush_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for flush operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback for flush operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_glyphs_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_glyphs_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for glyph operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for glyph operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_mask_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_mask_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for mask operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for mask operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_paint_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_paint_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for paint operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for paint operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_add_stroke_callback ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_add_stroke_callback
;;;                                (cairo_surface_t *abstract_surface,
;;;                                 cairo_surface_observer_callback_t func,
;;;                                 void *data);
;;;
;;; Adds a callback for stroke operations on the observed surface.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; func
;;;     callback function for stroke operations
;;;
;;; data
;;;     closure to pass to the callback
;;;
;;; Returns
;;;     the status of the surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_callback_t ()
;;;
;;; void
;;; (*cairo_surface_observer_callback_t) (cairo_surface_t *observer,
;;;                                       cairo_surface_t *target,
;;;                                       void *data);
;;;
;;; A generic callback function for surface operations.
;;;
;;; observer
;;;     the cairo_surface_observer_t
;;;
;;; target
;;;     the observed surface
;;;
;;; data
;;;     closure used when adding the callback
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_elapsed ()
;;;
;;; double
;;; cairo_surface_observer_elapsed (cairo_surface_t *abstract_surface);
;;;
;;; Returns the total observation time.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; Returns
;;;     the elapsed time, in nanoseconds
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_observer_print ()
;;;
;;; cairo_status_t
;;; cairo_surface_observer_print (cairo_surface_t *abstract_surface,
;;;                               cairo_write_func_t write_func,
;;;                               void *closure);
;;;
;;; Prints the observer log using the given callback.
;;;
;;; abstract_surface
;;;     a cairo_surface_observer_t
;;;
;;; write_func
;;;     callback for writing on a stream
;;;
;;; closure
;;;     data to pass to write_func
;;;
;;; Returns
;;;     the status of the print operation
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.surface-observer.lisp --------------------------------
