;;; ----------------------------------------------------------------------------
;;; cairo.tee-surface.lisp
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
;;; Tee surface
;;;
;;;     Redirect input to multiple surfaces
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_TEE_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_tee_surface_create
;;;     cairo_tee_surface_add
;;;     cairo_tee_surface_index
;;;     cairo_tee_surface_remove
;;;
;;; Description
;;;
;;;     The "tee" surface supports redirecting all its input to multiple
;;;     surfaces.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_TEE_SURFACE
;;;
;;; #define CAIRO_HAS_TEE_SURFACE
;;;
;;; Defined if the tee surface backend is available.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_tee_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_tee_surface_create (cairo_surface_t *primary);
;;;
;;; Creates a new "tee" surface.
;;;
;;; The primary surface is used when querying surface options, like font options
;;; and extents.
;;;
;;; Operations performed on the tee surface will be replayed on any surface
;;; added to it.
;;;
;;; primary
;;;     the primary cairo_surface_t
;;;
;;; Returns
;;;     the newly created surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_tee_surface_add ()
;;;
;;; void
;;; cairo_tee_surface_add (cairo_surface_t *abstract_surface,
;;;                        cairo_surface_t *target);
;;;
;;; Adds a new target surface to the list of replicas of a tee surface.
;;;
;;; abstract_surface
;;;     a cairo_tee_surface_t
;;;
;;; target
;;;     the surface to add
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_tee_surface_index ()
;;;
;;; cairo_surface_t *
;;; cairo_tee_surface_index (cairo_surface_t *abstract_surface,
;;;                          unsigned int index);
;;;
;;; Retrieves the replica surface at the given index.
;;;
;;; The primary surface used to create the cairo_tee_surface_t is always set at
;;; the zero index.
;;;
;;; abstract_surface
;;;     a cairo_tee_surface_t
;;;
;;; index
;;;     the index of the replica to retrieve
;;;
;;; Returns
;;;     the surface at the given index
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_tee_surface_remove ()
;;;
;;; void
;;; cairo_tee_surface_remove (cairo_surface_t *abstract_surface,
;;;                           cairo_surface_t *target);
;;;
;;; Removes the given surface from the list of replicas of a tee surface.
;;;
;;; abstract_surface
;;;     a cairo_tee_surface_t
;;;
;;; target
;;;     the surface to remove
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.tee-surface.lisp -------------------------------------
