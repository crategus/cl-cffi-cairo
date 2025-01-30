;;; ----------------------------------------------------------------------------
;;; cairo.png-surface.lisp
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
;;; PNG Support
;;;
;;;     Reading and writing PNG images
;;;
;;; Functions
;;;
;;;     cairo_image_surface_create_from_png
;;;     cairo_read_func_t                                   not implemented
;;;     cairo_image_surface_create_from_png_stream          not implemented
;;;     cairo_surface_write_to_png
;;;     cairo_write_func_t                                  not implemented
;;;     cairo_surface_write_to_png_stream                   not implemented
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png
;;; ----------------------------------------------------------------------------

(defun image-surface-create-from-png (path)
 #+liber-documentation
 "@version{2025-1-13}
  @argument[path]{a namestring or pathname for the path of the PNG file to load}
  @begin{return}
    The new @symbol{cairo:surface-t} instance initialized with the contents of
    the PNG file, or a \"nil\" surface if any error occurred.
  @end{return}
  @begin{short}
    Creates a new image surface and initializes the contents to the given PNG
    file.
  @end{short}
  A \"nil\" surface can be checked for with the @fun{cairo:surface-status}
  function which may return one of the following values: @code{:no-memory},
  @code{:file-not-found}, or @code{:read-error}. Alternatively, you can allow
  errors to propagate through the drawing operations and check the status on
  the context upon completion using the @fun{cairo:status} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:status}
  @see-function{cairo:surface-status}"
  (cffi:foreign-funcall "cairo_image_surface_create_from_png"
                        :string (namestring path)
                        (:pointer (:struct surface-t))))

(export 'image-surface-create-from-png)

;;; ----------------------------------------------------------------------------
;;; cairo_read_func_t
;;;
;;; cairo_read_func_t is the type of function which is called when a backend
;;; needs to read data from an input stream.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png_stream
;;;
;;; Creates a new image surface from PNG data read incrementally via the
;;; read_func function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png
;;; ----------------------------------------------------------------------------

(defun surface-write-to-png (surface path)
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance with pixel contents}
  @argument[path]{a namestring or pathname for the path of a file to write to}
  @begin{return}
    @code{:success} if the PNG file was written successfully. Otherwise,
    @code{:no-memory} if memory could not be allocated for the operation or
    @code{:surface-type-mismatch} if the surface does not have pixel contents,
    or @code{:write-error} if an I/O error occurs while attempting to write
    the file.
  @end{return}
  @begin{short}
    Writes the contents of the image surface to a new file as a PNG image.
  @end{short}
  @see-symbol{cairo:surface-t}"
  (cffi:foreign-funcall "cairo_surface_write_to_png"
                        (:pointer (:struct surface-t)) surface
                        :string (namestring path)
                        status-t))

(export 'surface-write-to-png)

;;; ----------------------------------------------------------------------------
;;; cairo_write_func_t
;;;
;;; cairo_write_func_t is the type of function which is called when a backend
;;; needs to write data to an output stream.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png_stream
;;;
;;; Writes the image surface to the write function.
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.png-surface.lisp -------------------------------------
