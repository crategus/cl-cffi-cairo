;;; ----------------------------------------------------------------------------
;;; cairo.png-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; PNG Support
;;;
;;;     Reading and writing PNG images
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_PNG_FUNCTIONS
;;;
;;; Functions
;;;
;;;     cairo_image_surface_create_from_png
;;;     cairo_read_func_t
;;;     cairo_image_surface_create_from_png_stream
;;;     cairo_surface_write_to_png
;;;     cairo_write_func_t
;;;     cairo_surface_write_to_png_stream
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_PNG_FUNCTIONS
;;;
;;; #define CAIRO_HAS_PNG_FUNCTIONS 1
;;;
;;; Defined if the PNG functions are available. This macro can be used to
;;; conditionally compile code using the cairo PNG functions.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png ()
;;; ----------------------------------------------------------------------------

(defun image-surface-create-from-png (path)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[path]{a namestring or pathname with the path of the PNG file to
    load}
  @begin{return}
    A new @symbol{cairo:surface-t} instance initialized with the contents of
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
;;; cairo_read_func_t ()
;;;
;;; cairo_status_t (*cairo_read_func_t) (void *closure,
;;;                                      unsigned char *data,
;;;                                      unsigned int length);
;;;
;;; cairo_read_func_t is the type of function which is called when a backend
;;; needs to read data from an input stream. It is passed the closure which was
;;; specified by the user at the time the read function was registered, the
;;; buffer to read the data into and the length of the data in bytes. The read
;;; function should return CAIRO_STATUS_SUCCESS if all the data was successfully
;;; read, CAIRO_STATUS_READ_ERROR otherwise.
;;;
;;; closure :
;;;     the input closure
;;;
;;; data :
;;;     the buffer into which to read the data
;;;
;;; length :
;;;     the amount of data to read
;;;
;;; Returns :
;;;     the status code of the read operation
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png_stream ()
;;;
;;; cairo_surface_t * cairo_image_surface_create_from_png_stream
;;;                                                (cairo_read_func_t read_func,
;;;                                                 void *closure);
;;;
;;; Creates a new image surface from PNG data read incrementally via the
;;; read_func function.
;;;
;;; read_func :
;;;     function called to read the data of the file
;;;
;;; closure :
;;;     data to pass to read_func.
;;;
;;; Returns :
;;;     a new cairo_surface_t initialized with the contents of the PNG file or a
;;;     "nil" surface if the data read is not a valid PNG image or memory could
;;;     not be allocated for the operation. A "nil" surface can be checked for
;;;     with cairo_surface_status(surface) which may return one of the following
;;;     values: CAIRO_STATUS_NO_MEMORY CAIRO_STATUS_READ_ERROR Alternatively,
;;;     you can allow errors to propagate through the drawing operations and
;;;     check the status on the context upon completion using cairo_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png ()
;;; ----------------------------------------------------------------------------

(defun surface-write-to-png (surface path)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[surface]{a @symbol{cairo:surface-t} instance with pixel contents}
  @argument[path]{a namestring or pathname with the path of a file to write to}
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
;;; cairo_write_func_t ()
;;;
;;; cairo_status_t (*cairo_write_func_t) (void *closure,
;;;                                       const unsigned char *data,
;;;                                       unsigned int length);
;;;
;;; cairo_write_func_t is the type of function which is called when a backend
;;; needs to write data to an output stream. It is passed the closure which was
;;; specified by the user at the time the write function was registered, the
;;; data to write and the length of the data in bytes. The write function should
;;; return CAIRO_STATUS_SUCCESS if all the data was successfully written,
;;; CAIRO_STATUS_WRITE_ERROR otherwise.
;;;
;;; closure :
;;;     the output closure
;;;
;;; data :
;;;     the buffer containing the data to write
;;;
;;; length :
;;;     the amount of data to write
;;;
;;; Returns :
;;;     the status code of the write operation
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png_stream ()
;;;
;;; cairo_status_t cairo_surface_write_to_png_stream
;;;                                              (cairo_surface_t *surface,
;;;                                               cairo_write_func_t write_func,
;;;                                               void *closure);
;;;
;;; Writes the image surface to the write function.
;;;
;;; surface :
;;;     a cairo_surface_t with pixel contents
;;;
;;; write_func :
;;;     a cairo_write_func_t
;;;
;;; closure :
;;;     closure data for the write function
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS if the PNG file was written successfully.
;;;     Otherwise, CAIRO_STATUS_NO_MEMORY is returned if memory could not be
;;;     allocated for the operation, CAIRO_STATUS_SURFACE_TYPE_MISMATCH if the
;;;     surface does not have pixel contents.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.png-surface.lisp -------------------------------------
