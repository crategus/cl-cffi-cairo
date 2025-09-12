;;; ----------------------------------------------------------------------------
;;; cairo.status.lisp
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
;;; Error handling
;;;
;;;     Decoding cairo's status
;;;
;;; Types and Values
;;;
;;;     cairo_status_t
;;;
;;; Functions
;;;
;;;     cairo_status_to_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_status_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum status-t
  :success
  :no-memory
  :invalid-restore
  :invalid-pop-group
  :no-current-point
  :invalid-matrix
  :invalid-status
  :null-pointer
  :invalid-string
  :invalid-path-data
  :read-error
  :write-error
  :surface-finished
  :surface-type-mismatch
  :pattern-type-mismatch
  :invalid-content
  :invalid-format
  :invalid-visual
  :file-not-found
  :invalid-dash
  :invalid-dsc-comment
  :invalid-index
  :clip-not-representable
  :temp-file-error
  :invalid-stride
  :font-type-mismatch
  :user-font-immutable
  :user-font-error
  :negative-count
  :invalid-clusters
  :invalid-slant
  :invalid-weight
  :invalid-size
  :user-font-not-implemented
  :device-type-mismatch
  :device-error
  :invalid-mesh-construction
  :device-finished
  :jbig2-global-missing
  :png-error
  :freetype-error
  :win32-gdk-error
  :tag-error
  #+cairo-1-18
  :dwrite-error
  #+cairo-1-18
  :svg-font-error
  :last-status)

#+liber-documentation
(setf (liber:alias-for-symbol 'status-t)
      "CEnum"
      (liber:symbol-documentation 'status-t)
 "@version{2025-09-02}
  @begin{declaration}
(cffi:defcenum status-t
  :success
  :no-memory
  :invalid-restore
  :invalid-pop-group
  :no-current-point
  :invalid-matrix
  :invalid-status
  :null-pointer
  :invalid-string
  :invalid-path-data
  :read-error
  :write-error
  :surface-finished
  :surface-type-mismatch
  :pattern-type-mismatch
  :invalid-content
  :invalid-format
  :invalid-visual
  :file-not-found
  :invalid-dash
  :invalid-dsc-comment
  :invalid-index
  :clip-not-representable
  :temp-file-error
  :invalid-stride
  :font-type-mismatch
  :user-font-immutable
  :user-font-error
  :negative-count
  :invalid-clusters
  :invalid-slant
  :invalid-weight
  :invalid-size
  :user-font-not-implemented
  :device-type-mismatch
  :device-error
  :invalid-mesh-construction
  :device-finished
  :jbig2-global-missing
  :png-error
  :freetype-error
  :win32-gdk-error
  :tag-error
  #+cairo-1-18
  :dwrite-error
  #+cairo-1-18
  :svg-font-error
  :last-status)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:success]{No error has occurred.}
      @entry[:no-memory]{Out of memory.}
      @entry[:invalid-restore]{The @fun{cairo:restore} function called without
        matching the @fun{cairo:save} function.}
      @entry[:invalid-pop-group]{No saved group to pop, that is, the
      @fun{cairo:pop-group} function without matching the @fun{cairo:push-group}
        function.}
      @entry[:no-current-point]{No current point defined.}
      @entry[:invalid-matrix]{Invalid matrix (not invertible).}
      @entry[:invalid-status]{Invalid @sym{cairo:status-t} value for an input.}
      @entry[:null-pointer]{@code{NULL} pointer.}
      @entry[:invalid-string]{Input string not valid UTF-8.}
      @entry[:path-data]{Input path data not valid.}
      @entry[:read-error]{Error while reading from input stream.}
      @entry[:write-error]{Error while writing to output stream.}
      @entry[:surface-finished]{Target surface has been finished.}
      @entry[:surface-type-mismatch]{The surface type is not appropriate for
        the operation.}
      @entry[:pattern-type-mismatch]{The pattern type is not appropriate for
        the operation.}
      @entry[:invalid-content]{Invalid @sym{cairo:content-t} value for an
        input.}
      @entry[:invalid-format]{Invalid @sym{cairo:format-t} value for an input.}
      @entry[:invalid-visual]{Invalid value for an input Visual.}
      @entry[:file-not-found]{File not found.}
      @entry[:invalid-dash]{Invalid value for a dash setting.}
      @entry[:invalid-dsc-comment]{Invalid value for a DSC comment.}
      @entry[:invalid-index]{Invalid index passed to getter.}
      @entry[:clip-not-representable]{Clip region not representable in desired
        format.}
      @entry[:temp-file-error]{Error creating or writing to a temporary file.}
      @entry[:invalid-stride]{Invalid value for stride.}
      @entry[:font-type-mismatch]{The font type is not appropriate for the
        operation.}
      @entry[:user-font-immutable]{The user-font is immutable.}
      @entry[:user-font-error]{Error occurred in a user-font callback function.}
      @entry[:negative-count]{Negative number used where it is not allowed.}
      @entry[:invalid-clusters]{Input clusters do not represent the accompanying
        text and glyph array.}
      @entry[:invalid-slant]{Invalid @sym{cairo:font-slant-t} value for an
        input.}
      @entry[:invalid-weight]{Invalid @sym{cairo:font-weight-t} value for an
        input.}
      @entry[:invalid-size]{Invalid value (typically too big) for the size of
        the input (surface, pattern, etc.).}
      @entry[:user-font-not-implemented]{User-font method not implemented.}
      @entry[:device-type-mismatch]{The device type is not appropriate for the
        operation.}
      @entry[:device-error]{An operation to the device caused an unspecified
        error.}
      @entry[:invalid-mesh-construction]{A mesh pattern construction operation
        was used outside of a @fun{cairo:mesh-pattern-begin-patch} and
        @fun{cairo:mesh-pattern-end-patch} pair of functions.}
      @entry[:device-finished]{Target device has been finished.}
      @entry[:jbig2-global-missing]{@code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID} has
        been used on at least one image but no image provided
        @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL}.}
      @entry[:png-error]{Error occurred in @code{libpng} while reading from or
        writing to a PNG file.}
      @entry[:freetype-error]{Error occurred in @code{libfreetype}.}
      @entry[:win32-gdi-error]{Error occurred in the Windows Graphics Device
        Interface.}
      @entry[:tag-error]{Invalid tag name, attributes, or nesting.}
      @entry[:dwrite-error]{Error occurred in the Windows Direct Write API.
        Since 1.18}
      @entry[:svg-font-error]{Error occurred in OpenType-SVG font rendering.
        Since 1.18}
      @entry[:last-status]{This is a special value indicating the number of
        status values defined in this enumeration. When using this value, note
        that the version of Cairo at run-time may have additional status values
        defined than the value of this symbol at compile-time.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:status-t} enumeration is used to indicate errors that can
    occur when using Cairo.
  @end{short}
  In some cases it is returned directly by functions. but when using a
  @sym{cairo:context-t} instance, the last error, if any, is stored in the
  context and can be retrieved with the @fun{cairo:status} function.

  Use the @fun{cairo:status-to-string} function to get a human readable
  representation of an error message.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:content-t}
  @see-symbol{cairo:format-t}
  @see-symbol{cairo:font-slant-t}
  @see-symbol{cairo:font-weight-t}
  @see-function{cairo:status}
  @see-function{cairo:status-to-string}
  @see-function{cairo:save}
  @see-function{cairo:restore}
  @see-function{cairo:pop-group}
  @see-function{cairo:push-group}
  @see-function{cairo:mesh-pattern-begin-patch}
  @see-function{cairo:mesh-pattern-end-patch}")

(export 'status-t)

;;; ----------------------------------------------------------------------------
;;; cairo_status_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_status_to_string" %status-to-string) :string
  (status status-t))

(defun status-to-string (status)
 #+liber-documentation
 "@version{2025-09-02}
  @argument[status]{a @sym{cairo:status-t} value}
  @return{The string representation of the Cario status.}
  @begin{short}
    Provides a human readable description of a Cairo status value.
  @end{short}
  @see-symbol{cairo:status-t}"
  (let ((desc '((:success . "no error has occurred")
                (:no-memory . "out of memory")
                (:invalid-restore . "cairo:restore without matching cairo:save")
                (:invalid-pop-group . "no saved group to pop, that is cairo:pop-group without matching cairo:push-group")
                (:no-current-point . "no current point defined")
                (:invalid-matrix . "invalid matrix (not invertible)")
                (:invalid-status . "invalid value for an input cairo:status-t")
                (:null-pointer . "NULL pointer")
                (:invalid-string . "input string not valid UTF-8")
                (:invalid-path-data . "input path data not valid")
                (:read-error . "error while reading from input stream")
                (:write-error . "error while writing to output stream")
                (:surface-finished . "the target surface has been finished")
                (:surface-type-mismatch . "the surface type is not appropriate for the operation")
                (:pattern-type-mismatch . "the pattern type is not appropriate for the operation")
                (:invalid-content . "invalid value for an input cairo:input-t")
                (:invalid-format . "invalid value for an input cairo:format-t")
                (:invalid-visual . "invalid value for an input Visual*")
                (:file-not-found . "file not found")
                (:invalid-dash . "invalid value for a dash setting")
                (:invalid-dsc-comment . "invalid value for a DSC comment")
                (:invalid-index . "invalid index passed to getter")
                (:clip-not-representable . "clip region not representable in desired format")
                (:temp-file-error . "error creating or writing to a temporary file")
                (:invalid-stride . "invalid value for stride")
                (:font-type-mismatch . "the font type is not appropriate for the operation")
                (:user-font-immutable . "the user-font is immutable")
                (:user-font-error . "error occurred in a user-font callback function")
                (:negative-count . "negative number used where it is not allowed")
                (:invalid-clusters . "input clusters do not represent the accompanying text and glyph arrays")
                (:invalid-slant . "invalid value for an input cairo:font-slant-t")
                (:invalid-weight . "invalid value for an input cairo:font-weight-t")
                (:invalid-size . "invalid value (typically too big) for the size of the input (surface, pattern, etc.)")
                (:user-font-not-implemented . "user-font method not implemented")
                (:device-type-mismatch . "the device type is not appropriate for the operation")
                (:device-error . "an operation to the device caused an unspecified error")
                (:invalid-mesh-construction . "invalid operation during mesh pattern construction")
                (:device-finished . "the target device has been finished")
                (:jbig2-global-missing . "CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID used but no CAIRO_MIME_TYPE_JBIG2_GLOBAL data provided")
                (:png-error . "error occurred in libpng while reading from or writing to a PNG file")
                (:freetype-error . "error occurred in libfreetype")
                (:win32-gdk-error . "error occurred in the Windows Graphics Device Interface")
                (:tag-error . "invalid tag name, attributes, or nesting")
                (:dwrite-error . "Window Direct Write error")
                (:svg-font-error .  "error occured while rendering an OpenType-SVG font")
               ))
        text)
  (if (setf text (cdr (assoc status desc :test #'eq)))
      text
      (%status-to-string status))))

(export 'status-to-string)

;;; --- End of file cairo.status.lisp ------------------------------------------
