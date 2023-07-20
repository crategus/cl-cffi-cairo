;;; ----------------------------------------------------------------------------
;;; cairo.script-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Script Surfaces
;;;
;;;     Rendering to replayable scripts
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_SCRIPT_SURFACE
;;;     cairo_script_mode_t
;;;
;;; Functions
;;;
;;;     cairo_script_create
;;;     cairo_script_create_for_stream
;;;     cairo_script_from_recording_surface
;;;     cairo_script_get_mode
;;;     cairo_script_set_mode
;;;     cairo_script_surface_create
;;;     cairo_script_surface_create_for_target
;;;     cairo_script_write_comment
;;;
;;; Description
;;;
;;;     The script surface provides the ability to render to a native script
;;;     that matches the cairo drawing model. The scripts can be replayed using
;;;     tools under the util/cairo-script directory, or with cairo-perf-trace.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_SCRIPT_SURFACE
;;;
;;; #define CAIRO_HAS_SCRIPT_SURFACE 1
;;;
;;; Defined if the script surface backend is available. The script surface
;;; backend is always built in since 1.12.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_script_mode_t
;;;
;;; A set of script output variants.
;;;
;;; CAIRO_SCRIPT_MODE_ASCII
;;;     the output will be in readable text (default).
;;;
;;; CAIRO_SCRIPT_MODE_BINARY
;;;     the output will use byte codes.
;;; ----------------------------------------------------------------------------

(cffi:defcenum script-mode-t
  :ascii
  :binary)

(export 'script-mode-t)

;;; ----------------------------------------------------------------------------
;;; cairo_script_create ()
;;;
;;; cairo_device_t *
;;; cairo_script_create (const char *filename);
;;;
;;; Creates a output device for emitting the script, used when creating the
;;; individual surfaces.
;;;
;;; filename :
;;;     the name (path) of the file to write the script to
;;;
;;; Returns :
;;;     a pointer to the newly created device. The caller owns the surface and
;;;     should call cairo_device_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" device if an error such as out of memory occurs. You
;;;     can use cairo_device_status() to check for this.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_create" %script-create) (:pointer (:struct device-t))
  (filename :string))

(defun script-create (path)
  (%script-create (namestring path)))

(export 'script-create)

;;; ----------------------------------------------------------------------------
;;; cairo_script_create_for_stream ()
;;;
;;; cairo_device_t *
;;; cairo_script_create_for_stream (cairo_write_func_t write_func,
;;;                                 void *closure);
;;;
;;; Creates a output device for emitting the script, used when creating the
;;; individual surfaces.
;;;
;;; write_func :
;;;     callback function passed the bytes written to the script
;;;
;;; closure :
;;;     user data to be passed to the callback
;;;
;;; Returns :
;;;     a pointer to the newly created device. The caller owns the surface and
;;;     should call cairo_device_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" device if an error such as out of memory occurs. You
;;;     can use cairo_device_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_from_recording_surface ()
;;;
;;; cairo_status_t
;;; cairo_script_from_recording_surface (cairo_device_t *script,
;;;                                      cairo_surface_t *recording_surface);
;;;
;;; Converts the record operations in recording_surface into a script.
;;;
;;; script :
;;;     the script (output device)
;;;
;;; recording_surface :
;;;     the recording surface to replay
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS on successful completion or an error code.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_from_recording_surface"
               script-from-recording-surface) status-t
  (script (:pointer (:struct device-t)))
  (surface (:pointer (:struct surface-t))))

(export 'script-from-recording-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_script_get_mode ()
;;;
;;; cairo_script_mode_t
;;; cairo_script_get_mode (cairo_device_t *script);
;;;
;;; Queries the script for its current output mode.
;;;
;;; script :
;;;     The script (output device) to query
;;;
;;; Returns :
;;;     the current output mode of the script
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; cairo_script_set_mode ()
;;;
;;; void
;;; cairo_script_set_mode (cairo_device_t *script,
;;;                        cairo_script_mode_t mode);
;;;
;;; Change the output mode of the script
;;;
;;; script :
;;;     The script (output device)
;;;
;;; mode :
;;;     the new mode
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

(defun (setf script-mode) (mode script)
  (cffi:foreign-funcall "cairo_script_set_mode"
                        (:pointer (:struct device-t)) script
                        script-mode-t mode
                        :void)
  mode)

(cffi:defcfun ("cairo_script_get_mode" script-mode) script-mode-t
  (script (:pointer (:struct device-t))))

(export 'script-mode)

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_script_surface_create (cairo_device_t *script,
;;;                              cairo_content_t content,
;;;                              double width,
;;;                              double height);
;;;
;;; Create a new surface that will emit its rendering through script
;;;
;;; script :
;;;     the script (output device)
;;;
;;; content :
;;;     the content of the surface
;;;
;;; width :
;;;     width in pixels
;;;
;;; height :
;;;     height in pixels
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;; ----------------------------------------------------------------------------

(defun script-surface-create (script content width height)
  (cffi:foreign-funcall "cairo_script_surface_create"
                        (:pointer (:struct device-t)) script
                        content-t content
                        :double (coerce width 'double-float)
                        :double (coerce height 'double-float)
                        (:pointer (:struct surface-t))))

(export 'script-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create_for_target ()
;;;
;;; cairo_surface_t *
;;; cairo_script_surface_create_for_target
;;;                                (cairo_device_t *script,
;;;                                 cairo_surface_t *target);
;;;
;;; Create a pxoy surface that will render to target and record the operations
;;; to device .
;;;
;;; script :
;;;     the script (output device)
;;;
;;; target :
;;;     a target surface to wrap
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_surface_create_for_target"
               script-surface-create-for-target) (:pointer (:struct surface-t))
  (script (:pointer (:struct device-t)))
  (target (:pointer (:struct surface-t))))

(export 'script-surface-create-for-target)

;;; ----------------------------------------------------------------------------
;;; cairo_script_write_comment ()
;;;
;;; void
;;; cairo_script_write_comment (cairo_device_t *script,
;;;                             const char *comment,
;;;                             int len);
;;;
;;; Emit a string verbatim into the script.
;;;
;;; script :
;;;     the script (output device)
;;;
;;; comment :
;;;     the string to emit
;;;
;;; len :
;;;     the length of the sting to write, or -1 to use strlen()
;;; ----------------------------------------------------------------------------

(defun script-write-comment (script comment)
  (cffi:foreign-funcall "cairo_script_write_comment"
                        (:pointer (:struct device-t)) script
                        :string comment
                        :int -1))

(export 'script-write-comment)

;;; --- End of file cairo.script-surface.lisp ----------------------------------
