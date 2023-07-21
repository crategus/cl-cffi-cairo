;;; ----------------------------------------------------------------------------
;;; cairo.script-surface.lisp
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
;;; Script Surfaces
;;;
;;;     Rendering to replayable scripts
;;;
;;; Types and Values
;;;
;;;     cairo_script_mode_t
;;;
;;; Functions
;;;
;;;     cairo_script_create
;;;     cairo_script_create_for_stream                     not implemented
;;;     cairo_script_from_recording_surface
;;;     cairo_script_get_mode
;;;     cairo_script_set_mode
;;;     cairo_script_surface_create
;;;     cairo_script_surface_create_for_target
;;;     cairo_script_write_comment
;;; ----------------------------------------------------------------------------

(in-package :cairo)


(defmacro with-cairo-script-surface ((surface path content width height)
                                     &body body)
 #+liber-documentation
 "@version{2023-7-21}
  @syntax[]{(with-cairo-script-surface (surface path content width height) body) => result}
  @argument[surface]{a newly allocated @symbol{cairo:surface-t} instance}
  @argument[path]{a path or namestring with the file to write the script to}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[width]{a number coerced to a double float with the width in pixels}
  @argument[height]{a number coerced to a double float with the height in
    pixels}
  @begin{short}
    The @sym{with-cairo-script-surface} macro allocates a new
    @symbol{cairo:surface-t} instance for a newly created @symbol{device-t}
    instance of @code{:script} type and executes the body that uses the
    Cairo script surface.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface and the
  Cairo device is released.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:device-t}
  @see-function{cairo:script-create}
  @see-function{cairo:script-surface-create}"
  (let ((device (gensym)))
    `(let* ((,device (script-create ,path))
            (,surface (script-surface-create ,device
                                             ,content
                                             ,width
                                             ,height)))
     (unwind-protect
       (progn ,@body)
       (progn
         (surface-destroy ,surface)
         (device-destroy ,device))))))

(export 'with-cairo-script-surface)

;;; ----------------------------------------------------------------------------
;;; enum cairo_script_mode_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum script-mode-t
  :ascii
  :binary)

#+liber-documentation
(setf (liber:alias-for-symbol 'script-mode-t)
      "CEnum"
      (liber:symbol-documentation 'script-mode-t)
 "@version{2023-7-21}
  @short{A set of script output variants.}
  @begin{pre}
(cffi:defcenum script-mode-t
  :ascii
  :binary)
  @end{pre}
  @begin[code]{table}
    @entry[:ascii]{The output will be in readable text (default).}
    @entry[:binary]{The output will use byte codes.}
  @end{table}
  @see-function{cairo:script-mode}")

(export 'script-mode-t)

;;; ----------------------------------------------------------------------------
;;; cairo_script_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_create" %script-create)
    (:pointer (:struct device-t))
  (filename :string))

(defun script-create (path)
 #+liber-documentation
 "@version{2023-7-21}
  @argument[path]{a path or namestring with the file to write the script to}
  @return{A @symbol{cairo:device-t} instance with the newly created device.
    The caller owns the device and should call the @fun{cairo:device-destroy}
    function when done with it. This function always returns a valid pointer,
    but it will return a pointer to a \"nil\" device if an error such as out of
    memory occurs. You can use the @fun{cairo:device-status} function to check
    for this.}
  @begin{short}
    Creates an output device for emitting the script, used when creating the
    individual surfaces.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}
  @see-function{cairo:device-status}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_from_recording_surface"
               script-from-recording-surface) status-t
 #+liber-documentation
 "@version{#2023-7-21}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[surface]{a @symbol{cairo:surface-t} instance with the recording
    surface to replay}
  @return{The @code{:success} value on successful completion or an error code.}
  @begin{short}
    Converts the recorded operations in @arg{surface} into a script.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:surface-t}"
  (script (:pointer (:struct device-t)))
  (surface (:pointer (:struct surface-t))))

(export 'script-from-recording-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_script_get_mode ()
;;; cairo_script_set_mode () -> script-mode
;;; ----------------------------------------------------------------------------

(defun (setf script-mode) (mode script)
  (cffi:foreign-funcall "cairo_script_set_mode"
                        (:pointer (:struct device-t)) script
                        script-mode-t mode
                        :void)
  mode)

(cffi:defcfun ("cairo_script_get_mode" script-mode) script-mode-t
 #+liber-documentation
 "@version{2023-7-21}
  @syntax[]{(cairo:script-mode script) => mode}
  @syntax[]{(setf (cairo:script-mode script) mode)}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[mode]{a @symbol{cairo:script-mode-t} value}
  @begin{short}
    The @sym{cairo:script-mode} function queries the script for its current
    output mode.
  @end{short}
  The @sym{(setf cairo:script-mode} functions changes the output mode of the
  script.
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:script-mode-t}"
  (script (:pointer (:struct device-t))))

(export 'script-mode)

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create ()
;;; ----------------------------------------------------------------------------

(defun script-surface-create (script content width height)
 #+liber-documentation
 "@version{2023-7-21}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[width]{a number coerced to a double float with the width in pixels}
  @argument[height]{a number coerced to a double float with the height in
    pixels}
  @return{A @symbol{cairo:surface-t} instance with the newly created surface.
    The caller owns the surface and should call the @fun{cairo:surface-destroy}
    function when done with it. This function always returns a valid pointer,
    but it will return a pointer to a \"nil\" surface if an error such as out
    of memory occurs. You can use the @fun{cairo:surface-status} function to
    check for this.}
  @begin{short}
    Creates a new surface that will emit its rendering through @arg{script}.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (cffi:foreign-funcall "cairo_script_surface_create"
                        (:pointer (:struct device-t)) script
                        content-t content
                        :double (coerce width 'double-float)
                        :double (coerce height 'double-float)
                        (:pointer (:struct surface-t))))

(export 'script-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create_for_target ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_surface_create_for_target"
               script-surface-create-for-target) (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{#2023-7-21}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[target]{a @symbol{cairo:surface-t} instance}
  @return{A @symbol{cairo:surface-t} instance to the newly created surface.
    The caller owns the surface and should call the @fun{cairo:surface-destroy}
    function when done with it. This function always returns a valid pointer,
    but it will return a pointer to a \"nil\" surface if an error such as out
    of memory occurs. You can use the @fun{cairo:surface-status} function to
    check for this.}
  @begin{short}
    Creates a proxy surface that will render to @arg{target} and record the
    operations to @arg{script}.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (script (:pointer (:struct device-t)))
  (target (:pointer (:struct surface-t))))

(export 'script-surface-create-for-target)

;;; ----------------------------------------------------------------------------
;;; cairo_script_write_comment ()
;;; ----------------------------------------------------------------------------

(defun script-write-comment (script comment)
 #+liber-documentation
 "@version{2023-7-21}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[comment]{a string with the comment to emit}
  @begin{short}
    Emit a string verbatim into the script.
  @end{short}
  @see-symbol{cairo:device-t}"
  (cffi:foreign-funcall "cairo_script_write_comment"
                        (:pointer (:struct device-t)) script
                        :string comment
                        :int -1))

(export 'script-write-comment)

;;; --- End of file cairo.script-surface.lisp ----------------------------------
