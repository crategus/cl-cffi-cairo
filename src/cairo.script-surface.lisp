;;; ----------------------------------------------------------------------------
;;; cairo.script-surface.lisp
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
 "@version{2024-1-13}
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
;;; cairo:with-script-surface
;;; ----------------------------------------------------------------------------

(defmacro with-script-surface ((surface path content width height) &body body)
 #+liber-documentation
 "@version{2024-1-13}
  @syntax{(cairo:with-script-surface (surface path content width height) body)
    => result}
  @argument[surface]{a newly allocated @symbol{cairo:surface-t} instance}
  @argument[path]{a path or namestring with the file to write the script to}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[width]{a number coerced to a double float with the width in pixels}
  @argument[height]{a number coerced to a double float with the height in
    pixels}
  @begin{short}
    The @fun{cairo:with-script-surface} macro allocates a new
    @symbol{cairo:surface-t} instance for a newly created
    @symbol{cairo:device-t} instance of @code{:script} type and executes the
    body that uses the Cairo script surface.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface and the
  Cairo device is released. This macro calls the @fun{cairo:script-create}
  function to create the device and the @fun{cairo:script-surface-create}
  function to create the surface.
  @begin[Example]{dictionary}
    From the examples for the Cairo library, which shows how to use a script
    surface.
    @begin{pre}
;; Draw a rectangle on a Cairo context
(defun draw-stroke (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.1)
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:stroke context)
  (cairo:restore context))

;; Generate a script surface and call a draw function
(defun demo-script-draw (&optional (drawfunc #'draw-stroke))
  (let ((path (sys-path \"out/script-draw.script\"))
        (width 200)
        (height 200))
    (cairo:with-script-surface (surface path :color width height)
      (cairo:with-context (context surface)
        (funcall drawfunc context width height)
        (cairo:surface-show-page surface)))))
    @end{pre}
    This is the output of this example.
    @begin{pre}
%!CairoScript
<< /content //COLOR /width 200 /height 200 >> surface context
1 g set-source
paint
n 50 50 100 100 rectangle
1 0 0 rgb set-source
200 200 scale
0.1 set-line-width
stroke+
show-page
pop
    @end{pre}
  @end{dictionary}
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

(export 'with-script-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_script_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_create" %script-create)
    (:pointer (:struct device-t))
  (filename :string))

(defun script-create (path)
 #+liber-documentation
 "@version{2024-1-13}
  @argument[path]{a path or namestring with the file to write the script to}
  @return{The newly created @symbol{cairo:device-t} instance.}
  @begin{short}
    Creates an output device for emitting the script, used when creating the
    individual surfaces.
  @end{short}
  The caller owns the device and should call the @fun{cairo:device-destroy}
  function when done with it. This function always returns a valid pointer, but
  it will return a pointer to a \"nil\" device if an error such as out of
  memory occurs. You can use the @fun{cairo:device-status} function to check
  for this.
  @begin[Note]{dictionary}
    Use the @fun{cairo:with-script-surface} macro to create a Cairo surface
    for an output device.
  @end{dictionary}
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}
  @see-function{cairo:device-status}
  @see-function{cairo:with-script-surface}"
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
;;;     pointer to a "nil" device if an error such as out of memory occurs.
;;;     You can use cairo_device_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_script_from_recording_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_script_from_recording_surface"
               script-from-recording-surface) status-t
 #+liber-documentation
 "@version{2024-1-13}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[surface]{a @symbol{cairo:surface-t} instance with the recording
    surface to replay}
  @return{The @code{:success} value on successful completion or an error code.}
  @short{Converts the recorded operations in @arg{surface} into a script.}
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
 "@version{2024-1-13}
  @syntax{(cairo:script-mode script) => mode}
  @syntax{(setf (cairo:script-mode script) mode)}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[mode]{a @symbol{cairo:script-mode-t} value}
  @begin{short}
    The @fun{cairo:script-mode} function queries the script for its current
    output mode.
  @end{short}
  The @setf{cairo:script-mode} functions changes the output mode of the script.
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:script-mode-t}"
  (script (:pointer (:struct device-t))))

(export 'script-mode)

;;; ----------------------------------------------------------------------------
;;; cairo_script_surface_create ()
;;; ----------------------------------------------------------------------------

(defun script-surface-create (script content width height)
 #+liber-documentation
 "@version{2024-1-13}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[width]{a number coerced to a double float with the width in pixels}
  @argument[height]{a number coerced to a double float with the height in
    pixels}
  @return{The newly created @symbol{cairo:surface-t} instance.}
  @begin{short}
    Creates a new surface that will emit its rendering through @arg{script}.
  @end{short}
  The caller owns the surface and should call the @fun{cairo:surface-destroy}
  function when done with it. This function always returns a valid pointer, but
  it will return a pointer to a \"nil\" surface if an error such as out of
  memory occurs. You can use the @fun{cairo:surface-status} function to check
  for this.
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
 "@version{2024-1-13}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[target]{a @symbol{cairo:surface-t} instance}
  @return{The newly created @symbol{cairo:surface-t} instance.}
  @begin{short}
    Creates a proxy surface that will render to @arg{target} and record the
    operations to @arg{script}.
  @end{short}
  The caller owns the surface and should call the @fun{cairo:surface-destroy}
  function when done with it. This function always returns a valid pointer, but
  it will return a pointer to a \"nil\" surface if an error such as out of
  memory occurs. You can use the @fun{cairo:surface-status} function to check
  for this.
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
 "@version{2024-1-13}
  @argument[script]{a @symbol{cairo:device-t} instance}
  @argument[comment]{a string with the comment to emit}
  @short{Emit a string verbatim into the script.}
  @see-symbol{cairo:device-t}"
  (cffi:foreign-funcall "cairo_script_write_comment"
                        (:pointer (:struct device-t)) script
                        :string comment
                        :int -1))

(export 'script-write-comment)

;;; --- End of file cairo.script-surface.lisp ----------------------------------
