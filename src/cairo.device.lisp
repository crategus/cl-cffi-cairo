;;; ----------------------------------------------------------------------------
;;; cairo.device.lisp
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
;;; cairo_device_t
;;;
;;;     Interface to underlying rendering system
;;;
;;; Types and Values
;;;
;;;     cairo_device_t
;;;     cairo_device_type_t
;;;
;;; Functions
;;;
;;;     cairo_device_reference
;;;     cairo_device_destroy
;;;     cairo_device_status
;;;     cairo_device_finish
;;;     cairo_device_flush
;;;
;;;     cairo_device_get_type
;;;     cairo_device_get_reference_count
;;;     cairo_device_set_user_data                          not implemented
;;;     cairo_device_get_user_data                          not implemented
;;;     cairo_device_acquire
;;;     cairo_device_release
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_device_type_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum device-type-t
  :drm
  :gl
  :script
  :xcb
  :xlib
  :xml
  :cogl
  :win32
  (:invalid -1))

#+liber-documentation
(setf (liber:alias-for-symbol 'device-type-t)
      "CEnum"
      (liber:symbol-documentation 'device-type-t)
 "@version{2025-1-18}
  @begin{declaration}
(cffi:defcenum device-type-t
  :drm
  :gl
  :script
  :xcb
  :xlib
  :xml
  :cogl
  :win32
  (:invalid -1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:drm]{The device is of type Direct Render Manager.}
      @entry[:gl]{The device is of type OpenGL.}
      @entry[:script]{The device is of type script.}
      @entry[:xcb]{The device is of type xcb.}
      @entry[:xlib]{The device is of type xlib.}
      @entry[:xml]{The device is of type XML.}
      @entry[:cogl]{The device is of type cogl.}
      @entry[:win32]{The device is of type win32.}
      @entry[:invalid]{The device is invalid.}
  @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:device-type-t} enumeration is used to describe the type
    of a given device.
  @end{short}
  The device types are also known as \"backends\" within Cairo. The device type
  can be queried with the @fun{cairo:device-type} function.

  The various @symbol{cairo:device-t} functions can be used with devices of any
  type, but some backends also provide type specific functions that must only
  be called with a device of the appropriate type. The behavior of calling a
  type specific function with a device of the wrong type is undefined.
  @begin[Notes]{dictionary}
    The only device type implemented in the Lisp API is the @code{:script}
    device type. See the @fun{cairo:with-script-surface} documentation for an
    example that uses a script surface.
  @end{dictionary}
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-type}
  @see-macro{cairo:with-script-surface}")

(export 'device-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_device_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct device-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'device-t)
      "CStruct"
      (liber:symbol-documentation 'device-t)
 "@version{2025-1-18}
  @begin{short}
    The @symbol{cairo:device-t} structure represents the driver interface for
    drawing operations to a @symbol{cairo:surface-t} instance.
  @end{short}
  There are different subtypes of @symbol{cairo:device-t} structures for
  different drawing backends.

  The type of a device can be queried with the @fun{cairo:device-type} function.
  Memory management of the @symbol{cairo:device-t} structure is done with the
  @fun{cairo:device-reference} and @fun{cairo:device-destroy} functions.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:device-type}
  @see-function{cairo:device-reference}
  @see-function{cairo:device-destroy}")

(export 'device-t)

;;; ----------------------------------------------------------------------------
;;; cairo_device_reference
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_reference" device-reference)
    (:pointer (:struct device-t))
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{The referenced @symbol{cairo:device-t} instance.}
  @begin{short}
    Increases the reference count on @arg{device} by one.
  @end{short}
  This prevents the device from being destroyed until a matching call to the
  @fun{cairo:device-destroy} function is made.

  The number of references to a @symbol{cairo:device-t} instance can be get
  using the @fun{cairo:device-reference-count} function.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}
  @see-function{cairo:device-reference-count}"
  (device (:pointer (:struct device-t))))

(export 'device-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_reference_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_get_reference_count" device-reference-count) :uint
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{return}
    The unsigned integer with the current reference count of @arg{device}.
    If the instance is a \"nil\" instance, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of the device.
  @end{short}
  @see-symbol{cairo:device-t}"
  (device (:pointer (:struct device-t))))

(export 'device-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_device_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_destroy" device-destroy) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{short}
    Decreases the reference count on @arg{device} by one.
  @end{short}
  If the result is zero, then @arg{device} and all associated resources are
  freed. See the @fun{cairo:device-reference} function.

  This function may acquire devices if the last reference was dropped.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-reference}"
  (device (:pointer (:struct device-t))))

(export 'device-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_device_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_status" device-status) status-t
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{The @symbol{cairo:status-t} value with an error code if the device is
    in an error state.}
  @begin{short}
    Checks whether an error has previously occurred for this device.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:status-t}"
  (device (:pointer (:struct device-t))))

(export 'device-status)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_get_type" device-type) device-type-t
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{The @symbol{cairo:device-type-t} value for the type of @arg{device}.}
  @begin{short}
    This function returns the type of the device.
  @end{short}
  See the @symbol{cairo:device-type-t} enumeration for available types.
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:device-type-t}"
  (device (:pointer (:struct device-t))))

(export 'device-type)

;;; ----------------------------------------------------------------------------
;;; cairo_device_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_finish" device-finish) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance to finish}
  @begin{short}
    This function finishes the device and drops all references to external
    resources.
  @end{short}
  All surfaces, fonts and other objects created for this device will be
  finished, too. Further operations on the device will not affect the device
  but will instead trigger a @code{:device-finished} error.

  When the last call to the @fun{cairo:device-destroy} function decreases the
  reference count to zero, Cairo will call the @fun{cairo:device-finish}
  function if it has not been called already, before freeing the resources
  associated with the device.

  This function may acquire devices.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}"
  (device (:pointer (:struct device-t))))

(export 'device-finish)

;;; ----------------------------------------------------------------------------
;;; cairo_device_flush
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_flush" device-flush) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{short}
    Finish any pending operations for the device and also restore any temporary
    modifications Cairo has made to the state of the device.
  @end{short}
  This function must be called before switching from using the device with Cairo
  to operating on it directly with native APIs. If the device does not support
  direct access, then this function does nothing.

  This function may acquire devices.
  @see-symbol{cairo:device-t}"
  (device (:pointer (:struct device-t))))

(export 'device-flush)

;;; ----------------------------------------------------------------------------
;;; cairo_device_set_user_data
;;;
;;; Attach user data to device.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_user_data
;;;
;;; Return user data previously attached to device using the specified key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_acquire
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_acquire" device-acquire) status-t
 #+liber-documentation
 "@version{2025-1-29}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{return}
    The @symbol{cairo:status-t} value that is @code{:success} on success or an
    error code if the device is in an error state and could not be acquired.
  @end{return}
  @begin{short}
    Acquires the device for the current thread.
  @end{short}
  This function will block until no other thread has acquired the device. After
  a successful call to the @fun{cairo:device-acquire} function, a matching call
  to the @fun{cairo:device-release} function is required.

  If the return value is @code{:sucess}, you successfully acquired the device.
  From now on your thread owns the device and no other thread will be able to
  acquire it until a matching call to the @fun{cairo:device-release} function.
  It is allowed to recursively acquire the device multiple times from the same
  thread.
  @begin[Notes]{dictionary}
    You must never acquire two different devices at the same time unless this
    is explicitly allowed. Otherwise the possibility of deadlocks exist.

    As various Cairo functions can acquire devices when called, these functions
    may also cause deadlocks when you call them with an acquired device. So you
    must not have a device acquired when calling them. These functions are
    marked in the documentation.
  @end{dictionary}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:status-t}
  @see-function{cairo:device-release}"
  (device (:pointer (:struct device-t))))

(export 'device-acquire)

;;; ----------------------------------------------------------------------------
;;; cairo_device_release
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_release" device-release) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{short}
    Releases a device previously acquired using the @fun{cairo:device-acquire}
    function.
  @end{short}
  See that function for details.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-acquire}"
  (device (:pointer (:struct device-t))))

(export 'device-release)

;;; --- End of file cairo.device.lisp ------------------------------------------
