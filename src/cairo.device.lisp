;;; ----------------------------------------------------------------------------
;;; cairo.device.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;     cairo_device_set_user_data
;;;     cairo_device_get_user_data
;;;     cairo_device_acquire
;;;     cairo_device_release
;;;
;;;     cairo_device_observer_elapsed ()
;;;     cairo_device_observer_fill_elapsed ()
;;;     cairo_device_observer_glyphs_elapsed ()
;;;     cairo_device_observer_mask_elapsed ()
;;;     cairo_device_observer_paint_elapsed ()
;;;     cairo_device_observer_print ()
;;;     cairo_device_observer_stroke_elapsed ()
;;;
;;; Description
;;;
;;; Devices are the abstraction Cairo employs for the rendering system used by
;;; a cairo_surface_t. You can get the device of a surface using
;;; cairo_surface_get_device().
;;;
;;; Devices are created using custom functions specific to the rendering system
;;; you want to use. See the documentation for the surface types for those
;;; functions.
;;;
;;; An important function that devices fulfill is sharing access to the
;;; rendering system between Cairo and your application. If you want to access
;;; a device directly that you used to draw to with Cairo, you must first call
;;; cairo_device_flush() to ensure that Cairo finishes all operations on the
;;; device and resets it to a clean state.
;;;
;;; Cairo also provides the functions cairo_device_acquire() and
;;; cairo_device_release() to synchronize access to the rendering system in a
;;; multithreaded environment. This is done internally, but can also be used by
;;; applications.
;;;
;;; Putting this all together, a function that works with devices should look
;;; something like this:
;;;
;;; void
;;; my_device_modifying_function (cairo_device_t *device)
;;; {
;;;   cairo_status_t status;
;;;
;;;   // Ensure the device is properly reset
;;;   cairo_device_flush (device);
;;;   // Try to acquire the device
;;;   status = cairo_device_acquire (device);
;;;   if (status != CAIRO_STATUS_SUCCESS) {
;;;     printf ("Failed to acquire the device: %s\n",
;;;             cairo_status_to_string (status));
;;;     return;
;;;   }
;;;
;;;   // Do the custom operations on the device here.
;;;   // But do not call any Cairo functions that might acquire devices.
;;;
;;;   // Release the device when done.
;;;   cairo_device_release (device);
;;; }
;;;
;;; Note
;;;
;;; Please refer to the documentation of each backend for additional usage
;;; requirements, guarantees provided, and interactions with existing surface
;;; API of the device functions for surfaces of that type.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_device_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct device-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'device-t)
      "CStruct"
      (liber:symbol-documentation 'device-t)
 "@version{#2020-12-16}
  @begin{short}
    A @sym{cairo:device-t} structure represents the driver interface for drawing
    operations to a @symbol{cairo:surface-t} instance.
  @end{short}
  There are different subtypes of @sym{cairo:device-t} structures for different
  drawing backends. For example, the @code{egl-device-create} function creates
  a device that wraps an EGL display and context.

  The type of a device can be queried with the @fun{cairo:device-type} function.
  Memory management of the @sym{cairo:device-t} structure is done with the
  @fun{cairo:device-reference} and @fun{cairo:device-destroy} functions.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:device-type}
  @see-function{cairo:device-reference}
  @see-function{cairo:device-destroy}")

(export 'device-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_device_type_t
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
 "@version{#2020-12-16}
  @begin{short}
    The @sym{cairo:device-type-t} enumeration is used to describe the type of a
    given device.
  @end{short}
  The devices types are also known as \"backends\" within Cairo. The device
  type can be queried with the function @fun{cairo:device-type}.

  The various @symbol{cairo:device-t} functions can be used with devices of any
  type, but some backends also provide type-specific functions that must only
  be called with a device of the appropriate type. These functions have names
  that begin with @code{type-device} such as
  @code{xcb-device-debug-cap-xrender-version}.

  The behavior of calling a type-specific function with a device of the wrong
  type is undefined.

  New entries may be added in future versions.
  @begin{pre}
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
  @end{pre}
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
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-type}")

(export 'device-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_device_reference ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_reference" device-reference)
    (:pointer (:struct device-t))
 #+liber-documentation
 "@version{#2020-12-16}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{The referenced @symbol{cairo:device-t} instance.}
  @begin{short}
    Increases the reference count on @arg{device} by one.
  @end{short}
  This prevents @arg{device} from being destroyed until a matching call to
  the @fun{cairo:device-destroy} function is made.

  The number of references to a @symbol{cairo:device-t} instance can be get
  using the @fun{cairo:device-reference-count} function.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}
  @see-function{cairo:device-reference-count}"
  (device (:pointer (:struct device-t))))

(export 'device-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_device_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_destroy" device-destroy) :void
 #+liber-documentation
 "@version{#2020-12-16}
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
;;; cairo_device_status ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_status" device-status) status-t
 #+liber-documentation
 "@version{2023-7-21}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{A @symbol{cairo:status-t} value with an error code if the device is
    in an error state.}
  @begin{short}
    Checks whether an error has previously occurred for this device.
  @end{short}
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:status-t}"
  (device (:pointer (:struct device-t))))

(export 'device-status)

;;; ----------------------------------------------------------------------------
;;; cairo_device_finish ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_finish" device-finish) :void
 #+liber-documentation
 "@version{#2023-1-10}
  @argument[device]{a @symbol{cairo:device-t} instance to finish}
  @begin{short}
    This function finishes the device and drops all references to external
    resources.
  @end{short}
  All surfaces, fonts and other objects created for this device will be
  finished, too. Further operations on the device will not affect the device
  but will instead trigger a @code{:device-finished} error.

  When the last call to the @fun{cairo:device-destroy} function decreases the
  reference count to zero, cairo will call the @sym{cairo:device-finish}
  function if it has not been called already, before freeing the resources
  associated with the device.

  This function may acquire devices.
  @see-symbol{cairo:device-t}
  @see-function{cairo:device-destroy}"
  (device (:pointer (:struct device-t))))

(export 'device-finish)

;;; ----------------------------------------------------------------------------
;;; cairo_device_flush ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_flush" device-flush) :void
 #+liber-documentation
 "@version{#2023-1-10}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{short}
    Finish any pending operations for the device and also restore any temporary
    modifications Cairo has made to the device's state.
  @end{short}
  This function must be called before switching from using the device with Cairo
  to operating on it directly with native APIs. If the device does not support
  direct access, then this function does nothing.

  This function may acquire devices.
  @see-symbol{cairo:device-t}"
  (device (:pointer (:struct device-t))))

(export 'device-flush)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_get_type" device-type) device-type-t
 #+liber-documentation
 "@version{2023-7-21}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @return{A value of the @symbol{cairo:device-type-t} enumeration for the
    type of @arg{device}.}
  @begin{short}
    This function returns the type of the device.
  @end{short}
  See the @symbol{cairo:device-type-t} enumeration for available types.
  @see-symbol{cairo:device-t}
  @see-symbol{cairo:device-type-t}"
  (device (:pointer (:struct device-t))))

(export 'device-type)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_reference_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_get_reference_count" device-reference-count) :uint
 #+liber-documentation
 "@version{#2020-12-16}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{return}
    An unsigned integer with the current reference count of @arg{device}.
    If the instance is a nil instance, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of the device.
  @end{short}
  @see-symbol{cairo:device-t}"
  (device (:pointer (:struct device-t))))

(export 'device-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_device_set_user_data ()
;;;
;;; cairo_status_t cairo_device_set_user_data (cairo_device_t *device,
;;;                                            const cairo_user_data_key_t *key,
;;;                                            void *user_data,
;;;                                            cairo_destroy_func_t destroy);
;;;
;;; Attach user data to device. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_device_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_user_data ()
;;;
;;; void * cairo_device_get_user_data (cairo_device_t *device,
;;;                                    const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to device using the specified key. If
;;; no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_acquire ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_acquire" device-acquire) status-t
 #+liber-documentation
 "@version{#2020-12-16}
  @argument[device]{a @symbol{cairo:device-t} instance}
  @begin{return}
    @code{:success} on success or an error code if the device is in an
    error state and could not be acquired. After a successful call to the
    @sym{cairo:device-acquire} function, a matching call to the
    @fun{cairo:device-release} function is required.
  @end{return}
  @begin{short}
    Acquires the device for the current thread.
  @end{short}
  This function will block until no other thread has acquired the device.

  If the return value is @code{:sucess}, you successfully acquired the device.
  From now on your thread owns the device and no other thread will be able to
  acquire it until a matching call to the @fun{cairo:device-release} function.
  It is allowed to recursively acquire the device multiple times from the same
  thread.
  @begin[Note]{dictionary}
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
;;; cairo_device_release ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_device_release" device-release) :void
 #+liber-documentation
 "@version{#2020-12-16}
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

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_elapsed ()
;;;
;;; double
;;; cairo_device_observer_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_fill_elapsed ()
;;;
;;; double
;;; cairo_device_observer_fill_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_glyphs_elapsed ()
;;;
;;; double
;;; cairo_device_observer_glyphs_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_mask_elapsed ()
;;;
;;; double
;;; cairo_device_observer_mask_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_paint_elapsed ()
;;;
;;; double
;;; cairo_device_observer_paint_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_print ()
;;;
;;; cairo_status_t
;;; cairo_device_observer_print (cairo_device_t *device,
;;;                              cairo_write_func_t write_func,
;;;                              void *closure);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_observer_stroke_elapsed ()
;;;
;;; double
;;; cairo_device_observer_stroke_elapsed (cairo_device_t *device);
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.device.lisp ------------------------------------------
