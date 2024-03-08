(in-package :cairo-test)

(def-suite cairo-device :in cairo-suite)
(in-suite cairo-device)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_device_type_t

(test cairo-device-type-t
  (is (eq :invalid (cffi:foreign-enum-keyword 'cairo:device-type-t -1)))
  (is (eq :drm (cffi:foreign-enum-keyword 'cairo:device-type-t 0)))
  (is (eq :gl (cffi:foreign-enum-keyword 'cairo:device-type-t 1)))
  (is (eq :script (cffi:foreign-enum-keyword 'cairo:device-type-t 2)))
  (is (eq :xcb (cffi:foreign-enum-keyword 'cairo:device-type-t 3)))
  (is (eq :xlib (cffi:foreign-enum-keyword 'cairo:device-type-t 4)))
  (is (eq :xml (cffi:foreign-enum-keyword 'cairo:device-type-t 5)))
  (is (eq :cogl (cffi:foreign-enum-keyword 'cairo:device-type-t 6)))
  (is (eq :win32 (cffi:foreign-enum-keyword 'cairo:device-type-t 7))))

;;;     cairo_device_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_device_reference
;;;     cairo_device_destroy
;;;     cairo_device_get_reference_count

(test cairo-device-reference/destroy
  (let* ((path (sys-path "out/tmp.script"))
         (script (cairo:script-create path)))
    (is (cffi:pointerp script))
    (is (eq :script (cairo:device-type script)))
    (is (= 1 (cairo:device-reference-count script)))
    (is (cffi:pointer-eq script (cairo:device-reference script)))
    (is (= 2 (cairo:device-reference-count script)))
    (is-false (cairo:device-destroy script))
    (is (= 1 (cairo:device-reference-count script)))
    (is-false (cairo:device-destroy script))))

;; no device for image surface
(test cairo-device-reference/destroy.1
  (let ((surface (cairo:image-surface-create :rgb24 100 150)))
    (is-false (cairo:surface-device surface))
    (is-false (cairo:surface-destroy surface))))

;; no device for pdf surface
(test cairo-device-reference/destroy.2
  (let* ((path (sys-path "out/tmp.pdf"))
         (surface (cairo:pdf-surface-create path 100 150)))
    (is-false (cairo:surface-device surface))
    (is-false (cairo:surface-destroy surface))))

;; Example with device for script surface
(test cairo-device-reference/destroy.3
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename))
         (surface (cairo:script-surface-create script :color 100 150)))
    (is (cffi:pointerp script))
    (is (cffi:pointerp surface))
    (is (cffi:pointer-eq script (cairo:surface-device surface)))
    (is-false (cairo:device-destroy script))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_device_status
;;;     cairo_device_finish
;;;     cairo_device_flush

(test cairo-device-status/finish/flush
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename))
         (surface (cairo:script-surface-create script :color 100 150)))
    (is (cffi:pointerp script))
    (is (cffi:pointerp surface))
    (is (cffi:pointer-eq script (cairo:surface-device surface)))
    (is (eq :success (cairo:device-status script)))
    (is-false (cairo:device-flush script))
    (is (eq :success (cairo:device-status script)))
    (is-false (cairo:device-finish script))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_device_get_type

(test cairo-device-type.1
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename)))
    (is (cffi:pointerp script))
    (is (eq :script (cairo:device-type script)))
    (is-false (cairo:device-destroy script))))

(test cairo-device-type.2
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename))
         (surface (cairo:script-surface-create script :color 100 150)))
    (is (cffi:pointerp script))
    (is (cffi:pointerp surface))
    (is (cffi:pointer-eq script (cairo:surface-device surface)))
    (is (eq :script (cairo:device-type script)))
    (is-false (cairo:device-destroy script))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_device_set_user_data
;;;     cairo_device_get_user_data

;;;     cairo_device_acquire
;;;     cairo_device_release

(test cairo-device-acquire/release
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename))
         (surface (cairo:script-surface-create script :color 100 150)))
    ;; Acquire the device
    (is (eq :success (cairo:device-acquire script)))

    (is (cffi:pointerp script))
    (is (cffi:pointerp surface))
    (is (cffi:pointer-eq script (cairo:surface-device surface)))
    (is (eq :script (cairo:device-type script)))
    ;; Release the device
    (is-false (cairo:device-release script))

    (is-false (cairo:device-destroy script))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_device_observer_elapsed ()
;;;     cairo_device_observer_fill_elapsed ()
;;;     cairo_device_observer_glyphs_elapsed ()
;;;     cairo_device_observer_mask_elapsed ()
;;;     cairo_device_observer_paint_elapsed ()
;;;     cairo_device_observer_print ()
;;;     cairo_device_observer_stroke_elapsed ()

;;; 2024-2-11
