(in-package :cairo-test)

(def-suite cairo-device :in cairo-suite)
(in-suite cairo-device)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_device_t
;;;     cairo_device_type_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_device_reference
;;;     cairo_device_destroy
;;;     cairo_device_get_reference_count

(test device-reference/destroy
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
(test device-reference/destroy.1
  (let ((surface (cairo:image-surface-create :rgb24 100 150)))
    (is (cffi:null-pointer-p (cairo:surface-device surface)))
    (is-false (cairo:surface-destroy surface))))

;; no device for pdf surface
(test device-reference/destroy.2
  (let* ((path (sys-path "out/tmp.pdf"))
         (surface (cairo:pdf-surface-create path 100 150)))
    (is (cffi:null-pointer-p (cairo:surface-device surface)))
    (is-false (cairo:surface-destroy surface))))

;; Example with device for script surface
(test device-reference/destroy.3
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

(test device-status/finish/flush
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

(test device-type.1
  (let* ((filename (sys-path "out/tmp.script"))
         (script (cairo:script-create filename)))
    (is (cffi:pointerp script))
    (is (eq :script (cairo:device-type script)))
    (is-false (cairo:device-destroy script))))

(test device-type.2
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

;;;     cairo_device_observer_elapsed ()
;;;     cairo_device_observer_fill_elapsed ()
;;;     cairo_device_observer_glyphs_elapsed ()
;;;     cairo_device_observer_mask_elapsed ()
;;;     cairo_device_observer_paint_elapsed ()
;;;     cairo_device_observer_print ()
;;;     cairo_device_observer_stroke_elapsed ()

;;; --- 2023-1-26 --------------------------------------------------------------
