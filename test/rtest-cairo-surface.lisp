(in-package :cairo-test)

(def-suite cairo-surface :in cairo-suite)
(in-suite cairo-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     CAIRO_HAS_MIME_SURFACE
;;;     CAIRO_MIME_TYPE_CCITT_FAX
;;;     CAIRO_MIME_TYPE_CCITT_FAX_PARAMS
;;;     CAIRO_MIME_TYPE_EPS
;;;     CAIRO_MIME_TYPE_EPS_PARAMS
;;;     CAIRO_MIME_TYPE_JBIG2
;;;     CAIRO_MIME_TYPE_JBIG2_GLOBAL
;;;     CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID
;;;     CAIRO_MIME_TYPE_JP2
;;;     CAIRO_MIME_TYPE_JPEG
;;;     CAIRO_MIME_TYPE_PNG
;;;     CAIRO_MIME_TYPE_URI
;;;     CAIRO_MIME_TYPE_UNIQUE_ID

;;;     cairo_surface_t
;;;     cairo_content_t
;;;     cairo_surface_type_t

;;;     cairo_format_t                 <- cairo.image-surface.lisp

;;; --- Functions --------------------------------------------------------------

;;;     cairo_surface_create_similar
;;;     cairo_surface_create_similar_image
;;;     cairo_surface_create_for_rectangle
;;;     cairo_surface_reference
;;;     cairo_surface_destroy
;;;     cairo_surface_status
;;;     cairo_surface_finish
;;;     cairo_surface_flush

;;;     cairo_surface_get_device

(test cairo-surface-device
  (let ((path (sys-path "out/output.script"))
        (device nil)
        (surface nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :script (cairo:device-type device)))
    (is (cffi:pointerp (setf surface
                             (cairo:script-surface-create device
                                                          :color
                                                          200 100))))
    (is (cffi:pointer-eq device (cairo:surface-device surface)))
    (is (eq :script (cairo:device-type (cairo:surface-device surface))))))

;;;     cairo_surface_get_font_options

(test cairo-surface-font-options
  (cairo:with-cairo-image-surface (surface :rgb24 200 100)
    (let ((options (cairo:font-options-create)))
      (is-false (cairo:surface-font-options surface options))
      (is (eq :success (cairo:font-options-status options)))
      (is (eq :default (cairo:font-options-antialias options)))
      (is (eq :default (cairo:font-options-subpixel-order options)))
      (is (eq :default (cairo:font-options-hint-style options)))
      (is (eq :on (cairo:font-options-hint-metrics options)))
      (is-false (cairo:font-options-variations options))
      (is-false (cairo:font-options-destroy options)))))

;;;     cairo_surface_get_content

(test cairo-surface-content
  (cairo:with-cairo-image-surface (surface :rgb24 200 100)
    (is (eq :color (cairo:surface-content surface)))))

;;;     cairo_surface_mark_dirty
;;;     cairo_surface_mark_dirty_rectangle
;;;     cairo_surface_set_device_offset
;;;     cairo_surface_get_device_offset
;;;     cairo_surface_get_device_scale
;;;     cairo_surface_set_device_scale
;;;     cairo_surface_set_fallback_resolution
;;;     cairo_surface_get_fallback_resolution

;;;     cairo_surface_get_type
;;;     cairo_surface_get_reference_count
;;;     cairo_surface_set_user_data
;;;     cairo_surface_get_user_data
;;;     cairo_surface_copy_page
;;;     cairo_surface_show_page
;;;     cairo_surface_has_show_text_glyphs
;;;     cairo_surface_set_mime_data
;;;     cairo_surface_get_mime_data
;;;     cairo_surface_supports_mime_type
;;;     cairo_surface_map_to_image
;;;     cairo_surface_unmap_image

;;; --- 2023-7-21 --------------------------------------------------------------
