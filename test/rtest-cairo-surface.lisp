(in-package :cairo-test)

(def-suite cairo-surface-suite :in cairo-suite)
(in-suite cairo-surface-suite)

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

;;;     cairo:with-surface

(test cairo-with-surface.1
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-surface (surface target :color 50 50)

      (is (eq :success (cairo:surface-status surface)))
      (is (eq :image (cairo:surface-type surface)))
      (is (= 1 (cairo:surface-reference-count surface)))
      (is (eq :color (cairo:surface-content surface)))

      (is (= 50 (cairo:image-surface-width surface)))
      (is (= 50 (cairo:image-surface-height surface)))
      (is (= 200 (cairo:image-surface-stride surface))))))

(test cairo-with-surface.2
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-surface (surface target :argb32 50 50)

      (is (eq :success (cairo:surface-status surface)))
      (is (eq :image (cairo:surface-type surface)))
      (is (= 1 (cairo:surface-reference-count surface)))
      (is (eq :color-alpha (cairo:surface-content surface)))

      (is (= 50 (cairo:image-surface-width surface)))
      (is (= 50 (cairo:image-surface-height surface)))
      (is (= 200 (cairo:image-surface-stride surface))))))

(test cairo-with-surface.3
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-surface (surface target 0 0 50 50)

      (is (eq :success (cairo:surface-status surface)))
      ;; TODO: We get a surface of type :image, but not :subsurface!?
      (is (eq :image (cairo:surface-type surface)))
      (is (= 1 (cairo:surface-reference-count surface)))
      (is (eq :color (cairo:surface-content surface))))))

;;;     cairo:with-context-for-surface

(test cairo-with-context-for-surface.1
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-context-for-surface (context target :color 50 50)

      (is (eq :success (cairo:surface-status (cairo:target context))))
      (is (eq :image (cairo:surface-type (cairo:target context))))
      ;; TODO: Why get we 3 for reference count!?
      (is (= 3 (cairo:surface-reference-count (cairo:target context))))
      (is (eq :color (cairo:surface-content (cairo:target context))))

      (is (= 50 (cairo:image-surface-width (cairo:target context))))
      (is (= 50 (cairo:image-surface-height (cairo:target context))))
      (is (= 200 (cairo:image-surface-stride (cairo:target context)))))))

(test cairo-with-context-for-surface.2
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-context-for-surface (context target :argb32 50 50)

      (is (eq :success (cairo:surface-status (cairo:target context))))
      (is (eq :image (cairo:surface-type (cairo:target context))))
      (is (= 3 (cairo:surface-reference-count (cairo:target context))))
      (is (eq :color-alpha (cairo:surface-content (cairo:target context))))

      (is (= 50 (cairo:image-surface-width (cairo:target context))))
      (is (= 50 (cairo:image-surface-height (cairo:target context))))
      (is (= 200 (cairo:image-surface-stride (cairo:target context)))))))

(test cairo-with-context-for-surface.3
  (cairo:with-image-surface (target :rgb24 200 100)
    (cairo:with-context-for-surface (context target 0 0 50 50)

      (is (eq :success (cairo:surface-status (cairo:target context))))
      ;; TODO: We get a surface of type :image, but not :subsurface!?
      (is (eq :image (cairo:surface-type (cairo:target context))))
      (is (= 3 (cairo:surface-reference-count (cairo:target context))))
      (is (eq :color (cairo:surface-content (cairo:target context)))))))

;;;     cairo_surface_create_similar

(test cairo-surface-create-similar.1
  (cairo:with-image-surface (surface :rgb24 200 100)
    (let ((similar (cairo:surface-create-similar surface :color 50 50)))
      (is (eq :success (cairo:surface-status similar)))
      (is (eq :image (cairo:surface-type similar)))
      (is (= 1 (cairo:surface-reference-count similar)))
      (is (eq :color (cairo:surface-content similar)))

      (is (= 50 (cairo:image-surface-width similar)))
      (is (= 50 (cairo:image-surface-height similar)))
      (is (= 200 (cairo:image-surface-stride similar)))

      (is-false (cairo:surface-destroy similar)))))

(test cairo-surface-create-similar.2
  (cairo:with-pdf-surface (surface nil 200 100)
    (let ((similar (cairo:surface-create-similar surface :color 50 50)))
      (is (eq :success (cairo:surface-status similar)))
      (is (eq :recording (cairo:surface-type similar)))
      (is (= 1 (cairo:surface-reference-count similar)))
      (is (eq :color (cairo:surface-content similar)))

      (is-false (cairo:surface-destroy similar)))))

;;;     cairo_surface_create_similar_image

(test cairo-surface-create-similar-image
  (cairo:with-image-surface (surface :rgb24 200 100)

    (let ((similar (cairo:surface-create-similar-image surface :argb32 50 50)))

      (is (eq :success (cairo:surface-status similar)))
      (is (eq :image (cairo:surface-type similar)))
      (is (= 1 (cairo:surface-reference-count similar)))
      (is (eq :color-alpha (cairo:surface-content similar)))

      (is (= 50 (cairo:image-surface-width similar)))
      (is (= 50 (cairo:image-surface-height similar)))
      (is (= 200 (cairo:image-surface-stride similar)))

      (is-false (cairo:surface-destroy similar)))))

;;;     cairo_surface_create_for_rectangle

(test cairo-surface-create-for-rectangle.1
  (cairo:with-image-surface (surface :rgb24 200 100)
    (let ((similar (cairo:surface-create-for-rectangle surface 0 0 50 50)))
      (is (eq :success (cairo:surface-status similar)))
      (is (eq :image (cairo:surface-type similar)))
      (is (= 1 (cairo:surface-reference-count similar)))
      (is (eq :color (cairo:surface-content similar)))

      (is (= 0 (cairo:image-surface-width similar)))
      (is (= 0 (cairo:image-surface-height similar)))
      (is (= 0 (cairo:image-surface-stride similar)))

      (is-false (cairo:surface-destroy similar)))))

(test cairo-surface-create-for-rectangle.2
  (cairo:with-pdf-surface (surface nil 200 100)
    (let ((similar (cairo:surface-create-for-rectangle surface 0 0 50 50)))
      (is (eq :success (cairo:surface-status similar)))
      (is (eq :pdf (cairo:surface-type similar)))
      (is (= 1 (cairo:surface-reference-count similar)))
      (is (eq :color-alpha (cairo:surface-content similar)))

      (is-false (cairo:surface-destroy similar)))))

;;;     cairo_surface_reference
;;;     cairo_surface_get_reference_count
;;;     cairo_surface_destroy
;;;     cairo_surface_get_type
;;;     cairo_surface_status

(test cairo-surface-reference/destroy
  (cairo:with-recording-surface (surface :color)
    (is (eq :recording (cairo:surface-type surface)))
    (is (eq :success (cairo:surface-status surface)))
    (is (= 1 (cairo:surface-reference-count surface)))
    (is (cffi:pointer-eq surface (cairo:surface-reference surface)))
    (is (= 2 (cairo:surface-reference-count surface)))
    (is-false (cairo:surface-destroy surface))
    (is (= 1 (cairo:surface-reference-count surface)))))

;;;     cairo_surface_finish
;;;     cairo_surface_flush

;;;     cairo_surface_get_device

(test cairo-surface-device.1
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
    (is (eq :script (cairo:device-type (cairo:surface-device surface))))

    (is-false (cairo:surface-destroy surface))
    (is-false (cairo:device-destroy device))))

(test cairo-surface-device.2
  (cairo:with-image-surface (surface :rgb24 200 100)
    (is-false (cairo:surface-device surface))))

;;;     cairo_surface_get_font_options

(test cairo-surface-font-options
  (cairo:with-image-surface (surface :rgb24 200 100)
    (let ((options (cairo:font-options-create)))
      (is (cffi:pointer-eq options
                           (cairo:surface-font-options surface options)))
      (is (eq :success (cairo:font-options-status options)))
      (is (eq :default (cairo:font-options-antialias options)))
      (is (eq :default (cairo:font-options-subpixel-order options)))
      (is (eq :default (cairo:font-options-hint-style options)))
      (is (eq :on (cairo:font-options-hint-metrics options)))
      (is-false (cairo:font-options-variations options))
      (is-false (cairo:font-options-destroy options)))))

;;;     cairo_surface_get_content

(test cairo-surface-content
  (cairo:with-image-surface (surface :rgb24 200 100)
    (is (eq :color (cairo:surface-content surface)))))

;;;     cairo_surface_mark_dirty
;;;     cairo_surface_mark_dirty_rectangle

;;;     cairo_surface_set_device_offset
;;;     cairo_surface_get_device_offset

(test cairo-surface-device-offset
  (cairo:with-image-surface (surface :rgb24 200 100)
    (is (equal '(0.0d0 0.0d0)
               (multiple-value-list (cairo:surface-device-offset surface))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (setf (cairo:surface-device-offset surface) '(10 20)))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list (cairo:surface-device-offset surface))))))

;;;     cairo_surface_get_device_scale
;;;     cairo_surface_set_device_scale

(test cairo-surface-device-scale
  (cairo:with-image-surface (surface :rgb24 200 100)
    (is (equal '(1.0d0 1.0d0)
               (multiple-value-list (cairo:surface-device-scale surface))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (setf (cairo:surface-device-scale surface) '(10 20)))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list (cairo:surface-device-scale surface))))))

;;;     cairo_surface_set_fallback_resolution
;;;     cairo_surface_get_fallback_resolution

(test cairo-surface-fallback-resolution
  (cairo:with-image-surface (surface :rgb24 200 100)
    (is (equal '(300.0d0 300.0d0)
               (multiple-value-list
                 (cairo:surface-fallback-resolution surface))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (setf (cairo:surface-fallback-resolution surface) '(10 20)))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (cairo:surface-fallback-resolution surface))))))

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

;;; 2024-1-13
