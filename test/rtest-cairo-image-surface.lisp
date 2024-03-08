(in-package :cairo-test)

(def-suite cairo-image-surface-suite :in cairo-suite)
(in-suite cairo-image-surface-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_format_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-image-surface

(test cairo-with-image-surface
  (cairo:with-image-surface (surface :argb32 200 100)
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :image (cairo:surface-type surface)))

    (is (eq :argb32 (cairo:image-surface-format surface)))
    (is (cffi:pointerp (cairo:image-surface-data surface)))
    (is (= 200 (cairo:image-surface-width surface)))
    (is (= 100 (cairo:image-surface-height surface)))
    (is (= 800 (cairo:image-surface-stride surface)))))

;;;     cairo:with-context-for-image-surface

(test cairo-with-context-for-image-surface
  (let (surface)
    (cairo:with-context-for-image-surface (context :argb32 200 100)

      (is (eq :success (cairo:status context)))
      (is (cffi:pointerp (setf surface (cairo:target context))))

      (is (eq :success (cairo:surface-status surface)))
      (is (eq :image (cairo:surface-type surface)))

      (is (eq :argb32 (cairo:image-surface-format surface)))
      (is (cffi:pointerp (cairo:image-surface-data surface)))
      (is (= 200 (cairo:image-surface-width surface)))
      (is (= 100 (cairo:image-surface-height surface)))
      (is (= 800 (cairo:image-surface-stride surface))))))

;;;     cairo_image_surface_create

(test cairo-image-surface-create
  (let ((surface (cairo:image-surface-create :argb32 200 100)))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :image (cairo:surface-type surface)))

    (is (eq :argb32 (cairo:image-surface-format surface)))
    (is (cffi:pointerp (cairo:image-surface-data surface)))
    (is (= 200 (cairo:image-surface-width surface)))
    (is (= 100 (cairo:image-surface-height surface)))
    (is (= 800 (cairo:image-surface-stride surface)))

    (is-false (cairo:surface-destroy surface))))

;;;     cairo_image_surface_create_for_data

(test cairo-image-surface-create-for-data
  (let ((target (cairo:image-surface-create :argb32 200 100)))
    (is (eq :success (cairo:surface-status target)))
    (is (eq :image (cairo:surface-type target)))

    (is (eq :argb32 (cairo:image-surface-format target)))
    (is (cffi:pointerp (cairo:image-surface-data target)))

    ;; Create an image surface that uses the data from target
    (let* ((data (cairo:image-surface-data target))
           (width 200)
           (height 100)
           (stride (cairo:format-stride-for-width :argb32 width))
           (surface (cairo:image-surface-create-for-data data
                                                         :argb32
                                                         width
                                                         height
                                                         stride)))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :image (cairo:surface-type surface)))

    (is (eq :argb32 (cairo:image-surface-format surface)))
    (is (cffi:pointer-eq (cairo:image-surface-data surface)
                         (cairo:image-surface-data target)))

      (is (= 200 (cairo:image-surface-width surface)))
      (is (= 100 (cairo:image-surface-height surface)))
      (is (= 800 (cairo:image-surface-stride surface)))

      (is-false (cairo:surface-destroy target))
      (is-false (cairo:surface-destroy surface)))))

;;;     cairo_image_surface_get_data
;;;     cairo_image_surface_get_format
;;;     cairo_image_surface_get_width
;;;     cairo_image_surface_get_height
;;;     cairo_image_surface_get_stride

;; Check for an surface that is not an image surface
(test cairo-image-surface-data
  (cairo:with-recording-surface (surface :color)
    ;; Returns nil if no image surface
    (is-false (cairo:image-surface-data surface))
    (is (eq :invalid (cairo:image-surface-format surface)))
    (is (= 0 (cairo:image-surface-width surface)))
    (is (= 0 (cairo:image-surface-height surface)))
    (is (= 0 (cairo:image-surface-stride surface)))))

;;;     cairo_format_stride_for_width

(test format-stride-for-width
  (is (= 400 (cairo:format-stride-for-width :argb32 100)))
  (is (= 400 (cairo:format-stride-for-width :rgb24 100)))
  (is (= 100 (cairo:format-stride-for-width :a8 100)))
  (is (=  16 (cairo:format-stride-for-width :a1 100)))
  (is (= 200 (cairo:format-stride-for-width :rgb16-565 100)))
  (is (= 400 (cairo:format-stride-for-width :rgb30 100))))

;;; 2024-1-12
