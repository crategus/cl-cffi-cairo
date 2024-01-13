(in-package :cairo-test)

(def-suite cairo-transformation :in cairo-suite)
(in-suite cairo-transformation)

;;; --- Functions --------------------------------------------------------------

;;;     cairo_translate

(test ctm-translate
  (cairo:with-image-surface (surface :rgb24 150 100)
    (cairo:with-context (context surface)

      (is (cffi:pointer-eq context (cairo:translate context 10 20)))
)))

;;;     cairo_scale
;;;     cairo_rotate
;;;     cairo_transform
;;;     cairo_set_matrix
;;;     cairo_get_matrix
;;;     cairo_identity_matrix
;;;     cairo_user_to_device
;;;     cairo_user_to_device_distance
;;;     cairo_device_to_user
;;;     cairo_device_to_user_distance


;;; 2024-1-12
