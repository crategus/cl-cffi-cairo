(in-package :cairo-test)

(def-suite cairo-transformation :in cairo-suite)
(in-suite cairo-transformation)

;;; --- Functions --------------------------------------------------------------

;;;     cairo_translate

(test cairo-translate
  (let ((width 200) (height 100))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-context (context surface)
        (is-false (cairo:translate context (/ width 2) (/ height 2)))
        (is (equal '(100.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 0))))
        (is (equal '(0.0d0 0.0d0)
                   (multiple-value-list
                       (cairo:device-to-user context (/ width 2) (/ height 2)))))
))))

;;;     cairo_scale

(test cairo-scale
  (let ((width 200) (height 100) (w 2) (h 2))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-context (context surface)
        (is-false (cairo:translate context (/ width 2) (/ height 2)))
        (is-false (cairo:scale context (/ width w) (/ height h)))
        ;; The origin
        (is (equal '(100.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 0))))
        (is (equal '(0.0d0 0.0d0)
                   (multiple-value-list
                       (cairo:device-to-user context (/ width 2) (/ height 2)))))
        ;; User -> device
        (is (equal '(200.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context 1 1))))
        (is (equal '(100.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 1))))
        (is (equal '(0.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context -1 1))))
        ;; User -> device
        (is (equal '(200.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context 1 1))))
        (is (equal '(200.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context 1 0))))
        (is (equal '(200.0d0 0.0d0)
                   (multiple-value-list (cairo:user-to-device context 1 -1))))
))))

;;;     cairo_rotate

(test cairo-rotate
  (let ((width 200) (height 100) (w 2) (h 2))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-context (context surface)
        (is-false (cairo:translate context (/ width 2) (/ height 2)))
        (is-false (cairo:scale context (/ width w) (/ height h)))
        (is-false (cairo:rotate context pi))
        ;; The origin
        (is (equal '(100.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 0))))
        (is (equal '(0.0d0 0.0d0)
                   (multiple-value-list
                       (cairo:device-to-user context (/ width 2) (/ height 2)))))
        ;; User -> device
        (is (every #'approx-equal
                   '(200.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context -1 -1))))
        (is (every #'approx-equal
                   '(100.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 -1))))
        (is (every #'approx-equal
                   '(0.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context 1 -1))))
        ;; User -> device
        (is (every #'approx-equal
                   '(200.0d0 100.0d0)
                   (multiple-value-list (cairo:user-to-device context -1 -1))))
        (is (every #'approx-equal
                   '(200.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context -1 0))))
        (is (every #'approx-equal
                   '(200.0d0 0.0d0)
                   (multiple-value-list (cairo:user-to-device context -1 1))))
))))

;;;     cairo_transform

(test cairo-transform
  (let ((width 200) (height 100))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-matrix (matrix)
        (cairo:with-context (context surface)
          (is (cffi:pointer-eq matrix
                               (cairo:matrix-translate matrix (/ width 2)
                                                              (/ height 2))))
          (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 100.0d0 50.0d0)
                     (cairo:matrix-to-float matrix)))
          (is-false (cairo:transform context matrix))
          ;; The origin
          (is (equal '(100.0d0 50.0d0)
                     (multiple-value-list (cairo:user-to-device context 0 0))))
          (is (equal '(0.0d0 0.0d0)
                     (multiple-value-list
                         (cairo:device-to-user context (/ width 2) (/ height 2)))))
)))))

;;;     cairo_set_matrix
;;;     cairo_get_matrix
;;;     cairo_identity_matrix

(test cairo-matrix
  (let ((width 200) (height 100))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-matrix (matrix)
        (cairo:with-context (context surface)
          (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
                     (cairo:matrix-to-float (cairo:matrix context matrix))))
          (is (cffi:pointer-eq matrix
                               (cairo:matrix-translate matrix (/ width 2)
                                                              (/ height 2))))
          (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 100.0d0 50.0d0)
                     (cairo:matrix-to-float matrix)))
          (is (cffi:pointer-eq matrix
                               (setf (cairo:matrix context) matrix)))
          (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 100.0d0 50.0d0)
                     (cairo:matrix-to-float (cairo:matrix context matrix))))
          (is-false (cairo:identity-matrix context))
          (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
                     (cairo:matrix-to-float (cairo:matrix context matrix))))
)))))

;;;     cairo_user_to_device
;;;     cairo_user_to_device_distance

(test cairo-user-to-device
  (let ((width 200) (height 100))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-context (context surface)
        (is-false (cairo:translate context (/ width 2) (/ height 2)))
        (is (equal '(100.0d0 50.0d0)
                   (multiple-value-list (cairo:user-to-device context 0 0))))
        (is (equal '(0.0d0 0.0d0)
                   (multiple-value-list
                     (cairo:user-to-device-distance context 0 0))))))))

;;;     cairo_device_to_user
;;;     cairo_device_to_user_distance

(test cairo-device-to-user
  (let ((width 200) (height 100))
    (cairo:with-recording-surface (surface :color 0 0 width height)
      (cairo:with-context (context surface)
        (is-false (cairo:translate context (/ width 2) (/ height 2)))
        (is (equal '(0.0d0 0.0d0)
                   (multiple-value-list
                       (cairo:device-to-user context
                                             (/ width 2) (/ height 2)))))
        (is (equal '(100.0d0 50.0d0)
                   (multiple-value-list
                       (cairo:device-to-user-distance context
                                                      (/ width 2)
                                                      (/ height 2)))))))))

;;; 2024-2-3
