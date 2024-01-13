(in-package :cairo-test)

(def-suite cairo-matrix :in cairo-suite)
(in-suite cairo-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_matrix_t

(test cairo-matrix-t
  (is (= 48 (cffi:foreign-type-size '(:struct cairo:matrix-t))))
  (is (equal '(CAIRO::XX CAIRO::YX CAIRO::XY CAIRO::YY CAIRO::X0 CAIRO::Y0)
             (cffi:foreign-slot-names '(:struct cairo:matrix-t)))))

;;; --- Functions --------------------------------------------------------------

;;;     cairo_matrix_init

(test cairo-matrix-init
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 0 0 0 0 0 0)))
    (is (every #'approx-equal
               '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_init_identity

(test cairo-matrix-init-identiy
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-identity matrix)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_init_translate

(test cairo-matrix-init-translate
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-translate matrix 1 2)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 1.0d0 2.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_init_scale

(test cairo-matrix-init-scale
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-scale matrix 2 3)))
    (is (every #'approx-equal
               '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_init_rotate

(test cairo-matrix-init-rotate.1
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-rotate matrix (/ pi 2))))
    (is (every #'approx-equal
               '(0.0d0 1.0d0 -1.0d0 0.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-matrix-init-rotate.2
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-rotate matrix pi)))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_translate

(test cairo-matrix-translate
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 1 0 0 1 0 0)))
    (is (cffi:pointer-eq matrix (cairo:matrix-translate matrix 2 3)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_scale

(test cairo-matrix-scale
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 1 0 0 1 0 0)))
    (is (cffi:pointer-eq matrix (cairo:matrix-scale matrix 2 3)))
    (is (every #'approx-equal
               '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_rotate

(test cairo-matrix-rotate
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 1 0 0 1 0 0)))
    (is (cffi:pointer-eq matrix (cairo:matrix-rotate matrix pi)))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_invert

(test cairo-matrix-invert
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 2 0 0 2 0 0)))
    (is (cffi:pointer-eq matrix (cairo:matrix-invert matrix)))
    (is (every #'approx-equal
               '(0.5d0 0.0d0 0.0d0 0.5d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_multiply

(test cairo-matrix-multiply
  (cffi:with-foreign-objects ((result '(:struct cairo:matrix-t))
                              (matrix-a '(:struct cairo:matrix-t))
                              (matrix-b '(:struct cairo:matrix-t)))
    (is (cffi:pointer-eq result (cairo:matrix-init-identity result)))
    (is (cffi:pointer-eq matrix-a (cairo:matrix-init matrix-a 1 1 1 1 0 0)))
    (is (cffi:pointer-eq matrix-b (cairo:matrix-init matrix-b 2 0 0 2 0 0)))
    (is (cffi:pointer-eq result (cairo:matrix-multiply result matrix-a matrix-b)))
;    (is (every #'approx-equal
;               '(2.0d0 2.0d0 2.0d0 2.0d0 0.0d0 0.0d0)
;               (cairo:matrix-to-float result)))
))

;;;     cairo_matrix_transform_distance

(test cairo-matrix-transform-distance
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 2 0 0 2 0 0)))
    (is (equal '(2.0d0 4.0d0)
               (multiple-value-list
                   (cairo:matrix-transform-distance matrix 1.0 2.0))))
))

;;;     cairo_matrix_transform_point

(test cairo-matrix-transform-point
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init matrix 2 0 0 2 0 0)))
    (is (equal '(2.0d0 4.0d0)
               (multiple-value-list
                   (cairo:matrix-transform-point matrix 1.0 2.0))))))

;;; 2024-1-12
