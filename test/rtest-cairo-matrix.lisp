(in-package :cairo-test)

(def-suite cairo-matrix-suite :in cairo-suite)
(in-suite cairo-matrix-suite)

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
    (is (cffi:pointer-eq matrix
                         (cairo:matrix-init matrix 1/2 0 0 1 2.0 3.0)))
    (is (every #'approx-equal
               '(0.5d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-with-matrix.1
  (cairo:with-matrix (matrix 1/2 0 0 1 2.0 3.0)
    (is (every #'approx-equal
               '(0.5d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_init_identity

(test cairo-matrix-init-identiy
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-identity matrix)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-with-matrix.2
  (cairo:with-matrix (matrix)
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

(test cairo-with-matrix.3
  (cairo:with-matrix (matrix :translate 1 2)
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

(test cairo-with-matrix.4
  (cairo:with-matrix (matrix :scale 2 3)
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

(test cairo-with-matrix.5
  (cairo:with-matrix (matrix (/ pi 2))
    (is (every #'approx-equal
               '(0.0d0 1.0d0 -1.0d0 0.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-matrix-init-rotate.2
  (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
    (is (cffi:pointer-eq matrix (cairo:matrix-init-rotate matrix pi)))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-with-matrix.6
  (cairo:with-matrix (matrix pi)
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_translate

(test cairo-matrix-translate
  (cairo:with-matrix (matrix)
    (is (cffi:pointer-eq matrix (cairo:matrix-translate matrix 2 3)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_scale

(test cairo-matrix-scale
  (cairo:with-matrix (matrix)
    (is (cffi:pointer-eq matrix (cairo:matrix-scale matrix 2 3)))
    (is (every #'approx-equal
               '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_rotate

(test cairo-matrix-rotate
  (cairo:with-matrix (matrix)
    (is (cffi:pointer-eq matrix (cairo:matrix-rotate matrix pi)))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_invert

(test cairo-matrix-invert.1
  (cairo:with-matrix (matrix 2 0 0 4 0 0)
    (is (cffi:pointer-eq matrix (cairo:matrix-invert matrix)))
    (is (every #'approx-equal
               '(0.5d0 0.0d0 0.0d0 0.25d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix)))))

(test cairo-matrix-invert.2
  (cairo:with-matrix (matrix 1 0 0 1 2 3)
    (is (cffi:pointer-eq matrix (cairo:matrix-invert matrix)))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 -2.0d0 -3.0d0)
               (cairo:matrix-to-float matrix)))))

;;;     cairo_matrix_multiply

(test cairo-matrix-multiply.1
  (cairo:with-matrices (result (matrix-a 2 0 0 2 0 0)
                               (matrix-b 3 0 0 3 0 0))
    (is (cffi:pointer-eq result
                         (cairo:matrix-multiply result matrix-a matrix-b)))
    (is (every #'approx-equal
               '(6.0d0 0.0d0 0.0d0 6.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float result)))))

(test cairo-matrix-multiply.2
  (cairo:with-matrices ((matrix-a 2 0 0 2 0 0) (matrix-b 3 0 0 3 0 0))
    (is (cffi:pointer-eq matrix-a
                         (cairo:matrix-multiply matrix-a matrix-a matrix-b)))
    (is (every #'approx-equal
               '(6.0d0 0.0d0 0.0d0 6.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix-a)))))

(test cairo-matrix-multiply.3
  (cairo:with-matrices ((matrix-a 2 0 0 2 0 0) (matrix-b 3 0 0 3 0 0))
    (is (cffi:pointer-eq matrix-b
                         (cairo:matrix-multiply matrix-b matrix-a matrix-b)))
    (is (every #'approx-equal
               '(6.0d0 0.0d0 0.0d0 6.0d0 0.0d0 0.0d0)
               (cairo:matrix-to-float matrix-b)))))

;;;     cairo_matrix_transform_distance

(test cairo-matrix-transform-distance
  (cairo:with-matrix (matrix 2 0 0 3 0 0)
    (is (equal '(2.0d0 6.0d0)
               (multiple-value-list
                   (cairo:matrix-transform-distance matrix 1.0 2.0))))))

;;;     cairo_matrix_transform_point

(test cairo-matrix-transform-point
  (cairo:with-matrix (matrix 2 0 0 3 0 0)
    (is (equal '(2.0d0 6.0d0)
               (multiple-value-list
                   (cairo:matrix-transform-point matrix 1.0 2.0))))))

;;; 2024-1-27
