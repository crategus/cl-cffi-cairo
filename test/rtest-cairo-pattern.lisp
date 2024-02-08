(in-package :cairo-test)

(def-suite cairo-pattern :in cairo-suite)
(in-suite cairo-pattern)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_pattern_t
;;;     cairo_extend_t
;;;     cairo_filter_t
;;;     cairo_pattern_type_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_pattern_reference
;;;     cairo_pattern_get_reference_count
;;;     cairo_pattern_destroy

(test cairo-pattern-reference/destory
  (let ((pattern (cairo:pattern-create-rgb 1.0 0.0 0.0)))
    (is (= 1 (cairo:pattern-reference-count pattern)))
    (is (cffi:pointer-eq pattern (cairo:pattern-reference pattern)))
    (is (= 2 (cairo:pattern-reference-count pattern)))
    (is-false (cairo:pattern-destroy pattern))
    (is (= 1 (cairo:pattern-reference-count pattern)))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_pattern_status
;;;     cairo_pattern_get_type

(test cairo-pattern-status/type
  (let ((pattern (cairo:pattern-create-rgb 1.0 0.0 0.0)))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :solid (cairo:pattern-type pattern)))))

;;;     cairo_pattern_set_extend
;;;     cairo_pattern_get_extend

(test cairo-pattern-status-extend
  (let ((pattern (cairo:pattern-create-rgb 1.0 0.0 0.0)))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :pad (setf (cairo:pattern-extend pattern) :pad)))
    (is (eq :pad (cairo:pattern-extend pattern)))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_pattern_set_filter
;;;     cairo_pattern_get_filter

(test cairo-pattern-status-filter
  (let ((pattern (cairo:pattern-create-rgb 1.0 0.0 0.0)))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :good (setf (cairo:pattern-filter pattern) :good)))
    (is (eq :good (cairo:pattern-filter pattern)))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_pattern_set_matrix
;;;     cairo_pattern_get_matrix

(test cairo-pattern-matrix
  (let ((pattern (cairo:pattern-create-rgb 1.0 0.0 0.0)))
    (cairo:with-matrix (matrix)
      (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float (cairo:pattern-matrix pattern matrix))))
      (is (cffi:pointer-eq matrix (cairo:matrix-scale matrix 2 3)))
      (is (cffi:pointer-eq matrix (setf (cairo:pattern-matrix pattern) matrix)))
      (is (cffi:pointer-eq matrix (cairo:pattern-matrix pattern matrix)))
      (is (equal '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float
                     (cairo:pattern-matrix pattern matrix)))))))

;;;     cairo_pattern_add_color_stop_rgb
;;;     cairo_pattern_add_color_stop_rgba
;;;     cairo_pattern_get_color_stop_count
;;;     cairo_pattern_get_color_stop_rgba

(test cairo-pattern-color-stop
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-linear 0 0 0 256)))
      (is (eq :success (cairo:pattern-status pattern)))
      (is (eq :linear (cairo:pattern-type pattern)))

      (is-false (cairo:pattern-add-color-stop-rgb  pattern 1.0 0.0 0.0 0.0))
      (is-false (cairo:pattern-add-color-stop-rgba pattern 0.0 1.0 1.0 1.0 0.5))

      (is (= 2 (cairo:pattern-color-stop-count pattern)))

      (is (equal '(0.0d0 1.0d0 1.0d0 1.0d0 0.5d0)
                 (multiple-value-list (cairo:pattern-color-stop-rgba pattern 0))))
      (is (equal '(1.0d0 0.0d0 0.0d0 0.0d0 1.0d0)
                 (multiple-value-list (cairo:pattern-color-stop-rgba pattern 1))))
)))

;;;     cairo_pattern_create_rgb

(test cairo-pattern-create-rgb
  (let ((pattern (cairo:pattern-create-rgb 1 1/2 0.5)))
    (is (cffi:pointerp pattern))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (= 1 (cairo:pattern-reference-count pattern)))
    (is (eq :pad (cairo:pattern-extend pattern)))
    (is (eq :good (cairo:pattern-filter pattern)))
    (is (eq :solid (cairo:pattern-type pattern)))
    (is (equal '(1.0d0 0.5d0 0.5d0 1.0d0)
               (multiple-value-list (cairo:pattern-rgba pattern))))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_pattern_create_rgba

(test cairo-pattern-create-rgba
  (let ((pattern (cairo:pattern-create-rgba 1 1/2 0.5 0.5)))
    (is (cffi:pointerp pattern))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (= 1 (cairo:pattern-reference-count pattern)))
    (is (eq :pad (cairo:pattern-extend pattern)))
    (is (eq :good (cairo:pattern-filter pattern)))
    (is (eq :solid (cairo:pattern-type pattern)))
    (is (equal '(1.0d0 0.5d0 0.5d0 0.5d0)
               (multiple-value-list (cairo:pattern-rgba pattern))))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_pattern_get_rgba

(test cairo-pattern-rgba.1
  (let ((pattern (cairo:pattern-create-rgb 1 1/2 0.5)))
    (is (equal '(1.0d0 0.5d0 0.5d0 1.0d0)
               (multiple-value-list (cairo:pattern-rgba pattern))))))

(test cairo-pattern-rgba.2
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-for-surface surface)))
      (is-false (cairo:pattern-rgba pattern))
      (is-false (cairo:pattern-destroy pattern)))))

;;;     cairo_pattern_create_for_surface
;;;     cairo_pattern_get_surface

(test cairo-pattern-create-for-surface.1
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-for-surface surface)))
      (is (cffi:pointerp pattern))
      (is (eq :success (cairo:pattern-status pattern)))
      (is (= 1 (cairo:pattern-reference-count pattern)))
      (is (eq :none (cairo:pattern-extend pattern)))
      (is (eq :good (cairo:pattern-filter pattern)))
      (is (eq :surface (cairo:pattern-type pattern)))
      (is-false (cairo:pattern-destroy pattern)))))

(test cairo-pattern-create-for-surface.2
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-for-surface surface)))
      (is (cffi:pointerp pattern))
      (is (eq :success (cairo:pattern-status pattern)))
      (is (= 1 (cairo:pattern-reference-count pattern)))
      (is (eq :none (cairo:pattern-extend pattern)))
      (is (eq :good (cairo:pattern-filter pattern)))
      (is (eq :surface (cairo:pattern-type pattern)))
      (is (cffi:pointer-eq surface (cairo:pattern-surface pattern)))
      (is (eq :recording (cairo:surface-type surface)))
      (is (eq :recording (cairo:surface-type (cairo:pattern-surface pattern))))
      (is-false (cairo:pattern-destroy pattern)))))

;;;     cairo_pattern_create_linear
;;;     cairo_pattern_get_linear_points

(test cairo-pattern-creat-linear
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-linear 0 0 0 256)))
      (is (eq :success (cairo:pattern-status pattern)))
      (is (eq :linear (cairo:pattern-type pattern)))
      (is (equal '(0.0d0 0.0d0 0.0d0 256.0d0)
                 (multiple-value-list (cairo:pattern-linear-points pattern)))))))

;;;     cairo_pattern_create_radial
;;;     cairo_pattern_get_radial_circles

(test cairo-pattern-creat-radial
  (cairo:with-recording-surface (surface :color)
    (let ((pattern (cairo:pattern-create-radial 115.2 102.4  25.6
                                                102.4 102.4 128.0)))
      (is (eq :success (cairo:pattern-status pattern)))
      (is (eq :radial (cairo:pattern-type pattern)))
      (is (every #'approx-equal
                 '(115.2 102.4  25.6 102.4 102.4 128.0)
                 (multiple-value-list (cairo:pattern-radial-circles pattern)))))))

;;;     cairo_pattern_create_mesh

(test cairo-mesh-pattern-create
  (let ((pattern (cairo:pattern-create-mesh)))
    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :mesh (cairo:pattern-type pattern)))
    (is (= 1 (cairo:pattern-reference-count pattern)))
    (is-false (cairo:pattern-destroy pattern))))

;;;     cairo_mesh_pattern_begin_patch
;;;     cairo_mesh_pattern_end_patch
;;;     cairo_mesh_pattern_move_to
;;;     cairo_mesh_pattern_line_to
;;;     cairo_mesh_pattern_curve_to

(test cairo-mesh-pattern-begin/end.1
  (let ((pattern (cairo:pattern-create-mesh)))

    ;; Add a Gouraud-shaded triangle
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 100 100)
    (cairo:mesh-pattern-line-to pattern 130 130)
    (cairo:mesh-pattern-line-to pattern 130  70)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)

    (cairo:mesh-pattern-end-patch pattern)

    (is (= 1 (cairo:mesh-pattern-patch-count pattern)))
    (is (equal '(:PATH
                 (:MOVE-TO 100.0d0 100.0d0)
                 (:CURVE-TO 110.0d0 110.0d0 120.0d0 120.0d0 130.0d0 130.0d0)
                 (:CURVE-TO 130.0d0 110.0d0 130.0d0 90.0d0 130.0d0 70.0d0)
                 (:CURVE-TO 120.0d0 80.0d0 110.0d0 90.0d0 100.0d0 100.0d0)
                 (:CURVE-TO 100.0d0 100.0d0 100.0d0 100.0d0 100.0d0 100.0d0))
               (cairo:mesh-pattern-path pattern 0)))

    (is-false (cairo:pattern-destroy pattern))
))

(test cairo-mesh-pattern-begin/end.2
  (let ((pattern (cairo:pattern-create-mesh)))

    ;; Add a Gouraud-shaded triangle
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 0 0)
    (cairo:mesh-pattern-curve-to pattern 30 -30 60 30 100 0)
    (cairo:mesh-pattern-curve-to pattern 60 30 130 60 100 100)
    (cairo:mesh-pattern-curve-to pattern 60 70 30 130 0 100)
    (cairo:mesh-pattern-curve-to pattern 30 70 -30 30 0 0)

    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 3 1 1 0)

    (cairo:mesh-pattern-end-patch pattern)

    (is (= 1 (cairo:mesh-pattern-patch-count pattern)))
    (is (equal '(:PATH
                 (:MOVE-TO 0.0d0 0.0d0)
                 (:CURVE-TO 30.0d0 -30.0d0 60.0d0 30.0d0 100.0d0 0.0d0)
                 (:CURVE-TO 60.0d0 30.0d0 130.0d0 60.0d0 100.0d0 100.0d0)
                 (:CURVE-TO 60.0d0 70.0d0 30.0d0 130.0d0 0.0d0 100.0d0)
                 (:CURVE-TO 30.0d0 70.0d0 -30.0d0 30.0d0 0.0d0 0.0d0))
               (cairo:mesh-pattern-path pattern 0)))

    (is-false (cairo:pattern-destroy pattern))
))

;;;     cairo_mesh_pattern_set_control_point
;;;     cairo_mesh_pattern_get_control_point

(test cairo-mesh-pattern-control-point

  (let ((pattern (cairo:pattern-create-mesh)))
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 100 100)
    (cairo:mesh-pattern-line-to pattern 130 130)
    (cairo:mesh-pattern-line-to pattern 130  70)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)

    (is-false (cairo:mesh-pattern-set-control-point pattern 0 110 100))
    (is-false (cairo:mesh-pattern-set-control-point pattern 1 120 120))
    (is-false (cairo:mesh-pattern-set-control-point pattern 2 90 40))
    (cairo:mesh-pattern-end-patch pattern)

    (is (eq :success (cairo:pattern-status pattern)))

    (is (equal '(110.0d0 100.d0)
               (multiple-value-list
                   (cairo:mesh-pattern-control-point pattern 0 0))))
    (is (equal '(120.0d0 120.0d0)
               (multiple-value-list
                   (cairo:mesh-pattern-control-point pattern 0 1))))
    (is (equal '(90.0d0 40.0d0)
               (multiple-value-list
                   (cairo:mesh-pattern-control-point pattern 0 2))))
))

;;;     cairo_mesh_pattern_set_corner_color_rgb
;;;     cairo_mesh_pattern_set_corner_color_rgba

;;;     cairo_mesh_pattern_get_corner_color_rgba

(test cairo-mesh-pattern-corner-color-rgba
  (let ((pattern (cairo:pattern-create-mesh)))

    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :mesh (cairo:pattern-type pattern)))

    ;; Draw a Coons pattern on the background of the drawing area
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 0 0)
    (cairo:mesh-pattern-curve-to pattern 30 -30 60 30 100 0)
    (cairo:mesh-pattern-curve-to pattern 60 30 130 60 100 100)
    (cairo:mesh-pattern-curve-to pattern 60 70 30 130 0 100)
    (cairo:mesh-pattern-curve-to pattern 30 70 -30 30 0 0)

    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 3 1 1 0)

    (cairo:mesh-pattern-end-patch pattern)

    (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (cairo:mesh-pattern-corner-color-rgba pattern 0 0))))
    (is (equal '(0.0d0 1.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (cairo:mesh-pattern-corner-color-rgba pattern 0 1))))
    (is (equal '(0.0d0 0.0d0 1.0d0 1.0d0)
               (multiple-value-list
                 (cairo:mesh-pattern-corner-color-rgba pattern 0 2))))
    (is (equal '(1.0d0 1.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (cairo:mesh-pattern-corner-color-rgba pattern 0 3))))
    ;; Invalid indizes
    (is-false (cairo:mesh-pattern-corner-color-rgba pattern 1 0))
    (is-false (cairo:mesh-pattern-corner-color-rgba pattern 0 4))
))

;;;     cairo_mesh_pattern_get_patch_count
;;;     cairo_mesh_pattern_get_path

(test cairo-mesh-pattern-path
  (let ((pattern (cairo:pattern-create-mesh)))

    (is (eq :success (cairo:pattern-status pattern)))
    (is (eq :mesh (cairo:pattern-type pattern)))

    ;; Draw a Coons pattern on the background of the drawing area
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 0 0)
    (cairo:mesh-pattern-curve-to pattern 30 -30 60 30 100 0)
    (cairo:mesh-pattern-curve-to pattern 60 30 130 60 100 100)
    (cairo:mesh-pattern-curve-to pattern 60 70 30 130 0 100)
    (cairo:mesh-pattern-curve-to pattern 30 70 -30 30 0 0)

    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 3 1 1 0)

    (cairo:mesh-pattern-end-patch pattern)

    (is (= 1 (cairo:mesh-pattern-patch-count pattern)))
    (is (equal '(:PATH
                 (:MOVE-TO 0.0d0 0.0d0)
                 (:CURVE-TO 30.0d0 -30.0d0 60.0d0 30.0d0 100.0d0 0.0d0)
                 (:CURVE-TO 60.0d0 30.0d0 130.0d0 60.0d0 100.0d0 100.0d0)
                 (:CURVE-TO 60.0d0 70.0d0 30.0d0 130.0d0 0.0d0 100.0d0)
                 (:CURVE-TO 30.0d0 70.0d0 -30.0d0 30.0d0 0.0d0 0.0d0))
               (cairo:mesh-pattern-path pattern 0)))
))

;;;     cairo_pattern_set_user_data
;;;     cairo_pattern_get_user_data

;;; 2024-2-2
