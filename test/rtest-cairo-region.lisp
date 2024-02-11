(in-package :cairo-test)

(def-suite cairo-region-suite :in cairo-suite)
(in-suite cairo-region-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_region_t
;;;     cairo_region_overlap_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_region_create

(test cairo-region-create
  (let ((region (cairo:region-create)))
    (is (eq :success (cairo:region-status region)))
    (is (= 0 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 0 0) (cairo:region-extents region)))
    (is-true (cairo:region-is-empty region))
    (is-false (cairo:region-destroy region))))

;;;     cairo_region_create_rectangle

(test cairo-region-create-rectangle
  (let ((region (cairo:region-create-rectangle 0 0 200 100)))
    (is (eq :success (cairo:region-status region)))
    (is (= 1 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 200 100) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))
    (is-false (cairo:region-destroy region))))

;;;     cairo_region_create_rectangles

(test cairo-region-create-rectangles
  (let ((region (cairo:region-create-rectangles '(0 0 1 1) '(1 1 2 2)
                                                '(3 3 3 3) '(6 6 4 4))))
    (is (eq :success (cairo:region-status region)))
    (is (= 4 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 10 10) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))
    (is-false (cairo:region-destroy region))))

;;;     cairo_region_copy
;;;     cairo_region_equal

(test cairo-region-copy/equal
  (let* ((region1 (cairo:region-create-rectangles '(0 0 1 1) '(1 1 2 2)
                                                  '(3 3 3 3) '(6 6 4 4)))
         (region2 (cairo:region-copy region1)))
    (is (eq :success (cairo:region-status region2)))
    (is (= 4 (cairo:region-num-rectangles region2)))
    (is (equal '(0 0 10 10) (cairo:region-extents region2)))
    (is-false (cairo:region-is-empty region2))

    (is (cairo:region-equal region1 region2))

    (is-false (cairo:region-destroy region1))
    (is-false (cairo:region-destroy region2))))

;;;     cairo_region_reference
;;;     cairo_region_destroy
;;;     cairo_region_status
;;;     cairo_region_get_extents
;;;     cairo_region_num_rectangles

;;;     cairo_region_get_rectangle

(test cairo-region-rectangle
  (let ((region (cairo:region-create-rectangles '(0 0 1 1) '(1 1 2 2)
                                                '(3 3 3 3) '(6 6 4 4))))
    (is (eq :success (cairo:region-status region)))
    (is (= 4 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 10 10) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))

    (is (equal '(0 0 1 1) (cairo:region-rectangle region 0)))
    (is (equal '(1 1 2 2) (cairo:region-rectangle region 1)))
    (is (equal '(3 3 3 3) (cairo:region-rectangle region 2)))
    (is (equal '(6 6 4 4) (cairo:region-rectangle region 3)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_is_empty

;;;     cairo_region_contains_point

(test cairo-region-contains-point
  (let ((region (cairo:region-create-rectangles '(0 0 1 1) '(1 1 2 2)
                                                '(3 3 3 3) '(6 6 4 4))))
    (is (eq :success (cairo:region-status region)))
    (is (= 4 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 10 10) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))

    (is-true (cairo:region-contains-point region 0 0))
    (is-true (cairo:region-contains-point region 1 1))
    (is-true (cairo:region-contains-point region 1 2))
    (is-true (cairo:region-contains-point region 9 9))

    (is-false (cairo:region-contains-point region 10 10))
    (is-false (cairo:region-contains-point region 0 1))
    (is-false (cairo:region-contains-point region 1 3))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_contains_rectangle

(test cairo-region-contains-rectangle
  (let ((region (cairo:region-create-rectangles '(0 0 1 1) '(1 1 2 2)
                                                '(3 3 3 3) '(6 6 4 4))))
    (is (eq :success (cairo:region-status region)))
    (is (= 4 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 10 10) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))

    (is (eq :in (cairo:region-contains-rectangle region 0 0 1 1)))
    (is (eq :in (cairo:region-contains-rectangle region 1 1 2 2)))
    (is (eq :in (cairo:region-contains-rectangle region 3 3 3 3)))
    (is (eq :in (cairo:region-contains-rectangle region 6 6 4 4)))

    (is (eq :part (cairo:region-contains-rectangle region 0 0 1 2)))
    (is (eq :part (cairo:region-contains-rectangle region 0 0 2 1)))

    (is (eq :out (cairo:region-contains-rectangle region 0 1 1 1)))
    (is (eq :out (cairo:region-contains-rectangle region 1 0 2 1)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_translate

(test cairo-region-translate
  (let ((region (cairo:region-create-rectangle 0 0 200 100)))
    (is (eq :success (cairo:region-status region)))
    (is (= 1 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 200 100) (cairo:region-extents region)))
    (is-false (cairo:region-is-empty region))

    (is-false (cairo:region-translate region 10 20))
    (is (equal '(10 20 200 100) (cairo:region-extents region)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_intersect

(test cairo-region-intersect
  (let ((region1 (cairo:region-create-rectangle 0 0 100 100))
        (region2 (cairo:region-create-rectangle 50 50 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (cffi:pointer-eq region1
                         (cairo:region-intersect region1 region2)))

    (is (equal '(50 50 50 50) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is-false (cairo:region-destroy region1))
    (is-false (cairo:region-destroy region2))))

;;;     cairo_region_intersect_rectangle

(test cairo-region-intersect-rectangle
  (let ((region (cairo:region-create-rectangle 0 0 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region)))

    (is (cffi:pointer-eq region
                         (cairo:region-intersect-rectangle region
                                                           50 50 100 100)))

    (is (equal '(50 50 50 50) (cairo:region-extents region)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_subtract

(test cairo-region-subtract
  (let ((region1 (cairo:region-create-rectangle 0 0 100 100))
        (region2 (cairo:region-create-rectangle 50 50 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (cffi:pointer-eq region1
                         (cairo:region-subtract region1 region2)))

    (is (equal '(0 0 100 100) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (= 2 (cairo:region-num-rectangles region1)))
    (is (equal '(0 0 100 50) (cairo:region-rectangle region1 0)))
    (is (equal '(0 50 50 50) (cairo:region-rectangle region1 1)))

    (is-false (cairo:region-destroy region1))
    (is-false (cairo:region-destroy region2))))

;;;     cairo_region_subtract_rectangle

(test cairo-region-subtract-rectangle
  (let ((region (cairo:region-create-rectangle 0 0 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region)))

    (is (cffi:pointer-eq region
                         (cairo:region-subtract-rectangle region
                                                          50 50 100 100)))

    (is (equal '(0 0 100 100) (cairo:region-extents region)))

    (is (= 2 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 100 50) (cairo:region-rectangle region 0)))
    (is (equal '(0 50 50 50) (cairo:region-rectangle region 1)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_union

(test cairo-region-union
  (let ((region1 (cairo:region-create-rectangle 0 0 100 100))
        (region2 (cairo:region-create-rectangle 50 50 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (cffi:pointer-eq region1
                         (cairo:region-union region1 region2)))

    (is (equal '(0 0 150 150) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (= 3 (cairo:region-num-rectangles region1)))
    (is (equal '(0 0 100 50) (cairo:region-rectangle region1 0)))
    (is (equal '(0 50 150 50) (cairo:region-rectangle region1 1)))
    (is (equal '(50 100 100 50) (cairo:region-rectangle region1 2)))

    (is-false (cairo:region-destroy region1))
    (is-false (cairo:region-destroy region2))))

;;;     cairo_region_union_rectangle

(test cairo-region-union-rectangle
  (let ((region (cairo:region-create-rectangle 0 0 100 100)))
    (is (equal '(0 0 100 100) (cairo:region-extents region)))

    (is (cffi:pointer-eq region
                         (cairo:region-union-rectangle region
                                                       50 50 100 100)))

    (is (equal '(0 0 150 150) (cairo:region-extents region)))

    (is (= 3 (cairo:region-num-rectangles region)))
    (is (equal '(0 0 100 50) (cairo:region-rectangle region 0)))
    (is (equal '(0 50 150 50) (cairo:region-rectangle region 1)))
    (is (equal '(50 100 100 50) (cairo:region-rectangle region 2)))

    (is-false (cairo:region-destroy region))))

;;;     cairo_region_xor

(test cairo-region-xor
  (let ((region1 (cairo:region-create-rectangle 0 0 100 100))
        (region2 (cairo:region-create-rectangle 50 50 100 100)))

    (is (equal '(0 0 100 100) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (cffi:pointer-eq region1
                         (cairo:region-xor region1 region2)))

    (is (equal '(0 0 150 150) (cairo:region-extents region1)))
    (is (equal '(50 50 100 100) (cairo:region-extents region2)))

    (is (= 4 (cairo:region-num-rectangles region1)))
    (is (equal '(0 0 100 50) (cairo:region-rectangle region1 0)))
    (is (equal '(0 50 50 50) (cairo:region-rectangle region1 1)))
    (is (equal '(100 50 50 50) (cairo:region-rectangle region1 2)))
    (is (equal '(50 100 100 50) (cairo:region-rectangle region1 3)))

    (is-false (cairo:region-destroy region1))
    (is-false (cairo:region-destroy region2))))

;;;     cairo_region_xor_rectangle

;;; 2024-2-10
