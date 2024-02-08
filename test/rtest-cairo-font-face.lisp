(in-package :cairo-test)

(def-suite cairo-font-face-suite :in cairo-suite)
(in-suite cairo-font-face-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_font_face_t
;;;     cairo_font_type_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_font_face_reference
;;;     cairo_font_face_get_reference_count
;;;     cairo_font_face_destroy

;; TODO: After the first run of the testsuite the reference count is no longer
;; has a the value 1, check this more carefully

(test cairo-font-face-reference/destroy
  (let (refcount)
    (cairo:with-toy-font-face (face "Sans" :normal :normal)

      (is (string= "Sans" (cairo:toy-font-face-family face)))
      (is (eq :normal (cairo:toy-font-face-slant face)))
      (is (eq :normal (cairo:toy-font-face-weight face)))

      (setf refcount (cairo:font-face-reference-count face))

      (is-false (cairo:font-face-reference nil))
      (is-false (cairo:font-face-reference (cffi:null-pointer)))
      (is (= refcount (cairo:font-face-reference-count face)))

      (is (cffi:pointer-eq face (cairo:font-face-reference face)))

      (is (= (+ 1 refcount) (cairo:font-face-reference-count face)))
      (is-false (cairo:font-face-destroy face))
      (is (= refcount (cairo:font-face-reference-count face))))))

;;;     cairo_font_face_status
;;;     cairo_font_face_get_type

(test cairo-font-face-status/type
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (is (eq :success (cairo:font-face-status face)))
    (is (eq :toy (cairo:font-face-type face)))))

;;;     cairo_font_face_set_user_data
;;;     cairo_font_face_get_user_data

;;; 2024-2-3
