(in-package :cairo-test)

(def-suite cairo-context :in cairo-suite)
(in-suite cairo-context)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_antialias_t
;;;     cairo_fill_rule_t
;;;     cairo_line_cap_t
;;;     cairo_line_join_t
;;;     cairo_operator_t

;;;     cairo_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_create

(test cairo-create.1
  (let* ((surface (cairo:image-surface-create :rgb24 100 150))
         (context (cairo:create surface)))
    ;; Check the surface
    (is (eq :success (cairo:surface-status surface)))
    (is (= 3 (cairo:surface-reference-count surface)))
    ;; Check the context
    (is (eq :success (cairo:status context)))
    (is (= 1 (cairo:reference-count context)))
    ;; Destroy the context
    (is-false (cairo:destroy context))
    (is (= 0 (cairo:reference-count context)))
    ;; Check the surface and destroy it
    (is (= 1 (cairo:surface-reference-count surface)))
    (is-false (cairo:surface-destroy surface))
    (is (= 0 (cairo:surface-reference-count surface)))))

(test cairo-create.2
  (cairo:with-image-surface (surface :rgb24 100 150)
    (let ((context (cairo:create surface)))
      ;; Check the surface
      (is (eq :success (cairo:surface-status surface)))
      (is (= 3 (cairo:surface-reference-count surface)))
      ;; Check the context
      (is (eq :success (cairo:status context)))
      (is (= 1 (cairo:reference-count context)))
      ;; Destroy the context
      (is-false (cairo:destroy context)))))

(test cairo-create.3
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      ;; Check the surface
      (is (eq :success (cairo:surface-status surface)))
      (is (= 3 (cairo:surface-reference-count surface)))
      ;; Check the context
      (is (eq :success (cairo:status context)))
      (is (= 1 (cairo:reference-count context))))))

;;;     cairo_reference
;;;     cairo_destroy

(test cairo-reference/destroy
  (cairo:with-image-surface (surface :rgb24 100 150)
    (let ((context (cairo:create surface)))
      (is (eq :success (cairo:status context)))
      (is (= 1 (cairo:reference-count context)))
      (is (cffi:pointerp (cairo:reference context)))
      (is (= 2 (cairo:reference-count context)))
      (is-false (cairo:destroy context))
      (is (= 1 (cairo:reference-count context)))
      (is-false (cairo:destroy context))
      (is (= 0 (cairo:reference-count context))))))

;;;     cairo_get_reference_count

(test cairo-reference-count
  (cairo:with-image-surface (surface :rgb24 100 150)
    (let ((context (cairo:create surface)))
      (is (= 1 (cairo:reference-count context)))
      (is (cffi:pointer-eq context (cairo:reference context)))
      (is (= 2 (cairo:reference-count context)))
      (is-false (cairo:destroy context))
      (is (= 1 (cairo:reference-count context)))
      (is-false (cairo:destroy context)))))

;;;     cairo_status

(test cairo-status
  (cairo:with-image-surface (surface :rgb24 150 100)
    (cairo:with-context (context surface)
      (is (eq :success (cairo:status context))))))

;;;   cairo_save
;;;   cairo_restore

(test cairo-save/restore
  (cairo:with-image-surface (surface :rgb24 150 100)
    (cairo:with-context (context surface)
      (is-false (cairo:save context))
      (is-false (cairo:restore context)))))

;;;   cairo_get_target

(test cairo-target
  (cairo:with-image-surface (surface :rgb24 150 100)
    (cairo:with-context (context surface)
      (is (eq :success (cairo:status context)))
      (is (= 3 (cairo:surface-reference-count surface)))
      (is (= 3 (cairo:surface-reference-count (cairo:target context))))
      (is (cffi:pointer-eq surface (cairo:target context))))))

;;;     cairo_push_group
;;;     cairo_push_group_with_content
;;;     cairo_pop_group
;;;     cairo_pop_group_to_source
;;;     cairo_get_group_target

;;;     cairo_set_source_rgb

(test cairo-set-source-rgb
  (cairo:with-context-for-recording-surface (context :color)
    (is (eq :success (cairo:status context)))
    (is (eq :success (cairo:pattern-status (cairo:source context))))
    (is (eq :solid (cairo:pattern-type (cairo:source context))))
    ;; Default source
    (is (equal '(0.0d0 0.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (cairo:pattern-rgba (cairo:source context)))))
    ;; Set new source
    (is-false (cairo:set-source-rgb context 0.5 1/2 1))
    (is (equal '(0.5d0 0.5d0 1.0d0 1.0d0)
               (multiple-value-list
                 (cairo:pattern-rgba (cairo:source context)))))))

;;;     cairo_set_source_rgba

(test cairo-set-source-rgba
  (cairo:with-context-for-recording-surface (context :color)
    (is (eq :success (cairo:status context)))
    (is (eq :success (cairo:pattern-status (cairo:source context))))
    (is (eq :solid (cairo:pattern-type (cairo:source context))))
    ;; Default source
    (is (equal '(0.0d0 0.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (cairo:pattern-rgba (cairo:source context)))))
    ;; Set new source
    (is-false (cairo:set-source-rgba context 0.5 1/2 1 0.1d0))
    (is (equal '(0.5d0 0.5d0 1.0d0 0.1d0)
               (multiple-value-list
                 (cairo:pattern-rgba (cairo:source context)))))))

;;;     cairo_get_source
;;;     cairo_set_source

(test cairo-source
  (cairo:with-context-for-recording-surface (context :color)
    (let ((pattern (cairo:pattern-create-rgb 1 1/2 0.5)))
      (is (cffi:pointer-eq pattern (setf (cairo:source context) pattern)))
      (is (cffi:pointer-eq pattern (cairo:source context)))
      (is (equal '(1.0d0 0.5d0 0.5d0 1.0d0)
                 (multiple-value-list
                   (cairo:pattern-rgba (cairo:source context)))))
      (is-false (cairo:pattern-destroy pattern)))))

;;;     cairo_set_source_surface

(test cairo-set-source-surface
  (cairo:with-context-for-recording-surface (context :color)
    (cairo:with-image-surface (surface :argb32 200 100)
      (is-false (cairo:set-source-surface context surface 200 100))
      (is (eq :surface (cairo:pattern-type (cairo:source context)))))))

;;;     cairo_set_antialias
;;;     cairo_get_antialias

(test cairo-antialias
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (eq :default (cairo:antialias context)))
      (is (eq :none (setf (cairo:antialias context) :none)))
      (is (eq :none (cairo:antialias context)))
      (is (eq :gray (setf (cairo:antialias context) :gray)))
      (is (eq :gray (cairo:antialias context)))
      (is (eq :subpixel (setf (cairo:antialias context) :subpixel)))
      (is (eq :subpixel (cairo:antialias context)))
      (is (eq :fast (setf (cairo:antialias context) :fast)))
      (is (eq :fast (cairo:antialias context)))
      (is (eq :good (setf (cairo:antialias context) :good)))
      (is (eq :good (cairo:antialias context)))
      (is (eq :best (setf (cairo:antialias context) :best)))
      (is (eq :best (cairo:antialias context))))))

;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash

(test cairo-dash
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      ;; Get default values
      (is (equal '(() 0.0d0)
                  (multiple-value-list (cairo:dash context))))
      (is (= 0 (cairo:dash-count context)))
      ;; Set dashes
      (is (equal '(1.0 2.0 3.0 4.0)
                 (setf (cairo:dash context 3) '(1.0 2.0 3.0 4.0))))
      (is (equal '((1.0d0 2.0d0 3.0d0 4.0d0) 3.0d0)
                 (multiple-value-list (cairo:dash context))))
      (is (= 4 (cairo:dash-count context)))
      ;; Clear dashes
      (is (equal '()
                 (setf (cairo:dash context 0) '())))
      (is (equal '(() 0.0d0)
                 (multiple-value-list (cairo:dash context))))
      (is (= 0 (cairo:dash-count context)))
      ;; Set an invalid dash
      (is (equal '(1.0 -2.0)
                 (setf (cairo:dash context 0) '(1.0 -2.0))))
      (is (eq :invalid-dash (cairo:status context))))))

;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule

(test cairo-fill-rule
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (eq :winding (cairo:fill-rule context)))
      (is (eq :even-odd (setf (cairo:fill-rule context) :even-odd)))
      (is (eq :even-odd (cairo:fill-rule context))))))

;;;     cairo_set_line_cap
;;;     cairo_get_line_cap

(test cairo-line-cap
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (eq :butt (cairo:line-cap context)))
      (is (eq :round (setf (cairo:line-cap context) :round)))
      (is (eq :round (cairo:line-cap context)))
      (is (eq :square (setf (cairo:line-cap context) :square)))
      (is (eq :square (cairo:line-cap context))))))

;;;     cairo_set_line_join
;;;     cairo_get_line_join

(test cairo-line-join
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (eq :miter (cairo:line-join context)))
      (is (eq :round (setf (cairo:line-join context) :round)))
      (is (eq :round (cairo:line-join context)))
      (is (eq :bevel (setf (cairo:line-join context) :bevel)))
      (is (eq :bevel (cairo:line-join context))))))

;;;     cairo_set_line_width
;;;     cairo_get_line_width

(test cairo-line-width.1
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (= 2.0d0 (cairo:line-width context)))
      (is (= 1.0d0 (setf (cairo:line-width context) 1.0)))
      (is (= 1.0d0 (cairo:line-width context))))))

(test cairo-line-width.2
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (= 0.5d0 (setf (cairo:line-width context) 1/2)))
      (is (= 0.5d0 (cairo:line-width context)))
      (is (= 1.0d0 (setf (cairo:line-width context) 1)))
      (is (= 1.0d0 (cairo:line-width context)))
      (is (= 2.0d0 (setf (cairo:line-width context) 2.0)))
      (is (= 2.0d0 (cairo:line-width context)))
      (is (= 3.0d0 (setf (cairo:line-width context) 3.0d0)))
      (is (= 3.0d0 (cairo:line-width context))))))

;;;     cairo_set_hairline                                 Since 1.18
;;;     cairo_get_hairline                                 Since 1.18

(test cairo-hairline
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is-false (cairo:hairline context))
      (is-true (setf (cairo:hairline context) t))
      (is-true (cairo:hairline context)))))

;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit

(test cairo-miter-limit
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (= 10.0d0 (cairo:miter-limit context)))
      (is (= 15.0d0 (setf (cairo:miter-limit context) 15.0)))
      (is (= 15.0d0 (cairo:miter-limit context))))))

;;;     cairo_set_operator
;;;     cairo_get_operator

(test cairo-operator
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (eq :over (cairo:operator context)))
      (is (eq :clear (setf (cairo:operator context) :clear)))
      (is (eq :clear (cairo:operator context))))))

;;;     cairo_set_tolerance
;;;     cairo_get_tolerance

(test cairo-tolerance
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
      (is (= 0.1d0 (cairo:tolerance context)))
      (is (approx-equal 0.2d0 (setf (cairo:tolerance context) 0.2)))
      (is (approx-equal 0.2d0 (cairo:tolerance context))))))

;;;     cairo_clip
;;;     cairo_clip_preserve
;;;     cairo_clip_extents
;;;     cairo_in_clip
;;;     cairo_reset_clip

(test cairo-clip-extents
  (cairo:with-context-for-recording-surface (context :color 0 0 200 300)
    (is-false (cairo:rectangle context 0 0 100 200))
    (is-false (cairo:clip context))
    (is (equal '(0.0d0 0.0d0 100.0d0 200.0d0)
               (multiple-value-list (cairo:clip-extents context))))
    (is-true (cairo:in-clip context 50 50))
    (is-false (cairo:in-clip context 1000 1000))
    (is-false (cairo:reset-clip context))
    (is (equal '(0.0d0 0.0d0 200.0d0 300.0d0)
               (multiple-value-list (cairo:clip-extents context))))))

;;;     cairo_rectangle_list_destroy
;;;     cairo_copy_clip_rectangle_list

(test cairo-copy-clip-rectangle-list.1
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:save context)
      (cairo:rectangle context 10 10 70 70)
      (cairo:clip context)
      (is (equal '(10.0d0 10.0d0 80.d0 80.d0)
                 (multiple-value-list (cairo:clip-extents context))))
      (is (equal '((10.0d0 10.0d0 70.0d0 70.0d0))
                 (cairo:copy-clip-rectangle-list context)))
      (let (rectlist)
        (is (cffi:pointerp (setf rectlist
                                 (cairo::%copy-clip-rectangle-list context))))
        (is (eq :success
                (cffi:foreign-slot-value rectlist
                                         '(:struct cairo::rectangle-list-t)
                                         'cairo::status)))
        (is (cffi:pointerp
                (cffi:foreign-slot-value rectlist
                                         '(:struct cairo::rectangle-list-t)
                                         'cairo::rectangles)))
        (is (= 1
               (cffi:foreign-slot-value rectlist
                                        '(:struct cairo::rectangle-list-t)
                                        'cairo::num-rectangles)))
        (let (rect)
          (setf rect (cffi:foreign-slot-value rectlist
                                              '(:struct cairo::rectangle-list-t)
                                              'cairo::rectangles))
          (is (= 10.0d0 (cffi:foreign-slot-value rect
                                                 '(:struct cairo::rectangle-t)
                                                 'cairo::x)))
          (is (= 10.0d0 (cffi:foreign-slot-value rect
                                                 '(:struct cairo::rectangle-t)
                                                 'cairo::y)))
          (is (= 70.0d0 (cffi:foreign-slot-value rect
                                                 '(:struct cairo::rectangle-t)
                                                 'cairo::width)))
          (is (= 70.0d0 (cffi:foreign-slot-value rect
                                                 '(:struct cairo::rectangle-t)
                                                 'cairo::height))))))))

;; Test the example from the documentation
(test cairo-copy-clip-rectangle-list.2
  (is (equal '((10.0d0 10.0d0 15.0d0 10.0d0)
               (10.0d0 20.0d0 20.0d0 5.0d0)
               (20.0d0 25.0d0 10.0d0 5.0d0))
             (cairo:with-recording-surface (surface :color)
               (cairo:with-context (context surface)
                 (cairo:rectangle context 10 10 15 15)
                 (cairo:rectangle context 20 20 10 10)
                 (cairo:clip context)
                 (cairo:copy-clip-rectangle-list context))))))

;;;     cairo_fill
;;;     cairo_fill_preserve
;;;     cairo_fill_extents
;;;     cairo_in_fill
;;;     cairo_mask
;;;     cairo_mask_surface
;;;     cairo_paint
;;;     cairo_paint_with_alpha
;;;     cairo_stroke
;;;     cairo_stroke_preserve
;;;     cairo_stroke_extents
;;;     cairo_in_stroke
;;;     cairo_copy_page
;;;     cairo_show_page

;;;     cairo_set_user_data
;;;     cairo_get_user_data

;;; 2024-1-25
