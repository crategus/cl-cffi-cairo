(in-package :cairo-test)

(def-suite cairo-path :in cairo-suite)
(in-suite cairo-path)

;;; Types and Values

;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t

;;; Functions

;;;     cairo_copy_path

;; TODO: More work is needed to show the code for iterating through the
;; structures for a path

#+nil
(test path-copy
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)

      (is-false (cairo:new-path context))
      (is-false (cairo:move-to context 10 10))
      (is-false (cairo:line-to context 20 20))
      (is-false (cairo:close-path context))

      (let* ((path-list nil)
             (path (cairo:path-copy context))
             (status (cairo:path-status path))
             (data (cairo:path-data path))
             (num-data (cairo:path-num-data path)))

        (is (eq :success status))
        (is (cffi:pointerp data))
        (is (= 7 num-data))

        (loop for count from 0 below num-data
              for ptr = (cffi:mem-aptr data :pointer count)
              for header = (cairo:path-data-header ptr)
              for type = (cairo:path-data-header-data-type header)
              for length = (cairo:path-data-header-length header)
              for point = (cairo:path-data-point ptr)

              do (format t "~%  count : ~a~%" count)
                 (format t "~&    ptr : ~a~%" ptr)
                 (format t "~& header : ~a~%" header)
                 (format t "~&   type : ~a~%" type)
                 (format t "~& length : ~a~%" length)
                 (format t "~&  point : ~a~%" point)
)))))

#|

      (loop for count from 0 below num-data
            for header = (foreign-slot-pointer path
                                               '(:struct cairo:path-data-t)
                                               'cairo::header)
            for data-type = (cffi:foreign-slot-value header
                                               '(:struct cairo::header-t)
                                               'cairo::data-type)
            for length = (cffi:foreign-slot-value header
                                             '(:struct cairo::header-t)
                                              'cairo::length)
            do (format t "~%     count : ~a~%" count)
               (format t "~&    header : ~a~%" header)
               (format t "~& data-type : ~a~%" data-type)
               (format t "~&    length : ~a~%" length)
               (format t "~&    offset : ~a~%" (* length (cffi:foreign-type-size :pointer)))


               (incf-pointer data (* length (cffi:foreign-type-size :pointer)))


      )
)))
|#

#+nil
(test copy-path.1
  (let* ((surface (cairo:image-surface-create :rgb24 100 150))
         (context (cairo:context-create surface)))

    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 10))
    (is-false (cairo:line-to context 20 20))
    (is-false (cairo:close-path context))

    (let* ((path-list nil)
           (path (cairo:copy-path context))
           (data (cffi:foreign-slot-value path
                                          '(:struct cairo:path-t)
                                          'cairo::data))
           (num-data (cffi:foreign-slot-value path
                                              '(:struct cairo:path-t)
                                              'cairo::num-data)))

      (is (eq :success
              (cffi:foreign-slot-value path '(:struct cairo:path-t)
                                            'cairo::status)))
      (is (cffi:pointerp
            (cffi:foreign-slot-value path '(:struct cairo:path-t) 'cairo::data)))
      (is (= 7
             (cffi:foreign-slot-value path '(:struct cairo:path-t)
                                           'cairo::num-data)))

      (dotimes (count num-data)
        (let* ((path-data (cffi:mem-aptr data
                                         '(:struct cairo:path-data-t)
                                         count))
               (header (foreign-slot-pointer path-data
                                             '(:struct cairo:path-data-t)
                                             'cairo::header))
               (points (foreign-slot-pointer path-data
                                             '(:struct cairo:path-data-t)
                                             'cairo::point)))

          (is (cffi:pointerp (cffi:mem-aptr data
                                            '(:struct cairo:path-data-t) count)))

          (is (cffi:pointerp header))
          (is (cffi:pointerp points))

          (let ((data-type (cffi:foreign-slot-value header
                                               '(:struct cairo::header-t)
                                               'cairo::data-type))
                (length (cffi:foreign-slot-value header
                                            '(:struct cairo::header-t)
                                            'cairo::length)))

          (format t "~& ~a ~a~%" data-type length)

          (push (cffi:foreign-slot-value header
                                        '(:struct cairo::header-t)
                                        'cairo::data-type)
                          path-list)

          (push (cffi:foreign-slot-value header
                                        '(:struct cairo::header-t)
                                        'cairo::length)
                          path-list)

          (setf length (if (< length 10) length 10))

          (dotimes (i length)
            (let* ((point-ptr (cffi:mem-aptr points
                                             '(:struct cairo::point-t) i)))

              (is (cffi:pointerp point-ptr))

              (push (cffi:foreign-slot-value point-ptr
                                            '(:struct cairo::point-t)
                                            'cairo::x)
                    path-list)
              (push (cffi:foreign-slot-value point-ptr
                                            '(:struct cairo::point-t)
                                            'cairo::y)
                    path-list)


          ))


      )))

      (is (equal '() (reverse path-list)))

)))

;;;     cairo_copy_path_flat
;;;     cairo_path_destroy
;;;     cairo_append_path

;;;     cairo_has_current_point
;;;     cairo_get_current_point

(test current-point
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is-false (cairo:new-path context))
      (is-false (cairo:move-to context 10 10))
      (is-true (cairo:has-current-point context))
      (is (equal '(10.0d0 10.0d0)
                  (multiple-value-list (cairo:current-point context))))
      (is-false (cairo:line-to context 20 20))
      (is-true (cairo:has-current-point context))
      (is (equal '(20.0d0 20.0d0)
                  (multiple-value-list (cairo:current-point context)))))))

;;;     cairo_new_path
;;;     cairo_new_sub_path
;;;     cairo_close_path
;;;     cairo_arc
;;;     cairo_arc_negative
;;;     cairo_curve_to
;;;     cairo_line_to
;;;     cairo_move_to
;;;     cairo_rectangle
;;;     cairo_glyph_path
;;;     cairo_text_path
;;;     cairo_rel_curve_to
;;;     cairo_rel_line_to
;;;     cairo_rel_move_to
;;;     cairo_path_extents

;;; --- 2023-1-3 ---------------------------------------------------------------
