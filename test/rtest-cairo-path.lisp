(in-package :cairo-test)

(def-suite cairo-path :in cairo-suite)
(in-suite cairo-path)

(defvar *verbose-cairo-path* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_path_data_type_t

;;;     cairo_path_t
;;;     cairo_path_data_t

(test cairo-path-structure.1
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 10))
    (is-false (cairo:line-to context 20 20))
    (is-false (cairo:close-path context))
    (let* ((path (cairo:copy-path context))
           (status (cairo:path-status path))
           (data (cairo:path-data path))
           (numdata (cairo:path-numdata path)))
      ;; Get the data array
      (is (eq :success status))
      (is (cffi:pointerp data))
      (is (= 7 numdata))
      (when *verbose-cairo-path*
        (format t "~%")
        (format t "          path : ~a~%" path)
        (format t "      data-arr : ~a~%" data)
        (format t "       numdata : ~a~%" numdata))
      ;; First element
      (setf data (cairo:path-data path))
      (is (eq :move-to (cairo::header-data-type data)))
      (is (= 2 (cairo::header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo::header-data-type data))
        (format t " header-length : ~a~%" (cairo::header-length data)))
      ;; Second element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (= 10.0 (cairo::point-x data)))
      (is (= 10.0 (cairo::point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo::point-x data))
        (format t "       point-y : ~a~%" (cairo::point-y data)))
      ;; Third element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (eq :line-to (cairo::header-data-type data)))
      (is (= 2 (cairo::header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo::header-data-type data))
        (format t " header-length : ~a~%" (cairo::header-length data)))
      ;; Fourth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (= 20.0 (cairo::point-x data)))
      (is (= 20.0 (cairo::point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo::point-x data))
        (format t "       point-y : ~a~%" (cairo::point-y data)))
      ;; Fifth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (eq :close-path (cairo::header-data-type data)))
      (is (= 1 (cairo::header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo::header-data-type data))
        (format t " header-length : ~a~%" (cairo::header-length data)))
      ;; Sixth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (eq :move-to (cairo::header-data-type data)))
      (is (= 2 (cairo::header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo::header-data-type data))
        (format t " header-length : ~a~%" (cairo::header-length data)))
      ;; Seventh element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo::path-data-t))))
      (is (= 10.0 (cairo::point-x data)))
      (is (= 10.0 (cairo::point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo::point-x data))
        (format t "       point-y : ~a~%" (cairo::point-y data)))
      (is-false (cairo:path-destroy path)))))

(test cairo-path-structure.2
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 10))
    (is-false (cairo:line-to context 20 20))
    (is-false (cairo:close-path context))
    (let* ((path (cairo:copy-path context))
           (numdata (cairo:path-numdata path)))
      (when *verbose-cairo-path*
        (format t "  path : ~a~%" path)
        (format t "   num : ~a~%" numdata))
      (loop with count = 0
            with element = nil
            with data = (cairo:path-data path)
            with size = (cffi:foreign-type-size '(:struct cairo::path-data-t))
            collect element
            while (< count numdata)
            do (cond ((eq :move-to (cairo::header-data-type data))
                      (setf element (list :move-to))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo::header-data-type data))
                        (format t "length : ~a~%" (cairo::header-length data)))
                      (setf count (incf count (cairo::header-length data)))
                      (setf data (cffi:inc-pointer data size))
                      (when *verbose-cairo-path*
                        (format t " x : ~a~%" (cairo::point-x data))
                        (format t " y : ~a~%" (cairo::point-y data)))
                      (setf data (cffi:inc-pointer data size)))
                     ((eq :line-to (cairo::header-data-type data))
                      (setf element (list :line-to))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo::header-data-type data))
                        (format t "length : ~a~%" (cairo::header-length data)))
                      (setf count (incf count (cairo::header-length data)))
                      (setf data (cffi:inc-pointer data size))
                      (when *verbose-cairo-path*
                        (format t " x : ~a~%" (cairo::point-x data))
                        (format t " y : ~a~%" (cairo::point-y data)))
                      (setf data (cffi:inc-pointer data size)))
                     ((eq :close-path (cairo::header-data-type data))
                      (setf element (list :close-path))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo::header-data-type data))
                        (format t "length : ~a~%" (cairo::header-length data)))
                      (setf count (incf count (cairo::header-length data)))
                      (setf data (cffi:inc-pointer data size)))
                     (t (error "KEYWORD not known to PATH-DATA-TYPE-T"))))
      (is-false (cairo:path-destroy path)))))

;;; --- Functions --------------------------------------------------------------

;;;     cairo_copy_path
;;;     cairo_copy_path_flat
;;;     cairo_path_destroy

(test cairo-copy-path
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 20))
    (is-false (cairo:line-to context 30 40))
    (is-false (cairo:curve-to context 50 60 70 80 90 100))
    (is-false (cairo:close-path context))
    (let ((path (cairo:copy-path context)))
      (is (equal '(:PATH (:MOVE-TO 10.0d0 20.0d0)
                         (:LINE-TO 30.0d0 40.0d0)
                         (:CURVE-TO 50.0d0 60.0d0 70.0d0 80.0d0 90.0d0 100.0d0)
                         (:CLOSE-PATH) (:MOVE-TO 10.0d0 20.0d0))
                 (cairo:path-data-to-list path)))
    (is-false (cairo:path-destroy path)))))

(test cairo-copy-path-flat
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 20))
    (is-false (cairo:line-to context 30 40))
    (is-false (cairo:curve-to context 50 60 70 80 90 100))
    (is-false (cairo:close-path context))
    (let ((path (cairo:copy-path-flat context)))
      (is (equal '(:PATH (:MOVE-TO 10.0d0 20.0d0)
                         (:LINE-TO 30.0d0 40.0d0)
                         (:LINE-TO 90.0d0 100.0d0)
                         (:CLOSE-PATH)
                         (:MOVE-TO 10.0d0 20.0d0))
                 (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path)))))

;;;     cairo_append_path

(test cairo-append-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
    ;; Create a path
    (is-false (cairo:new-path context))
    (is-false (cairo:move-to context 10 20))
    (is-false (cairo:line-to context 30 40))
    (is-false (cairo:close-path context))
    ;; Copy the path
    (let ((path (cairo:copy-path-flat context)))
      (is (equal '(:PATH
                   (:MOVE-TO 10.0d0 20.0d0)
                   (:LINE-TO 30.0d0 40.0d0)
                   (:CLOSE-PATH)
                   (:MOVE-TO 10.0d0 20.0d0))
                 (cairo:path-data-to-list path)))
      (is-false (cairo:append-path context path))
      (is-false (cairo:path-destroy path)))
    ;; Append the path
    (let ((path (cairo:copy-path-flat context)))
      (is (equal '(:PATH
                   (:MOVE-TO 10.0d0 20.0d0)
                   (:LINE-TO 30.0d0 40.0d0)
                   (:CLOSE-PATH)
                   (:MOVE-TO 10.0d0 20.0d0)
                   (:LINE-TO 30.0d0 40.0d0)
                   (:CLOSE-PATH)
                   (:MOVE-TO 10.0d0 20.0d0))
                 (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_has_current_point
;;;     cairo_get_current_point

(test cairo-current-point
  (cairo:with-image-surface (surface :rgb24 100 150)
    (cairo:with-context (context surface)
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

(test cairo-new-path/sub-path/close-path.1
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (is-false (cairo:move-to context 10 20))
      (is-false (cairo:line-to context 30 40))
      (is-false (cairo:line-to context 50 60))
      (is-false (cairo:close-path context))
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 20.0d0)
                     (:LINE-TO 50.0d0 60.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 10.0d0 20.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

(test cairo-new-path/sub-path/close-path.2
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (is-false (cairo:move-to context 10 20))
      (is-false (cairo:line-to context 30 40))

      (is-false (cairo:new-sub-path context))
      (is-false (cairo:move-to context 0 0))
      (is-false (cairo:line-to context 5 0))
      (is-false (cairo:close-path context))

      (is-false (cairo:line-to context 50 60))
      (is-false (cairo:close-path context))
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 20.0d0)
                     (:LINE-TO 30.0d0 40.0d0)
                     (:MOVE-TO 0.0d0 0.0d0)
                     (:LINE-TO 5.0d0 0.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 0.0d0 0.0d0)
                     (:LINE-TO 50.0d0 60.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 0.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

(test cairo-new-path/sub-path/close-path.2
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (is-false (cairo:move-to context 10 20))
      (is-false (cairo:line-to context 30 40))

      (is-false (cairo:new-sub-path context))
      (is-false (cairo:move-to context 0 0))
      (is-false (cairo:line-to context 5 0))

      (is-false (cairo:line-to context 50 60))
      (is-false (cairo:close-path context))
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 20.0d0)
                     (:LINE-TO 30.0d0 40.0d0)
                     (:MOVE-TO 0.0d0 0.0d0)
                     (:LINE-TO 5.0d0 0.0d0)
                     (:LINE-TO 50.0d0 60.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 0.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_path_extents

(test cairo-path-extents
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:rectangle context 0 0 10 20)
      (cairo:close-path context)
      (is (equal '(0.0d0 0.0d0 10.0d0 20.0d0)
                 (multiple-value-list (cairo:path-extents context))))

      (cairo:new-path context)
      (cairo:move-to context 5 -5)
      (cairo:line-to context 20 10)
      (cairo:close-path context)
      (is (equal '(5.0d0 -5.0d0 20.0d0 10.0d0)
                 (multiple-value-list (cairo:path-extents context)))))))

;;;     cairo_move_to
;;;     cairo_rel_move_to

(test cairo-move-to/rel-move-to
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:move-to context 10 20)
      (cairo:line-to context 5 10)
      (cairo:rel-move-to context -10 20)
      (cairo:close-path context)
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 20.0d0)
                     (:LINE-TO 5.0d0 10.0d0)
                     (:MOVE-TO -5.0d0 30.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO -5.0d0 30.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_line_to
;;;     cairo_rel_line_to

(test cairo-line-to/rel-line-to
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:move-to context 0 0)
      (cairo:line-to context 10 20)
      (cairo:rel-line-to context -5 10)
      (cairo:close-path context)
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 0.0d0 0.0d0)
                     (:LINE-TO 10.0d0 20.0d0)
                     (:LINE-TO 5.0d0 30.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 0.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_curve_to
;;;     cairo_rel_curve_to

(test cairo-curve-to
  (let (;; Coordinates of the curve
        (x   25.6) (y  128.0)
        (x1 102.4) (y1 230.4)
        (x2 153.6) (y2  25.6)
        (x3 230.4) (y3 128.0))
    (cairo:with-recording-surface (surface :color)
      (cairo:with-context (context surface)
        (cairo:new-path context)
        (cairo:move-to context x y)
        (cairo:curve-to context x1 y1 x2 y2 x3 y3)
        (cairo:close-path context)
        (let ((path (cairo:copy-path context)))
          (is (equal '(:PATH
                       (:MOVE-TO 25.6015625d0 128.0d0)
                       (:CURVE-TO 102.3984375d0 230.3984375d0
                                  153.6015625d0 25.6015625d0
                                  230.3984375d0 128.0d0)
                       (:CLOSE-PATH)
                       (:MOVE-TO 25.6015625d0 128.0d0))
                     (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path)))))))

(test cairo-rel-curve-to
  (let (;; Coordinates of the curve
        (x   25.6) (y  128.0)
        (x1 102.4) (y1 230.4)
        (x2 153.6) (y2  25.6)
        (x3 230.4) (y3 128.0))
    (cairo:with-recording-surface (surface :color)
      (cairo:with-context (context surface)
        (cairo:new-path context)
        (cairo:move-to context x y)
        (cairo:rel-curve-to context (- x1 x) (- y1 y)
                                    (- x2 x) (- y2 y)
                                    (- x3 x) (- y3 y))
        (cairo:close-path context)
        (let ((path (cairo:copy-path context)))
          (is (equal '(:PATH
                       (:MOVE-TO 25.6015625d0 128.0d0)
                       (:CURVE-TO 102.40234375d0 230.3984375d0
                                  153.6015625d0 25.6015625d0
                                  230.40234375d0 128.0d0)
                       (:CLOSE-PATH)
                       (:MOVE-TO 25.6015625d0 128.0d0))
                     (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path)))))))

;;;     cairo_rectangle

(test cairo-rectangle
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:rectangle context  10 20 30 40)
      (let ((path (cairo:copy-path-flat context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 20.0d0)
                     (:LINE-TO 40.0d0 20.0d0)
                     (:LINE-TO 40.0d0 60.0d0)
                     (:LINE-TO 10.0d0 60.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 10.0d0 20.0d0))
                   (cairo:path-data-to-list path)))
        (is-false (cairo:path-destroy path))))))

#+nil
(cairo:with-recording-surface (surface :color)
  (cairo:with-context (context surface)
    (cairo:new-path context)
    (cairo:rectangle context  10 20 30 40)
    (let ((path (cairo:copy-path-flat context)))
      (prog1
        (cairo:path-data-to-list path)
        (cairo:path-destroy path)))))

;;;     cairo_arc
;;;     cairo_arc_negative

(test cairo-arc
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:arc context 0 0 10 pi (* 2 pi))
      (cairo:close-path context)
      (let ((path (cairo:copy-path-flat context)))
        (is (equal '(:PATH
                     (:MOVE-TO -10.0d0 0.0d0)
                     (:LINE-TO -9.80078125d0 -2.01953125d0)
                     (:LINE-TO -9.21875d0 -3.89453125d0)
                     (:LINE-TO -8.296875d0 -5.59375d0)
                     (:LINE-TO -7.07421875d0 -7.07421875d0)
                     (:LINE-TO -5.59375d0 -8.296875d0)
                     (:LINE-TO -3.89453125d0 -9.21875d0)
                     (:LINE-TO -2.01953125d0 -9.80078125d0)
                     (:LINE-TO 0.0d0 -10.0d0)
                     (:LINE-TO 2.01171875d0 -9.80078125d0)
                     (:LINE-TO 3.890625d0 -9.21875d0)
                     (:LINE-TO 5.5859375d0 -8.296875d0)
                     (:LINE-TO 7.0703125d0 -7.07421875d0)
                     (:LINE-TO 8.28515625d0 -5.59375d0)
                     (:LINE-TO 9.2109375d0 -3.89453125d0)
                     (:LINE-TO 9.79296875d0 -2.01953125d0)
                     (:LINE-TO 10.0d0 0.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO -10.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

(test cairo-arc-negative
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:arc-negative context 0 0 10 (* 2 pi) pi)
      (cairo:close-path context)
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO 10.0d0 0.0d0)
                     (:CURVE-TO 10.0d0 -5.5234375d0
                                5.5234375d0 -10.0d0
                                0.0d0 -10.0d0)
                     (:CURVE-TO -5.5234375d0 -10.0d0
                                -10.0d0 -5.5234375d0
                                -10.0d0 0.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO 10.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_glyph_path

#-windows
(test cairo-glyph-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:glyph-path context '((20 0 10))) ; #\0
      (cairo:close-path context)
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO  3.546875d0 10.0d0)
                     (:LINE-TO  2.6875d0 10.0d0)
                     (:LINE-TO  2.6875d0 4.421875d0)
                     (:CURVE-TO 2.6875d0 4.08984375d0
                                2.6875d0 3.828125d0 2.6875d0 3.640625d0)
                     (:CURVE-TO 2.6953125d0 3.4453125d0
                                2.7109375d0 3.2421875d0 2.734375d0 3.03125d0)
                     (:CURVE-TO 2.6171875d0 3.1484375d0
                                2.51953125d0 3.25d0 2.4375d0 3.34375d0)
                     (:CURVE-TO 2.3515625d0 3.4296875d0
                                2.2421875d0 3.53125d0 2.109375d0 3.65625d0)
                     (:LINE-TO  1.34375d0 4.34375d0)
                     (:LINE-TO  0.890625d0 3.6875d0)
                     (:LINE-TO  2.8125d0 2.0d0)
                     (:LINE-TO  3.546875d0 2.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  3.546875d0 10.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  3.546875d0 10.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

#+windows
(test cairo-glyph-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:glyph-path context '((20 0 10))) ; #\0
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO  3.7265625d0 10.0d0)
                     (:LINE-TO  2.84765625d0 10.0d0)
                     (:LINE-TO  2.84765625d0 4.3984375d0)
                     (:CURVE-TO 2.63671875d0 4.6015625d0
                                2.359375d0 4.8046875d0
                                2.015625d0 5.00390625d0)
                     (:CURVE-TO 1.671875d0 5.20703125d0
                                1.36328125d0 5.359375d0
                                1.08984375d0 5.4609375d0)
                     (:LINE-TO  1.08984375d0 4.609375d0)
                     (:CURVE-TO 1.58203125d0 4.37890625d0
                                2.01171875d0 4.09765625d0
                                2.37890625d0 3.76953125d0)
                     (:CURVE-TO 2.74609375d0 3.44140625d0
                                3.0078125d0 3.12109375d0
                                3.16015625d0 2.8125d0)
                     (:LINE-TO  3.7265625d0 2.8125d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  3.7265625d0 10.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;;     cairo_text_path

#-windows
(test cairo-text-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:text-path context "1")
      (cairo:close-path context)
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO  3.546875d0 0.0d0)
                     (:LINE-TO  2.6875d0 0.0d0)
                     (:LINE-TO  2.6875d0 -5.578125d0)
                     (:CURVE-TO 2.6875d0 -5.91015625d0
                                2.6875d0 -6.171875d0 2.6875d0 -6.359375d0)
                     (:CURVE-TO 2.6953125d0 -6.5546875d0
                                2.7109375d0 -6.7578125d0 2.734375d0 -6.96875d0)
                     (:CURVE-TO 2.6171875d0 -6.8515625d0
                                2.51953125d0 -6.75d0 2.4375d0 -6.65625d0)
                     (:CURVE-TO 2.3515625d0 -6.5703125d0
                                2.2421875d0 -6.46875d0 2.109375d0 -6.34375d0)
                     (:LINE-TO  1.34375d0 -5.65625d0)
                     (:LINE-TO  0.890625d0 -6.3125d0)
                     (:LINE-TO  2.8125d0 -8.0d0)
                     (:LINE-TO  3.546875d0 -8.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  6.0d0 0.0d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  6.0d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

#+windows
(test cairo-text-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:text-path context "1")
      (let ((path (cairo:copy-path context)))
        (is (equal '(:PATH
                     (:MOVE-TO  3.7265625d0 0.0d0)
                     (:LINE-TO  2.84765625d0 0.0d0)
                     (:LINE-TO  2.84765625d0 -5.6015625d0)
                     (:CURVE-TO 2.63671875d0 -5.3984375d0
                                2.359375d0 -5.1953125d0
                                2.015625d0 -4.99609375d0)
                     (:CURVE-TO 1.671875d0 -4.79296875d0
                                1.36328125d0 -4.640625d0
                                1.08984375d0 -4.5390625d0)
                     (:LINE-TO  1.08984375d0 -5.390625d0)
                     (:CURVE-TO 1.58203125d0 -5.62109375d0
                                2.01171875d0 -5.90234375d0
                                2.37890625d0 -6.23046875d0)
                     (:CURVE-TO 2.74609375d0 -6.55859375d0
                                3.0078125d0 -6.87890625d0
                                3.16015625d0 -7.1875d0)
                     (:LINE-TO  3.7265625d0 -7.1875d0)
                     (:CLOSE-PATH)
                     (:MOVE-TO  5.5625d0 0.0d0))
                   (cairo:path-data-to-list path)))
      (is-false (cairo:path-destroy path))))))

;;; 2024-1-23
