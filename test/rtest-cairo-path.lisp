(in-package :cairo-test)

(def-suite cairo-path :in cairo-suite)
(in-suite cairo-path)

(defvar *verbose-cairo-path* nil)

;;; Types and Values

;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t

(test path-structure.1
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
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
      (is (eq :move-to (cairo:header-data-type data)))
      (is (= 2 (cairo:header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo:header-data-type data))
        (format t " header-length : ~a~%" (cairo:header-length data)))
      ;; Second element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (= 10.0 (cairo:point-x data)))
      (is (= 10.0 (cairo:point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo:point-x data))
        (format t "       point-y : ~a~%" (cairo:point-y data)))
      ;; Third element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (eq :line-to (cairo:header-data-type data)))
      (is (= 2 (cairo:header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo:header-data-type data))
        (format t " header-length : ~a~%" (cairo:header-length data)))
      ;; Fourth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (= 20.0 (cairo:point-x data)))
      (is (= 20.0 (cairo:point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo:point-x data))
        (format t "       point-y : ~a~%" (cairo:point-y data)))
      ;; Fifth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (eq :close-path (cairo:header-data-type data)))
      (is (= 1 (cairo:header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo:header-data-type data))
        (format t " header-length : ~a~%" (cairo:header-length data)))
      ;; Sixth element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (eq :move-to (cairo:header-data-type data)))
      (is (= 2 (cairo:header-length data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "   header-type : ~a~%" (cairo:header-data-type data))
        (format t " header-length : ~a~%" (cairo:header-length data)))
      ;; Seventh element
      (setf data
            (cffi:inc-pointer
                data
                (cffi:foreign-type-size '(:struct cairo:path-data-t))))
      (is (= 10.0 (cairo:point-x data)))
      (is (= 10.0 (cairo:point-y data)))
      (when *verbose-cairo-path*
        (format t "          data : ~a~%" data)
        (format t "       point-x : ~a~%" (cairo:point-x data))
        (format t "       point-y : ~a~%" (cairo:point-y data)))
      (is-false (cairo:path-destroy path)))))

(test path-structure.2
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
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
            with size = (cffi:foreign-type-size '(:struct cairo:path-data-t))
            collect element
            while (< count numdata)
            do (cond ((eq :move-to (cairo:header-data-type data))
                      (setf element (list :move-to))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo:header-data-type data))
                        (format t "length : ~a~%" (cairo:header-length data)))
                      (setf count (incf count (cairo:header-length data)))
                      (setf data (cffi:inc-pointer data size))
                      (when *verbose-cairo-path*
                        (format t " x : ~a~%" (cairo:point-x data))
                        (format t " y : ~a~%" (cairo:point-y data)))
                      (setf data (cffi:inc-pointer data size)))
                     ((eq :line-to (cairo:header-data-type data))
                      (setf element (list :line-to))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo:header-data-type data))
                        (format t "length : ~a~%" (cairo:header-length data)))
                      (setf count (incf count (cairo:header-length data)))
                      (setf data (cffi:inc-pointer data size))
                      (when *verbose-cairo-path*
                        (format t " x : ~a~%" (cairo:point-x data))
                        (format t " y : ~a~%" (cairo:point-y data)))
                      (setf data (cffi:inc-pointer data size)))
                     ((eq :close-path (cairo:header-data-type data))
                      (setf element (list :close-path))
                      (when *verbose-cairo-path*
                        (format t "move-to :~%")
                        (format t "  data : ~a~%" data)
                        (format t "  type : ~a~%" (cairo:header-data-type data))
                        (format t "length : ~a~%" (cairo:header-length data)))
                      (setf count (incf count (cairo:header-length data)))
                      (setf data (cffi:inc-pointer data size)))
                     (t (error "KEYWORD not known to PATH-DATA-TYPE-T"))))
      (is-false (cairo:path-destroy path)))))

;;; --- Functions --------------------------------------------------------------

(defun path-to-lisp (path)
  (loop with count = 0
        with numdata = (cairo:path-numdata path)
        with element = :path
        with data = (cairo:path-data path)
        with size = (cffi:foreign-type-size '(:struct cairo:path-data-t))
        collect element
        while (< count numdata)
        do (cond ((eq :move-to (cairo:header-data-type data))
                  (setf element (list :move-to))
                  (setf count (incf count (cairo:header-length data)))
                  (setf data (cffi:inc-pointer data size))
                  (push (cairo:point-x data) element)
                  (push (cairo:point-y data) element)
                  (setf element (reverse element))
                  (setf data (cffi:inc-pointer data size)))
                 ((eq :line-to (cairo:header-data-type data))
                  (setf element (list :line-to))
                  (setf count (incf count (cairo:header-length data)))
                  (setf data (cffi:inc-pointer data size))
                  (push (cairo:point-x data) element)
                  (push (cairo:point-y data) element)
                  (setf element (reverse element))
                  (setf data (cffi:inc-pointer data size)))
                 ((eq :curve-to (cairo:header-data-type data))
                  (setf element (list :curve-to))
                  (setf count (incf count (cairo:header-length data)))
                  (setf data (cffi:inc-pointer data size))
                  (push (cairo:point-x data) element)
                  (push (cairo:point-y data) element)
                  (setf data (cffi:inc-pointer data size))
                  (push (cairo:point-x data) element)
                  (push (cairo:point-y data) element)
                  (setf data (cffi:inc-pointer data size))
                  (push (cairo:point-x data) element)
                  (push (cairo:point-y data) element)
                  (setf element (reverse element))
                  (setf data (cffi:inc-pointer data size)))
                 ((eq :close-path (cairo:header-data-type data))
                  (setf element (list :close-path))
                  (setf count (incf count (cairo:header-length data)))
                  (setf data (cffi:inc-pointer data size)))
                 (t (error "KEYWORD ~a not known to PATH-DATA-TYPE-T"
                           (cairo:header-data-type data))))))

;;;     cairo_copy_path

(test copy-path
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
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
                 (path-to-lisp path)))
    (is-false (cairo:path-destroy path)))))

;;;     cairo_copy_path_flat

(test copy-path-flat
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
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
                 (path-to-lisp path)))
      (is-false (cairo:path-destroy path)))))

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

;;; --- 2023-1-13 --------------------------------------------------------------
