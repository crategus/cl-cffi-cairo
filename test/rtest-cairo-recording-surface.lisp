(in-package :cairo-test)

(def-suite cairo-recording-surface :in cairo-suite)
(in-suite cairo-recording-surface)

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-reccording-surface

(test cairo-with-recording-surface.1
  (cairo:with-recording-surface (surface :color)
    (is (eq :success (cairo:surface-status surface)))
    (is-false (cairo:recording-surface-extents surface))))

(test cairo-with-recording-surface.2
  (cairo:with-recording-surface (surface :color 1 2 3 4)
    (is (eq :success (cairo:surface-status surface)))
    (is (equal '(1.0d0 2.0d0 3.0d0 4.0d0)
               (multiple-value-list
                   (cairo:recording-surface-extents surface))))))

;;;     cairo:with-context-for-recording-surface

(test cairo-with-context-for-recording-surface.1
  (cairo:with-context-for-recording-surface (context :color)
    (is (eq :success (cairo:status context)))
    (is (eq :success (cairo:surface-status (cairo:target context))))
    (is (eq :recording (cairo:surface-type (cairo:target context))))))

(test cairo-with-context-for-recording-surface.2
  (cairo:with-context-for-recording-surface (context :color 1 2 3 4)
    (is (eq :success (cairo:status context)))
    (is (eq :success (cairo:surface-status (cairo:target context))))
    (is (eq :recording (cairo:surface-type (cairo:target context))))
    (is (equal '(1.0d0 2.0d0 3.0d0 4.0d0)
               (multiple-value-list
                   (cairo:recording-surface-extents (cairo:target context)))))))

;;;     cairo_recording_surface_create ()

(test cairo-recording-surface-create.1
  (let ((surface nil))
    (is (cffi:pointerp (setf surface
                             (cairo:recording-surface-create :color))))
    (is (eq :success (cairo:surface-status surface)))
    (is-false (cairo:surface-destroy surface))))

(test cairo-recording-surface-create.2
  (let ((surface nil))
    (is (cffi:pointerp (setf surface
                             (cairo:recording-surface-create :color
                                                             :width 100
                                                             :height 200))))
    (is (eq :success (cairo:surface-status surface)))
    (is-false (cairo:surface-destroy surface))))

(test cairo-recording-surface-create.3
  (let ((surface nil))
    (is (cffi:pointerp
            (setf surface
                  (cairo::%recording-surface-create :color
                                                    (cffi:null-pointer)))))
    (is (eq :success (cairo:surface-status surface)))
    ;; Check the extents of the surface
    (is (equal '(0.0d0 0.0d0 0.0d0 0.0d0)
               (multiple-value-list
                   (cairo:recording-surface-ink-extents surface))))
    ;; Unbounded surface
    (is-false (cairo:recording-surface-extents surface))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_recording_surface_ink_extents ()
;;;     cairo_recording_surface_get_extents ()

(test cairo-recording-surface-extents.1
  (let ((surface (cairo:recording-surface-create :color)))
    (is (equal '(0.0d0 0.0d0 0.0d0 0.0d0)
               (multiple-value-list
                   (cairo:recording-surface-ink-extents surface))))
    ;; Unbounded surface
    (is-false (cairo:recording-surface-extents surface))
    (is-false (cairo:surface-destroy surface))))

(test cairo-recording-surface-extents.2
  (let ((surface (cairo:recording-surface-create :color
                                                 :x 1 :y 1/2
                                                 :width 2.5
                                                 :height 3.0d0)))
    (is (equal '(0.0d0 0.0d0 0.0d0 0.0d0)
               (multiple-value-list
                   (cairo:recording-surface-ink-extents surface))))
    ;; Bounded surface
    (is (equal '(1.0d0 0.5d0 2.5d0 3.0d0)
               (multiple-value-list
                   (cairo:recording-surface-extents surface))))
    (is-false (cairo:surface-destroy surface))))

(test cairo-recording-surface-extents.3
  (let* ((width 100)
         (height 100)
         (surface (cairo:recording-surface-create :color :width width
                                                         :height height)))
    (cairo:with-context (context surface)
      (funcall #'draw-stroke context width height)
      (is-false (cairo:surface-show-page surface)))
    (is (equal '(0.0d0 0.0d0 100.0d0 100.0d0)
               (multiple-value-list
                   (cairo:recording-surface-ink-extents surface))))
    ;; Bounded surface
    (is (equal '(0.0d0 0.0d0 100.0d0 100.0d0)
               (multiple-value-list
                   (cairo:recording-surface-extents surface))))
    (is-false (cairo:surface-destroy surface))))

;;; 2024-1-14