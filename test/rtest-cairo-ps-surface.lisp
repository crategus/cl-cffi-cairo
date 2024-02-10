(in-package :cairo-test)

(def-suite cairo-ps-surface :in cairo-suite)
(in-suite cairo-ps-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     CAIRO_HAS_PS_SURFACE

;;;     cairo_ps_level_t

(test cairo-ps-level-t
  (is (eq :level-2 (cffi:foreign-enum-keyword 'cairo:ps-level-t 0)))
  (is (eq :level-3 (cffi:foreign-enum-keyword 'cairo:ps-level-t 1))))

;;; --- Functions --------------------------------------------------------------

;;;     cairo_ps_surface_create

(test cairo-ps-surface-create.1
  (let ((path (sys-path "out/output.ps"))
        (width 100)
        (height 100)
        (surface nil))
    (is (cffi:pointerp (setf surface
                             (cairo:ps-surface-create path width height))))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :ps (cairo:surface-type surface)))
    (is-false (cairo:surface-destroy surface))))

(test cairo-ps-surface-create.2
  (let ((width 100)
        (height 100)
        (surface nil))
    (is (cffi:pointerp (setf surface
                             (cairo:ps-surface-create nil width height))))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :ps (cairo:surface-type surface)))
    (is-false (cairo:surface-destroy surface))))

(test cairo-ps-surface-create.3
  (let ((path (sys-path "out/output.ps"))
        (width 100)
        (height 100)
        (surface nil))
    (is (cffi:pointerp (setf surface
                             (cairo:ps-surface-create path width height))))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :ps (cairo:surface-type surface)))
    ;; Draw on the surface
    (cairo:with-context (context surface)
      (funcall #'draw-stroke context width height)
      (is-false (cairo:surface-show-page surface)))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_ps_surface_create_for_stream

;; not implemented

;;;     cairo_ps_surface_restrict_to_level

(test cairo-ps-surface-restrict-to-level
  (let ((surface (cairo:ps-surface-create nil 100 200)))
    (is-false (cairo:ps-surface-restrict-to-level surface :level-2))
    (is-false (cairo:ps-surface-restrict-to-level surface :level-3))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_ps_get_levels

(test cairo-ps-levels
  (is (equal '(:level-2 :level-3) (cairo:ps-levels))))

;;;     cairo_ps_level_to_string

(test cairo-ps-level-to-string
  (is (equal '("PS Level 2" "PS Level 3")
             (mapcar #'cairo:ps-level-to-string (cairo:ps-levels)))))

;;;     cairo_ps_surface_set_eps
;;;     cairo_ps_surface_get_eps

(test cairo-ps-surface-eps
  (let ((surface (cairo:ps-surface-create nil 100 200)))
    (is (eq :success (cairo:surface-status surface)))
    (is-false (cairo:ps-surface-eps surface))
    (is-true (setf (cairo:ps-surface-eps surface) t))
    (is-true (cairo:ps-surface-eps surface))))

;;;     cairo_ps_surface_set_size

(test cairo-ps-surface-set-size
  (let ((surface (cairo:ps-surface-create nil 0 0)))
    (is-false (cairo:ps-surface-set-size surface 100 200))))

;;;     cairo_ps_surface_dsc_begin_setup
;;;     cairo_ps_surface_dsc_begin_page_setup
;;;     cairo_ps_surface_dsc_comment

;; The example from the documentation
(test cairo-ps-surface-dsc-comment
  (let* ((path (sys-path "out/comment.ps"))
         (width 100) (height 200)
         (surface (cairo:ps-surface-create path width height)))
    ;; Create a context for the surface
    (cairo:with-context (context surface)
      ;; Header page 1
      (cairo:ps-surface-dsc-comment surface "%%Title: My excellent document")
      (cairo:ps-surface-dsc-comment surface
                                    "%%Copyright: Copyright (C) 2014 Crategus")
      ;; Setup page 1
      (cairo:ps-surface-dsc-begin-setup surface)
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *MediaColor White")
      ;; Page setup page 1
      (cairo:ps-surface-dsc-begin-page-setup surface)
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *PageSize A3")
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *InputSlot Capacity")
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *MediaType Glossy")
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *MediaColor Blue")
      ;; Show the first page
      (cairo:show-page context)
      ;; Header page 2
      (cairo:ps-surface-dsc-comment surface
                                    "%%IncludeFeature: *PageSize A5")
      ;; Show the second page
      (cairo:show-page context))
    (cairo:surface-destroy surface)))

;;; 2024-1-14