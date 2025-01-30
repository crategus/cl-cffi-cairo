(in-package :cairo-test)

(def-suite cairo-pdf-surface :in cairo-suite)
(in-suite cairo-pdf-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_pdf_outline_flags_t
;;;     cairo_pdf_metadata_t
;;;     cairo_pdf_version_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-pdf-surface

(test cairo-with-pdf-surface
  (let ((path (sys-path "out/pdf-surface.pdf")))
    (cairo:with-pdf-surface (surface path 100 200)
      (is (eq :success (cairo:surface-status surface)))
      (is (eq :pdf (cairo:surface-type surface)))
      (is (= 1 (cairo:surface-reference-count surface)))
      (is (eq :color-alpha (cairo:surface-content surface))))))

;;;     cairo:with-context-for-pdf-surface

(test cairo-with-context-for-pdf-surface
  (let ((path (sys-path "out/pdf-surface.pdf")))
    (cairo:with-context-for-pdf-surface (context path 100 200)
      (is (eq :success (cairo:surface-status (cairo:target context))))
      (is (eq :pdf (cairo:surface-type (cairo:target context))))
      ;; TODO: Why get we 2 for reference count!?
      (is (= 2 (cairo:surface-reference-count (cairo:target context))))
      (is (eq :color-alpha (cairo:surface-content (cairo:target context)))))))

;;;     cairo_pdf_surface_create

(test cairo-pdf-surface-create
  (let* ((path (sys-path "out/pdf-surface.pdf"))
         (surface (cairo:pdf-surface-create path 200 100)))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :pdf (cairo:surface-type surface)))
    (is (= 1 (cairo:surface-reference-count surface)))
    (is (eq :color-alpha (cairo:surface-content surface)))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_pdf_surface_create_for_stream

;;;     cairo_pdf_surface_restrict_to_version

(test cairo-pdf-surface-restrict-to-version
  (cairo:with-pdf-surface (surface nil 200 100)
    (is-false (cairo:pdf-surface-restrict-to-version surface :version-1-7))))

;;;     cairo_pdf_get_versions ()

(test cairo-pdf-versions
  (is (equal '(:VERSION-1-4 :VERSION-1-5 :VERSION-1-6 :VERSION-1-7)
             (cairo:pdf-versions))))

;;;     cairo_pdf_version_to_string ()

(test cairo-pdf-version-to-string
  (is (equal '("PDF 1.4" "PDF 1.5" "PDF 1.6" "PDF 1.7")
             (mapcar #'cairo:pdf-version-to-string (cairo:pdf-versions)))))

;;;     cairo_pdf_surface_set_size

(test cairo-pdf-surface-set-size
  (cairo:with-pdf-surface (surface nil 0 0)
    (is-false (cairo:pdf-surface-set-size surface 200 100))))

;;;     cairo_pdf_surface_add_outline
;;;     cairo_pdf_surface_set_metadata
;;;     cairo_pdf_surface_set_page_label
;;;     cairo_pdf_surface_set_thumbnail_size

;;; 2024-1-13
