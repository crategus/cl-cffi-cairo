(in-package :cairo-test)

(def-suite cairo-pdf-surface :in cairo-suite)
(in-suite cairo-pdf-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     CAIRO_HAS_PDF_SURFACE
;;;     CAIRO_PDF_OUTLINE_ROOT
;;;     cairo_pdf_outline_flags_t
;;;     cairo_pdf_metadata_t
;;;     cairo_pdf_version_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_pdf_surface_create ()
;;;     cairo_pdf_surface_create_for_stream ()

;;;     cairo_pdf_surface_restrict_to_version ()

;;;     cairo_pdf_get_versions ()

#-windows
(test pdf-versions
  (is (equal '(:version-1-4 :version-1-5)
             (cairo:pdf-versions))))

;;;     cairo_pdf_version_to_string ()

#-windows
(test pdf-version-to-string
  (is (equal '("PDF 1.4" "PDF 1.5")
             (mapcar #'cairo:pdf-version-to-string (cairo:pdf-versions)))))

;;;     cairo_pdf_surface_set_size ()
;;;     cairo_pdf_surface_add_outline ()
;;;     cairo_pdf_surface_set_metadata ()
;;;     cairo_pdf_surface_set_page_label ()
;;;     cairo_pdf_surface_set_thumbnail_size ()

;;; --- 2023-1-7 ---------------------------------------------------------------
