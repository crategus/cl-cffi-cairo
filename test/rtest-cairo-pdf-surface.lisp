(in-package :cairo-test)

(def-suite cairo-pdf-surface :in cairo-suite)
(in-suite cairo-pdf-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_pdf_outline_flags_t

(test cairo-pdf-outline-flags-t
  ;; Check values
  (is (= 1 (cffi:foreign-bitfield-value 'cairo:pdf-outline-flags-t :open)))
  (is (= 2 (cffi:foreign-bitfield-value 'cairo:pdf-outline-flags-t :bold)))
  (is (= 4 (cffi:foreign-bitfield-value 'cairo:pdf-outline-flags-t :italic)))
  ;; Check symbols
  (is (equal '(:OPEN :BOLD :ITALIC)
             (cffi:foreign-bitfield-symbols 'cairo:pdf-outline-flags-t
                                            (+ 1 2 4)))))

;;;     cairo_pdf_metadata_t

(test cairo-pdf-metadata-t
  ;; Check values
  (is (= 0 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :title)))
  (is (= 1 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :author)))
  (is (= 2 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :subject)))
  (is (= 3 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :keywords)))
  (is (= 4 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :creator)))
  (is (= 5 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :create-date)))
  (is (= 6 (cffi:foreign-enum-value 'cairo:pdf-metadata-t :mod-date)))
  ;; Check keywords
  (is (eq :title (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 0)))
  (is (eq :author (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 1)))
  (is (eq :subject (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 2)))
  (is (eq :keywords (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 3)))
  (is (eq :creator (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 4)))
  (is (eq :create-date (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 5)))
  (is (eq :mod-date (cffi:foreign-enum-keyword 'cairo:pdf-metadata-t 6))))

;;;     cairo_pdf_version_t

(test cairo-pdf-version-t
  ;; Check values
  (is (= 0 (cffi:foreign-enum-value 'cairo:pdf-version-t :version-1-4)))
  (is (= 1 (cffi:foreign-enum-value 'cairo:pdf-version-t :version-1-5)))
  (is (= 2 (cffi:foreign-enum-value 'cairo:pdf-version-t :version-1-6)))
  (is (= 3 (cffi:foreign-enum-value 'cairo:pdf-version-t :version-1-7)))
  ;; Check keywords
  (is (eq :version-1-4 (cffi:foreign-enum-keyword 'cairo:pdf-version-t 0)))
  (is (eq :version-1-5 (cffi:foreign-enum-keyword 'cairo:pdf-version-t 1)))
  (is (eq :version-1-6 (cffi:foreign-enum-keyword 'cairo:pdf-version-t 2)))
  (is (eq :version-1-7 (cffi:foreign-enum-keyword 'cairo:pdf-version-t 3))))

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
