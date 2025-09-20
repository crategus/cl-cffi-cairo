(in-package :cairo-test)

(def-suite cairo-status-suite :in cairo-suite)
(in-suite cairo-status-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_status_t

(test cairo-status-t
  ;; Check values
  (is (=  0 (cffi:foreign-enum-value 'cairo:status-t :success)))
  (is (=  1 (cffi:foreign-enum-value 'cairo:status-t :no-memory)))
  (is (=  2 (cffi:foreign-enum-value 'cairo:status-t :invalid-restore)))
  (is (=  3 (cffi:foreign-enum-value 'cairo:status-t :invalid-pop-group)))
  (is (=  4 (cffi:foreign-enum-value 'cairo:status-t :no-current-point)))
  (is (=  5 (cffi:foreign-enum-value 'cairo:status-t :invalid-matrix)))
  (is (=  6 (cffi:foreign-enum-value 'cairo:status-t :invalid-status)))
  (is (=  7 (cffi:foreign-enum-value 'cairo:status-t :null-pointer)))
  (is (=  8 (cffi:foreign-enum-value 'cairo:status-t :invalid-string)))
  (is (=  9 (cffi:foreign-enum-value 'cairo:status-t :invalid-path-data)))
  (is (= 10 (cffi:foreign-enum-value 'cairo:status-t :read-error)))
  (is (= 11 (cffi:foreign-enum-value 'cairo:status-t :write-error)))
  (is (= 12 (cffi:foreign-enum-value 'cairo:status-t :surface-finished)))
  (is (= 13 (cffi:foreign-enum-value 'cairo:status-t :surface-type-mismatch)))
  (is (= 14 (cffi:foreign-enum-value 'cairo:status-t :pattern-type-mismatch)))
  (is (= 15 (cffi:foreign-enum-value 'cairo:status-t :invalid-content)))
  (is (= 16 (cffi:foreign-enum-value 'cairo:status-t :invalid-format)))
  (is (= 17 (cffi:foreign-enum-value 'cairo:status-t :invalid-visual)))
  (is (= 18 (cffi:foreign-enum-value 'cairo:status-t :file-not-found)))
  (is (= 19 (cffi:foreign-enum-value 'cairo:status-t :invalid-dash)))
  (is (= 20 (cffi:foreign-enum-value 'cairo:status-t :invalid-dsc-comment)))
  (is (= 21 (cffi:foreign-enum-value 'cairo:status-t :invalid-index)))
  (is (= 22 (cffi:foreign-enum-value 'cairo:status-t :clip-not-representable)))
  (is (= 23 (cffi:foreign-enum-value 'cairo:status-t :temp-file-error)))
  (is (= 24 (cffi:foreign-enum-value 'cairo:status-t :invalid-stride)))
  (is (= 25 (cffi:foreign-enum-value 'cairo:status-t :font-type-mismatch)))
  (is (= 26 (cffi:foreign-enum-value 'cairo:status-t :user-font-immutable)))
  (is (= 27 (cffi:foreign-enum-value 'cairo:status-t :user-font-error)))
  (is (= 28 (cffi:foreign-enum-value 'cairo:status-t :negative-count)))
  (is (= 29 (cffi:foreign-enum-value 'cairo:status-t :invalid-clusters)))
  (is (= 30 (cffi:foreign-enum-value 'cairo:status-t :invalid-slant)))
  (is (= 31 (cffi:foreign-enum-value 'cairo:status-t :invalid-weight)))
  (is (= 32 (cffi:foreign-enum-value 'cairo:status-t :invalid-size)))
  (is (= 33 (cffi:foreign-enum-value 'cairo:status-t :user-font-not-implemented)))
  (is (= 34 (cffi:foreign-enum-value 'cairo:status-t :device-type-mismatch)))
  (is (= 35 (cffi:foreign-enum-value 'cairo:status-t :device-error)))
  (is (= 36 (cffi:foreign-enum-value 'cairo:status-t :invalid-mesh-construction)))
  (is (= 37 (cffi:foreign-enum-value 'cairo:status-t :device-finished)))
  (is (= 38 (cffi:foreign-enum-value 'cairo:status-t :jbig2-global-missing)))
  (is (= 39 (cffi:foreign-enum-value 'cairo:status-t :png-error)))
  (is (= 40 (cffi:foreign-enum-value 'cairo:status-t :freetype-error)))
  (is (= 41 (cffi:foreign-enum-value 'cairo:status-t :win32-gdk-error)))
  (is (= 42 (cffi:foreign-enum-value 'cairo:status-t :tag-error)))
  (is (= 43 (cffi:foreign-enum-value 'cairo:status-t :dwrite-error)))
  (is (= 44 (cffi:foreign-enum-value 'cairo:status-t :svg-font-error)))
  (is (= 45 (cffi:foreign-enum-value 'cairo:status-t :last-status)))
  ;; Check keywords
  (is (eq :success (cffi:foreign-enum-keyword 'cairo:status-t 0)))
)

;;; --- Functions --------------------------------------------------------------

;;;     cairo_status_to_string

(test cairo-status-to-string
  (is (string= "no error has occurred"
               (cairo:status-to-string :success)))
  (is (string= "out of memory"
               (cairo:status-to-string :no-memory)))
  (is (string= "cairo:restore without matching cairo:save"
               (cairo:status-to-string :invalid-restore)))
  (is (string= "no current point defined"
               (cairo:status-to-string :no-current-point)))
  (is (string= "invalid matrix (not invertible)"
               (cairo:status-to-string :invalid-matrix))))

;;; 2025-09-19
