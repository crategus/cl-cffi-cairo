(in-package :cairo-test)

(def-suite cairo-version-suite :in cairo-suite)
(in-suite cairo-version-suite)

;;; --- Functions --------------------------------------------------------------

;;;     CAIRO_VERSION_ENCODE

(test cairo-version-encode
  (is (= 10203 (cairo:version-encode 1 2 3))))

;;;     cairo_version

#-windows
(test cairo-version
  (is (= 11802 (cairo:version))))

#+windows
(test cairo-version
  (is (= 11804 (cairo:version))))

;;;     cairo_version_string

#-windows
(test cairo-version-string
  (is (string= "1.18.2" (cairo:version-string))))

#+windows
(test cairo-version-string
  (is (string= "1.18.4" (cairo:version-string))))

;;; 2025-3-28
