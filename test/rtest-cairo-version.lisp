(in-package :cairo-test)

(def-suite cairo-version :in cairo-suite)
(in-suite cairo-version)

;;; --- Functions --------------------------------------------------------------

;;;     CAIRO_VERSION_ENCODE

(test version-encode
  (is (= 10203 (cairo:version-encode 1 2 3))))

;;;     cairo_version

#-windows
(test version
  (is (= 11800 (cairo:version))))

;;;     cairo_version_string

#-windows
(test version-string
  (is (string= "1.18.0" (cairo:version-string))))

;;; --- 2023-11-5 --------------------------------------------------------------
