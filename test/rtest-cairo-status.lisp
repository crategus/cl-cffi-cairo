(in-package :cairo-test)

(def-suite cairo-status-suite :in cairo-suite)
(in-suite cairo-status-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_status_t

;;; Functions

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

;;;     cairo_debug_reset_static_data

;;; 2024-1-27
