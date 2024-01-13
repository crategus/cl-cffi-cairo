(in-package :cairo-test)

(def-suite cairo-script-surface :in cairo-suite)
(in-suite cairo-script-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_script_mode_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-script-surface

(test cairo-with-script-surface
  (let ((path (sys-path "out/output.script")))
    (cairo:with-script-surface (surface path :color 200 100)
      (is (and (cffi:pointerp surface) (not (cffi:null-pointer-p surface))))
      (is (eq :ascii (cairo:script-mode (cairo:surface-device surface))))
      (is (eq :script (cairo:device-type (cairo:surface-device surface)))))))

;;;     cairo_script_create

(test cairo-script-create
  (let ((path (sys-path "out/output.script"))
        (device nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :success (cairo:device-status device)))
    (is (eq :script (cairo:device-type device)))
    (is (eq :ascii (cairo:script-mode device)))))

;;;     cairo_script_create_for_stream
;;;     cairo_script_from_recording_surface

;;;     cairo_script_get_mode
;;;     cairo_script_set_mode

(test cairo-script-mode
  (let ((path (sys-path "out/outpout.script"))
        (device nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :script (cairo:device-type device)))
    (is (eq :ascii (cairo:script-mode device)))
    (is (eq :binary (setf (cairo:script-mode device) :binary)))
    (is (eq :binary (cairo:script-mode device)))))

;;;     cairo_script_surface_create

(test cairo-script-surface-create
  (let ((path (sys-path "out/output.script"))
        (device nil)
        (surface nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :script (cairo:device-type device)))
    (is (cffi:pointerp (setf surface
                             (cairo:script-surface-create device
                                                          :color
                                                          200 100))))
    (is (cffi:pointer-eq device (cairo:surface-device surface)))
    (is (eq :script (cairo:device-type (cairo:surface-device surface))))))

;;;     cairo_script_surface_create_for_target

;;;     cairo_script_write_comment

(test cairo-script-write-comment
  (let ((path (sys-path "out/comment.script")))
    (cairo:with-script-surface (surface path :color 200 100)
      (is-false (cairo:script-write-comment (cairo:surface-device surface)
                                            "Der Kommentar")))))

;;; 2024-1-12
