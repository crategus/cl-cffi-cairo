(in-package :cairo-test)

(def-suite cairo-script-surface :in cairo-suite)
(in-suite cairo-script-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_script_mode_t

(test cairo-script-mode-t
  (is (eq :ascii (cffi:foreign-enum-keyword 'cairo:script-mode-t 0)))
  (is (eq :binary (cffi:foreign-enum-keyword 'cairo:script-mode-t 1))))

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-script-surface

(test cairo-with-script-surface
  (let ((path (sys-path "out/output.script"))
        (width 200)
        (height 200))
    (cairo:with-script-surface (surface path :color width height)
      (is (eq :success (cairo:surface-status surface)))
      (is (eq :script (cairo:surface-type surface)))
      (is (eq :success (cairo:device-status (cairo:surface-device surface))))
      (is (and (cffi:pointerp surface) (not (cffi:null-pointer-p surface))))
      (is (eq :ascii (cairo:script-mode (cairo:surface-device surface))))
      (is (eq :script (cairo:device-type (cairo:surface-device surface))))
      ;; Draw on the surface
      (cairo:with-context (context surface)
        (funcall #'draw-stroke context width height)
        (is-false (cairo:surface-show-page surface))))))

;;;     cairo_script_create

(test cairo-script-create
  (let ((path (sys-path "out/output.script"))
        (device nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :success (cairo:device-status device)))
    (is (eq :script (cairo:device-type device)))
    (is (eq :ascii (cairo:script-mode device)))
    (is (= 1 (cairo:device-reference-count device)))
    (is-false (cairo:device-destroy device))))

;;;     cairo_script_create_for_stream

;; not implemented

;;;     cairo_script_from_recording_surface

(test cairo-script-from-recording-surface
  (let* ((path (sys-path "out/output.script"))
         ;; Create the recording surface
         (surface (cairo:recording-surface-create :color))
         (script (cairo:script-create path)))
    (is (eq :success (cairo:surface-status surface)))
    (is (eq :recording (cairo:surface-type surface)))
    (is (eq :success (cairo:device-status script)))
    ;; Create a context and draw a rectangle
    (cairo:with-context (context surface)
      (funcall #'draw-stroke context 100 100)
      (is-false (cairo:surface-show-page surface))
      ;; Convert the recorded operations into a script
      (is (eq :success (cairo:script-from-recording-surface script surface)))
      (is-false (cairo:device-destroy script))
      (is-false (cairo:surface-destroy surface)))))

;;;     cairo_script_get_mode
;;;     cairo_script_set_mode

(test cairo-script-mode
  (let ((path (sys-path "out/output.script"))
        (device nil))
    (is (cffi:pointerp (setf device (cairo:script-create path))))
    (is (eq :script (cairo:device-type device)))
    (is (eq :ascii (cairo:script-mode device)))
    (is (eq :binary (setf (cairo:script-mode device) :binary)))
    (is (eq :binary (cairo:script-mode device)))
    (is-false (cairo:device-destroy device))))

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
    (is (eq :script (cairo:device-type (cairo:surface-device surface))))
    (is-false (cairo:device-destroy device))
    (is-false (cairo:surface-destroy surface))))

;;;     cairo_script_surface_create_for_target

(test cairo-script-surface-create-for-target
  (let* ((path (sys-path "out/output.script"))
         (script (cairo:script-create path))
         (proxy nil))
    (cairo:with-image-surface (target :argb32 100 100)
      (is (eq :success (cairo:surface-status target)))
      (setf proxy (cairo:script-surface-create-for-target script target))
      (is (eq :success (cairo:surface-status proxy)))
      ;; Create a context for proxy and draw a rectangle
      (cairo:with-context (context proxy)
        (funcall #'draw-stroke context 100 100)
        (is-false (cairo:surface-show-page proxy))
        ;; Write the image into a file
        (cairo:surface-write-to-png (cairo:target context)
                                    (sys-path "out/ouput.png"))
        (is-false (cairo:surface-destroy proxy))
        (is-false (cairo:device-destroy script))))))

;;;     cairo_script_write_comment

(test cairo-script-write-comment
  (let ((path (sys-path "out/output.script")))
    (cairo:with-script-surface (surface path :color 200 100)
      (is-false (cairo:script-write-comment (cairo:surface-device surface)
                                            "Comment")))))

;;; 2024-1-13
