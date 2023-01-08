(defpackage :cairo-example
  (:use :common-lisp :iter)
  (:import-from :gobject #:gobject-class)
  (:import-from :glib    #:+g-source-continue+)
  (:import-from :cairo   #:with-cairo-image-surface
                         #:with-cairo-context)
  (:export #:run-example
           #:demo-png-image
           #:demo-png-image-for-data
           #:demo-png-image-from-png
           #:demo-svg-file
           #:demo-pdf-file
           #:demo-drawing
           #:demo-drawing-lines
           #:demo-drawing-dashes
           #:demo-drawing-caps
           #:demo-drawing-joins
           #:demo-text-soulmate
           #:demo-text-centered
           #:demo-text-shaded
           #:demo-text-gradient
           #:demo-text-glyph
           #:demo-cairo-clock

           #:demo-cairo-stroke
           #:demo-cairo-fill
           #:demo-cairo-text
           #:demo-cairo-paint
           #:demo-cairo-mask
           #:demo-cairo-set-source-rgba
           #:demo-cairo-set-source-gradient
           #:demo-cairo-path
           #:demo-cairo-dash
           ))

(in-package :cairo-example)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :cl-cffi-cairo-example)))
    (princ-to-string (merge-pathnames filename system-path))))

 ;; A wrapper to run an example
(defun run-example (func &optional (filename nil))
  (let ((resource (when filename
                        (g:resource-load (sys-path filename))))
        ;; Create an application
        (app (make-instance 'gtk:application
                            :application-id "com.crategus.run-example"
                            :resource-base-path (cffi:null-pointer)
                            :flags :none)))
    ;; Register the resources
    (when resource
      (g:resources-register resource))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (funcall func application)))
    ;; Connect signal "shutdown"
    (g:signal-connect app "shutdown"
        (lambda (application)
          (declare (ignore application))
          ;; Unregister the resources
          (when resource
            (g:resources-unregister resource))))
    ;; Run the application
    (g:application-run app nil)))

;;; 2022-12-20
