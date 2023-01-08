(in-package :cairo-example)

(defun draw-cairo-stroke (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.1)
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:stroke context)
  (cairo:restore context))

(defun draw-cairo-fill (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:fill context)
  (cairo:restore context))

(defun draw-cairo-text (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0.0 0.0 0.0)
  (cairo:select-font-face context "Georgia" :weight :bold)
  (cairo:set-font-size context 1.2)
  (let ((extents (cairo:text-extents context "a")))
    (cairo:move-to context
                   (- 0.5 (/ (cairo:text-extents-width extents) 2)
                             (cairo:text-extents-x-bearing extents))
                   (- 0.5 (/ (cairo:text-extents-height extents) 2)
                             (cairo:text-extents-y-bearing extents)))
     (cairo:show-text context "a"))
  (cairo:restore context))

(defun draw-cairo-paint (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0.0 0.0 0.0)
  (cairo:paint-with-alpha context 0.5d0)
  (cairo:restore context))

(defun draw-cairo-mask (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let ((linpat (cairo:pattern-create-linear 0 0 1 1))
        (radpat (cairo:pattern-create-radial 0.5 0.5 0.25
                                             0.5 0.5 0.75)))
    (cairo:pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
    (cairo:pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)
    (cairo:pattern-add-color-stop-rgba radpat 0 0 0 0 1)
    (cairo:pattern-add-color-stop-rgba radpat 0.5 0 0 0 0)
    (setf (cairo:source context) linpat)
    (cairo:mask context radpat))
  (cairo:restore context))

(defun draw-cairo-set-source-rgba (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0 0 0)
  (cairo:move-to context 0 0)
  (cairo:line-to context 1 1)
  (cairo:move-to context 1 0)
  (cairo:line-to context 0 1)
  (setf (cairo:line-width context) 0.2)
  (cairo:stroke context)
  (cairo:rectangle context 0 0 0.5 0.5)
  (cairo:set-source-rgba context 1 0 0 0.80)
  (cairo:fill context)
  (cairo:rectangle context 0 0.5 0.5 0.5)
  (cairo:set-source-rgba context 0 1 0 0.60)
  (cairo:fill context)
  (cairo:rectangle context 0.5 0 0.5 0.5)
  (cairo:set-source-rgba context 0 0 1 0.40)
  (cairo:fill context)
  (cairo:restore context))

(defun draw-cairo-set-source-gradient (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let ((radpat (cairo:pattern-create-radial 0.25 0.25 0.10
                                             0.50 0.50 0.50))
        (linpat (cairo:pattern-create-linear 0.25 0.35 0.75 0.65)))
    (cairo:pattern-add-color-stop-rgb radpat 0.00 1.00 0.80 0.80)
    (cairo:pattern-add-color-stop-rgb radpat 1.00 0.90 0.00 0.00)
    (iter (for i from 1 below 10)
          (iter (for j from 1 below 10)
                (cairo:rectangle context
                                 (- (/ i 10.0) 0.04)
                                 (- (/ j 10.0) 0.04)
                                 0.08
                                 0.08)))
    (setf (cairo:source context) radpat)
    (cairo:fill context)
    (cairo:pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
    (cairo:pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
    (cairo:pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
    (cairo:pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
    (cairo:pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)
    (cairo:rectangle context 0.0 0.0 1.0 1.0)
    (setf (cairo:source context) linpat)
    (cairo:fill context))
  (cairo:restore context))

(defun draw-cairo-path (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0d0 1.0d0 1.0d0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.01d0)
  (cairo:set-source-rgb context 1.0d0 0.0d0 0.0d0)
  (cairo:move-to context 0.25 0.25)
  (cairo:line-to context 0.5 0.375)
  (cairo:rel-line-to context 0.25 -0.125)
  (cairo:arc context 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
  (cairo:rel-curve-to context -0.25 -0.125 -0.25 0.125 -0.5 0)
  (cairo:close-path context)
  (cairo:stroke context)
  (cairo:restore context))

(defun draw-cairo-dash (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let* ((scale 300)
         (dashes (list (/ 50.0 scale)
                       (/ 10.0 scale)
                       (/ 10.0 scale)
                       (/ 10.0 scale)))
         (offset (/ -50.0 scale)))
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:dash context offset) dashes)
    (setf (cairo:line-width context) (/ 10.0 scale))
    (cairo:move-to context (/ 128.0 scale) (/ 25.6 scale))
    (cairo:line-to context (/ 230.4 scale) (/ 230.4 scale))
    (cairo:rel-line-to context (/ -102.4 scale) 0.0)
    (cairo:curve-to context (/ 51.2 scale)
                            (/ 230.4 scale)
                            (/ 51.2 scale)
                            (/ 128.0 scale)
                            (/ 128.0 scale)
                            (/ 128.0 scale))
    (cairo:stroke context))
  (cairo:restore context))


(defun draw-to-image (func)
  (let ((width 400) (height 400))
    (with-cairo-image-surface (surface :argb32 width height)
      (with-cairo-context (context surface)
        (funcall func context width height)
        (cairo:surface-write-to-png surface (sys-path "image.png"))))))

(defun draw-to-svg-file (func)
  (let* (;; Create a SVG surface and a Cairo context.
         (width 400)
         (height 400)
         (filename (sys-path "svgfile.svg"))
         (surface (cairo:svg-surface-create filename width height))
         (context (cairo:create surface)))
    (funcall func context width height)
    ;; Destroy the resources.
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

(defun draw-to-pdf-file ()
  (let* (;; Create a PDF surface and a Cairo context.
         (filename (sys-path "pdffile.pdf"))
         (width 400)
         (height 400)
         (surface (cairo:pdf-surface-create filename width height))
         (context (cairo:create surface)))

    (cairo:pdf-surface-set-metadata surface :title "The document title")
    (cairo:pdf-surface-set-metadata surface :author "The document author")
    (cairo:pdf-surface-set-metadata surface :subject "The document subject")
    (cairo:pdf-surface-set-metadata surface :keywords "The document keywords")
    (cairo:pdf-surface-set-metadata surface :creator "The document creator")
    (cairo:pdf-surface-set-metadata surface :create-date "2023-01-07T01:02:03")
    (cairo:pdf-surface-set-metadata surface :mod-date "2023-01-08T04:05:06")

    (cairo:pdf-surface-restrict-to-version surface :version-1-4)

    (cairo:pdf-surface-set-page-label surface "Cairo stroke")
    (funcall #'draw-cairo-stroke context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo fill")
    (funcall #'draw-cairo-fill context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo text")
    (funcall #'draw-cairo-text context width height)
    (cairo:show-page context)

    (cairo:pdf-surface-set-page-label surface "Cairo paint")
    (funcall #'draw-cairo-paint context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo mask")
    (funcall #'draw-cairo-mask context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo RGBA")
    (funcall #'draw-cairo-set-source-rgba context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo gradient")
    (funcall #'draw-cairo-set-source-gradient context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo path")
    (funcall #'draw-cairo-path context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo dash")
    (funcall #'draw-cairo-dash context width height)
    (cairo:surface-show-page surface)

    ;; Destroy the resources
    (cairo:surface-destroy surface)
    (cairo:destroy context)))


(defun demo-cairo-stroke (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Stroke"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (setf (cairo:line-width cr) 0.1)
          (cairo:set-source-rgb cr 1.0 0.0 0.0)
          (cairo:rectangle cr 0.25 0.25 0.5 0.5)
          (cairo:stroke cr)))
    (gtk:widget-show window)))

(defun demo-cairo-fill (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Fill"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (cairo:set-source-rgb cr 1.0 0.0 0.0)
          (cairo:rectangle cr 0.25 0.25 0.5 0.5)
          (cairo:fill cr)))
      (gtk:widget-show window)))

(defun demo-cairo-text (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Text"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
         ;; Drawing code goes here
         (cairo:set-source-rgb cr 0.0 0.0 0.0)
         (cairo:select-font-face cr "Georgia" :weight :bold)
         (cairo:set-font-size cr 1.2)
         (let ((text-extents (cairo:text-extents cr "a")))
           (cairo:move-to cr
                          (- 0.5
                             (/ (cairo:text-extents-width text-extents) 2)
                             (cairo:text-extents-x-bearing text-extents))
                          (- 0.5
                             (/ (cairo:text-extents-height text-extents) 2)
                             (cairo:text-extents-y-bearing text-extents)))
           (cairo:show-text cr "a"))))
    (gtk:widget-show window)))

(defun demo-cairo-paint (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Paint"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (cairo:set-source-rgb cr 0.0 0.0 0.0)
          (cairo:paint-with-alpha cr 0.5d0)))
    (gtk:widget-show window)))

(defun demo-cairo-mask (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Mask"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (let ((linpat (cairo:pattern-create-linear 0 0 1 1))
                (radpat (cairo:pattern-create-radial 0.5 0.5 0.25
                                                     0.5 0.5 0.75)))
            (cairo:pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
            (cairo:pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)
            (cairo:pattern-add-color-stop-rgba radpat 0 0 0 0 1)
            (cairo:pattern-add-color-stop-rgba radpat 0.5 0 0 0 0)
            (setf (cairo:source cr) linpat)
            (cairo:mask cr radpat))))
    (gtk:widget-show window)))

(defun demo-cairo-set-source-rgba (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Set Source RGBA"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (cairo:set-source-rgb cr 0 0 0)
          (cairo:move-to cr 0 0)
          (cairo:line-to cr 1 1)
          (cairo:move-to cr 1 0)
          (cairo:line-to cr 0 1)
          (setf (cairo:line-width cr) 0.2)
          (cairo:stroke cr)
          (cairo:rectangle cr 0 0 0.5 0.5)
          (cairo:set-source-rgba cr 1 0 0 0.80)
          (cairo:fill cr)
          (cairo:rectangle cr 0 0.5 0.5 0.5)
          (cairo:set-source-rgba cr 0 1 0 0.60)
          (cairo:fill cr)
          (cairo:rectangle cr 0.5 0 0.5 0.5)
          (cairo:set-source-rgba cr 0 0 1 0.40)
          (cairo:fill cr)))
    (gtk:widget-show window)))

(defun demo-cairo-set-source-gradient (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Set Source Gradient"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (let ((radpat (cairo:pattern-create-radial 0.25 0.25 0.10
                                                     0.50 0.50 0.50))
                (linpat (cairo:pattern-create-linear 0.25 0.35 0.75 0.65)))
            (cairo:pattern-add-color-stop-rgb radpat 0.00 1.00 0.80 0.80)
            (cairo:pattern-add-color-stop-rgb radpat 1.00 0.90 0.00 0.00)
            (iter (for i from 1 below 10)
                  (iter (for j from 1 below 10)
                        (cairo:rectangle cr
                                         (- (/ i 10.0) 0.04)
                                         (- (/ j 10.0) 0.04)
                                         0.08
                                         0.08)))
            (setf (cairo:source cr) radpat)
            (cairo:fill cr)
            (cairo:pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
            (cairo:pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
            (cairo:pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
            (cairo:pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
            (cairo:pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)
            (cairo:rectangle cr 0.0 0.0 1.0 1.0)
            (setf (cairo:source cr) linpat)
            (cairo:fill cr))))
        (gtk:widget-show window)))

(defun demo-cairo-path (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Path"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0d0 1.0d0 1.0d0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (setf (cairo:line-width cr) 0.01d0)
          (cairo:set-source-rgb cr 1.0d0 0.0d0 0.0d0)
          (cairo:move-to cr 0.25 0.25)
          (cairo:line-to cr 0.5 0.375)
          (cairo:rel-line-to cr 0.25 -0.125)
          (cairo:arc cr
                     0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
          (cairo:rel-curve-to cr -0.25 -0.125 -0.25 0.125 -0.5 0)
          (cairo:close-path cr)
          (cairo:stroke cr)))
    (gtk:widget-show window)))

(defun demo-cairo-dash (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Demo Cairo Stroke"
                                :default-width 400
                                :default-height 400)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          ;; Clear surface
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Example is in 1.0 x 1.0 coordinate space
          (cairo:scale cr width height)
          ;; Drawing code goes here
          (let* ((scale 500)
                 (dashes (list (/ 50.0 scale)
                               (/ 10.0 scale)
                               (/ 10.0 scale)
                               (/ 10.0 scale)))
                 (offset (/ -50.0 scale)))
            (cairo:set-source-rgb cr 0.0 0.0 0.0)
            (setf (cairo:dash cr offset) dashes)
            (setf (cairo:line-width cr) (/ 10.0 scale))
            (cairo:move-to cr (/ 128.0 scale) (/ 25.6 scale))
            (cairo:line-to cr (/ 230.4 scale) (/ 230.4 scale))
            (cairo:rel-line-to cr (/ -102.4 scale) 0.0)
            (cairo:curve-to cr (/ 51.2 scale)
                               (/ 230.4 scale)
                               (/ 51.2 scale)
                               (/ 128.0 scale)
                               (/ 128.0 scale)
                               (/ 128.0 scale))
             (cairo:stroke cr))))
    (gtk:widget-show window)))

;;; 2022-12-20
