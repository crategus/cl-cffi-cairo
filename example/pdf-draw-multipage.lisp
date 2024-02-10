(in-package :cairo-example)

(defun demo-pdf-draw-multipage ()
  (let* (;; Create a PDF surface and a Cairo context.
         (id 0)
         (pathname (sys-path "out/pdf-draw-multipage.pdf"))
         (width 400)
         (height 400)
         (surface (cairo:pdf-surface-create pathname width height))
         (context (cairo:create surface)))

    (cairo:pdf-surface-set-metadata surface :title "The document title")
    (cairo:pdf-surface-set-metadata surface :author "The document author")
    (cairo:pdf-surface-set-metadata surface :subject "The document subject")
    (cairo:pdf-surface-set-metadata surface :keywords "The document keywords")
    (cairo:pdf-surface-set-metadata surface :creator "The document creator")
    (cairo:pdf-surface-set-metadata surface :create-date "2023-01-07T01:02:03")
    (cairo:pdf-surface-set-metadata surface :mod-date "2023-01-08T04:05:06")

    (cairo:pdf-surface-restrict-to-version surface :version-1-4)
    (setf id
          (cairo:pdf-surface-add-outline surface id "Cairo Examples" "" :bold))

    (cairo:pdf-surface-set-page-label surface "Cairo stroke")
    (funcall #'cairo-draw-stroke context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo fill")
    (funcall #'cairo-draw-fill context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo text letter")
    (funcall #'cairo-draw-text-letter context width height)
    (cairo:show-page context)

    (cairo:pdf-surface-set-page-label surface "Cairo paint")
    (funcall #'cairo-draw-paint context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo mask")
    (funcall #'cairo-draw-mask context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo RGBA")
    (funcall #'cairo-draw-source-rgba context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo gradient")
    (funcall #'cairo-draw-source-gradient context width height)
    (cairo:surface-show-page surface)

    (setf id
          (cairo:pdf-surface-add-outline surface id "Cairo Path" "" :bold))

    (cairo:pdf-surface-set-page-label surface "Cairo path")
    (funcall #'cairo-draw-path context width height)
    (cairo:surface-show-page surface)

    (cairo:pdf-surface-set-page-label surface "Cairo dash")
    (funcall #'draw-dash context width height)
    (cairo:surface-show-page surface)

    ;; Destroy the resources
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

;;; --- 2023-2-12 --------------------------------------------------------------
