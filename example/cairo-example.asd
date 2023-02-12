(asdf:defsystem :cairo-example
  :name "cairo-example"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-cairo :cl-cffi-glib)
  :components ((:file "cairo-example")

               (:file "draw-caps")
               (:file "draw-dashes")
               (:file "draw-joins")

               (:file "draw-stroke")
               (:file "draw-fill")
               (:file "draw-text")
               (:file "draw-paint")
               (:file "draw-mask")
               (:file "draw-source-rgba")
               (:file "draw-source-gradient")
               (:file "draw-path")
               (:file "draw-dash")
               (:file "draw-logo")

               (:file "draw-text-centered")
               (:file "draw-text-glyph")
               (:file "draw-text-gradient")
               (:file "draw-text-shaded")
               (:file "draw-text-soulmate")

               (:file "png-image")
               (:file "png-image-draw")
               (:file "png-image-for-data")
               (:file "png-image-from-png")

               (:file "svg-simple")
               (:file "svg-draw")

               (:file "pdf-simple")
               (:file "pdf-draw")
               (:file "pdf-draw-multipage")
               ))

;;; --- 2023-2-12 --------------------------------------------------------------
