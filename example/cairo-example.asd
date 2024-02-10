(asdf:defsystem :cairo-example
  :name "cairo-example"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-cairo
               :cl-cffi-glib)               ; only for g:malloc and g:free
  :components ((:file "cairo-example")
               ;; Examples from https://www.cairographics.org/samples/
               (:file "draw-arc")
               (:file "draw-arc-negative")
               (:file "draw-clip")
               (:file "draw-clip-image")
               (:file "draw-curve-to")
               (:file "draw-dash")
               (:file "draw-fill-preserve")
               (:file "draw-fill-style")
               (:file "draw-gradient")
               (:file "draw-image")
               (:file "draw-image-pattern")
               (:file "draw-multi-caps")
               (:file "draw-rounded-rectangle")
               (:file "draw-line-cap")
               (:file "draw-line-join")
               (:file "draw-text")
               (:file "draw-text-align-center")
               (:file "draw-text-extents")

               (:file "draw-mesh-coons-patch")
               (:file "draw-mesh-gouraud-triangle")

               (:file "draw-caps")
               (:file "draw-dashes")
               (:file "draw-joins")

               (:file "draw-stroke")
               (:file "draw-fill")
               (:file "draw-text-letter")
               (:file "draw-paint")
               (:file "draw-mask")
               (:file "draw-source-rgba")
               (:file "draw-source-gradient")
               (:file "draw-path")
               (:file "draw-logo")

               (:file "draw-text-centered")
               (:file "draw-text-glyph")
               (:file "draw-text-gradient")
               (:file "draw-text-shaded")
               (:file "draw-text-soulmate")

               ;; More draw functions
               (:file "draw-path-glyph")

               (:file "png-image")
               (:file "png-image-draw")
               (:file "png-image-for-data")
               (:file "png-image-from-png")

               (:file "svg-simple")
               (:file "svg-draw")

               (:file "pdf-simple")
               (:file "pdf-draw")
               (:file "pdf-draw-multipage")

               (:file "script-draw")
               (:file "ps-draw")
               ))

;;; --- 2023-7-21 --------------------------------------------------------------
