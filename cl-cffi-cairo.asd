;;; ----------------------------------------------------------------------------
;;; cl-cffi-cairo.asd
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-cairo
  :name "cl-cffi-cairo"
  :version "0.1.0"
  :author  "Dieter Kaiser"
  :license "LLGPL"
  :components
  ((:module src
    :serial t
    :components
    ((:file "cairo.package")
     (:file "cairo.init")

     ;; Utilities
     (:file "cairo.version")                ; Version checks
     (:file "cairo.status")                 ; Decoding Cairo's status
     (:file "cairo.matrix")                 ; Generic matrix operations
     (:file "cairo.types")                  ; Generic data types

     ;; Enumerations
     (:file "cairo.enumerations")           ; Enumerations for Cairo

     ;; Fonts
     (:file "cairo.font-options")           ; How a font should be rendered
     (:file "cairo.font-face")              ; Base class for font faces
     (:file "cairo.scaled-font")            ; Font face at particular size

     (:file "cairo.freetype-font")          ; Font support for FreeType
     (:file "cairo.win32-font")             ; Font support for Windows
     (:file "cairo.quartz-font")            ; Font support on OS X
     (:file "cairo.user-font")              ; Font support for user font

     ;; Surfaces
     (:file "cairo.device")                 ; interface to rendering system
     (:file "cairo.surface")                ; Base class for surfaces
     (:file "cairo.image-surface")          ; Rendering to memory buffers
     (:file "cairo.png-surface")            ; Reading and writing PNG images
     (:file "cairo.pdf-surface")            ; Rendering PDF documents
     (:file "cairo.ps-surface")             ; Rendering PostScript documents
     (:file "cairo.recording-surface")      ; Records all drawing operations
     (:file "cairo.win32-surface")          ; Windows surface support
     (:file "cairo.svg-surface")            ; Rendering SVG documents
     (:file "cairo.quartz-surface")         ; Rendering to Quartz surfaces
     (:file "cairo.xcb-surface")            ; Rendering using the XCB library
     (:file "cairo.xlib-surface")           ; Rendering using XLib
     (:file "cairo.xlib-xrender-surface")   ; using XLib and X Render
     (:file "cairo.script-surface")         ; Rendering to replayable scripts

     ;; Drawing
     (:file "cairo.pattern")                ; Sources for drawing
     (:file "cairo.context")                ; Cairo drawing context
     (:file "cairo.region")                 ; Representing pixel area
     (:file "cairo.transformation")         ; Transformations
     (:file "cairo.raster-source")          ; Supplying arbitrary image data
     (:file "cairo.tag")                    ; Hyperlinks, document structure

     ;; More Drawing
     (:file "cairo.text")                   ; Rendering text and glyphs
     (:file "cairo.path")                   ; Creating paths
    )))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-cairo/test")))
  :depends-on (:cffi))

;; Definine a test operation for the library

(defsystem :cl-cffi-cairo/test
  :name "cl-cffi-cairo/test"
  :pathname "test/"
  :serial t
  :components ((:file "rtest-cairo")
               (:file "rtest-cairo-context")
               (:file "rtest-cairo-device")
               (:file "rtest-cairo-font-options")
               (:file "rtest-cairo-image-surface")
               (:file "rtest-cairo-matrix")
               (:file "rtest-cairo-path")
               (:file "rtest-cairo-pattern")
               (:file "rtest-cairo-pdf-surface")
               (:file "rtest-cairo-scaled-font")
               (:file "rtest-cairo-status")
               (:file "rtest-cairo-text")
               (:file "rtest-cairo-transformation")
               (:file "rtest-cairo-version"))
  :perform
  (test-op (o c)
    (uiop:symbol-call :fiveam :run!
                      (uiop:find-symbol* :cairo-suite :cairo-test)))
  :depends-on (:cl-cffi-cairo :fiveam))

;; Examples for the Cairo library

(asdf:defsystem :cl-cffi-cairo/example
  :name "cl-cffi-cairo/example"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :pathname "example/"
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

;;; --- End of file cl-cffi-cairo.asd ------------------------------------------
