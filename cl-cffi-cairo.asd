;;; ----------------------------------------------------------------------------
;;; cl-cffi-cairo.asd
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-cairo
  :name "cl-cffi-cairo"
  :version "0.4.0"
  :author  "Dieter Kaiser"
  :license "MIT"
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
  :version "0.4.0"
  :author  "Dieter Kaiser"
  :license "MIT"
  :pathname "test/"
  :serial t
  :components ((:file "rtest-cairo")

               (:file "rtest-cairo-version")
               (:file "rtest-cairo-status")
               (:file "rtest-cairo-matrix")

               (:file "rtest-cairo-font-options")
               (:file "rtest-cairo-scaled-font")

               (:file "rtest-cairo-device")
               (:file "rtest-cairo-surface")
               (:file "rtest-cairo-image-surface")
               (:file "rtest-cairo-pdf-surface")
               (:file "rtest-cairo-script-surface")

               (:file "rtest-cairo-pattern")
               (:file "rtest-cairo-context")
               (:file "rtest-cairo-transformation")

               (:file "rtest-cairo-text")
               (:file "rtest-cairo-path"))
  :perform
  (test-op (o c)
    (uiop:symbol-call :fiveam :run!
                      (uiop:find-symbol* :cairo-suite :cairo-test)))
  :depends-on (:cl-cffi-cairo :fiveam))

;;; --- End of file cl-cffi-cairo.asd ------------------------------------------
