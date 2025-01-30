;;; ----------------------------------------------------------------------------
;;; cl-cffi-cairo.asd
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-cairo
  :name "cl-cffi-cairo"
  :version "0.6.0"
  :author  "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module src
    :serial t
    :components
    ((:file "cairo.package")
     (:file "cairo.init")

     ;; Utilities
     (:file "cairo.version")
     (:file "cairo.status")
     (:file "cairo.matrix")
     (:file "cairo.types")

     ;; Enumerations
     (:file "cairo.enumerations")

     ;; Fonts
     (:file "cairo.font-options")
     (:file "cairo.font-face")
     (:file "cairo.scaled-font")

     ;; Surfaces
     (:file "cairo.device")
     (:file "cairo.surface")
     (:file "cairo.image-surface")
     (:file "cairo.png-surface")
     (:file "cairo.pdf-surface")
     (:file "cairo.ps-surface")
     (:file "cairo.recording-surface")
     (:file "cairo.svg-surface")
     (:file "cairo.script-surface")

     ;; Drawing
     (:file "cairo.pattern")
     (:file "cairo.region")
     (:file "cairo.context")
     (:file "cairo.transformation")
     (:file "cairo.text")
     (:file "cairo.path")
    )))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-cairo/test")))
  :depends-on (:iterate :cffi))

;; Definine a test operation for the library

(defsystem :cl-cffi-cairo/test
  :name "cl-cffi-cairo/test"
  :version "0.6.0"
  :author  "Dieter Kaiser"
  :license "MIT"
  :pathname "test/"
  :serial t
  :components ((:file "rtest-cairo")

               ;; Utilities
               (:file "rtest-cairo-version")
               (:file "rtest-cairo-status")
               (:file "rtest-cairo-matrix")

               ;; Fonts
               (:file "rtest-cairo-font-options")
               (:file "rtest-cairo-font-face")
               (:file "rtest-cairo-scaled-font")

               ;; Surfaces
               (:file "rtest-cairo-device")
               (:file "rtest-cairo-surface")
               (:file "rtest-cairo-image-surface")
               (:file "rtest-cairo-png-surface")
               (:file "rtest-cairo-pdf-surface")
               (:file "rtest-cairo-ps-surface")
               (:file "rtest-cairo-recording-surface")
               (:file "rtest-cairo-script-surface")
               (:file "rtest-cairo-svg-surface")

               ;; Drawing
               (:file "rtest-cairo-pattern")
               (:file "rtest-cairo-region")
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
