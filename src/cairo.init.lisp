;;; ----------------------------------------------------------------------------
;;; cairo.init.lisp
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

(in-package :cairo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library cairo
      ((:and :unix (:not :darwin))
       (:or "libcairo.so.2" "libcairo.so"))
       (:darwin "libcairo.dylib")
       (:windows "libcairo-2.dll")
       (t (:default "libcairo")))

  (cffi:use-foreign-library cairo)
  (pushnew :cairo *features*))

;;; ----------------------------------------------------------------------------

;; TODO: Implement this without using GLib.

#+nil
(glib-init::push-library-version-features cairo
    (truncate (/ (cairo-version) 10000))
    (- (truncate (/ (cairo-version) 100))
       (* 100 (truncate (/ (cairo-version) 10000))))
    1 16)

#+nil
(glib-init::require-library-version "Cairo" 1 16
    (truncate (/ (cairo-version) 10000))
    (- (truncate (/ (cairo-version) 100))
       (* 100 (truncate (/ (cairo-version) 10000)))))

;;; --- End of file cairo.init.lisp --------------------------------------------
