;;; ----------------------------------------------------------------------------
;;; cairo.init.lisp
;;;
;;; Copyright (C) 2012 - 2022 Dieter Kaiser
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
