;;; ----------------------------------------------------------------------------
;;; cairo.init.lisp
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

(in-package :cairo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library cairo
      ((:and :unix (:not :darwin))
       (:or "libcairo.so.2" "libcairo.so"))
       (:darwin "libcairo.dylib")
       (:windows "libcairo-2.dll")
       (t (:default "libcairo")))

  (cffi:use-foreign-library cairo)
  ;; push the hostname on *features*
  (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
  (pushnew :cairo *features*))

;;; ----------------------------------------------------------------------------

;;; Functions and macros to check the library version

(defmacro push-library-version-features (library-name
                                         major-version-var
                                         minor-version-var
                                         &body versions)
  `(eval-when (:load-toplevel :execute)
     ,@(iter (for (major minor) on versions by #'cddr)
             (collect `(when (or (and (= ,major-version-var ,major)
                                      (>= ,minor-version-var ,minor))
                                 (> ,major-version-var ,major))
                         (pushnew ,(intern (format nil "~A-~A-~A"
                                                   (string library-name)
                                                   major minor)
                                           (find-package :keyword))
                                  *features*))))))

(define-condition foreign-library-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s "Library ~A has too old version: it is ~A but required ~
                        to be at least ~A"
                       (.library c)
                       (.actual-version c)
                       (.minimum-version c)))))

(defun require-library-version (library min-major-version
                                        min-minor-version
                                        major-version
                                        minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
      (error 'foreign-library-version-mismatch
             :library library
             :minimum-version (format nil "~A.~A"
                                      min-major-version min-minor-version)
             :actual-version (format nil "~A.~A"
                                     major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

;;; ----------------------------------------------------------------------------

(defparameter +cairo-version+ (cffi:foreign-funcall "cairo_version" :int))

(push-library-version-features cairo
    (truncate (/ +cairo-version+ 10000))
    (- (truncate (/ +cairo-version+ 100))
       (* 100 (truncate (/ +cairo-version+ 10000))))
    1 16   ; Since 2018-10-19
    1 18   ; Since 2023-09-23
    1 20)

(require-library-version "Cairo" 1 16
    (truncate (/ +cairo-version+ 10000))
    (- (truncate (/ +cairo-version+ 100))
       (* 100 (truncate (/ +cairo-version+ 10000)))))

;;; ----------------------------------------------------------------------------

(declaim (inline mklist))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun flatten (tree)
  (let (lst)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree lst)))))
      (traverse tree))
    (nreverse lst)))

;;; --- End of file cairo.init.lisp --------------------------------------------
