(in-package :cairo-test)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(let ((eps-factor 1.0d-1))
  (defun approx-equal (x y)
    (or (< (abs (- x y)) eps-factor)
        (< (abs (- x y)) (* eps-factor (max (abs x) (abs y)))))))

(defun sys-path (filename &optional (package :cl-cffi-cairo))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2022-10-7
