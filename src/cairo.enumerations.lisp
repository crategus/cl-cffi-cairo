;;; ----------------------------------------------------------------------------
;;; cairo.enumerations.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------
;;; enum cairo_antialias_t
;;; ----------------------------------------------------------------------------

(defcenum antialias-t
  :default
  :none
  :gray
  :subpixel
  :fast
  :good
  :best)

#+liber-documentation
(setf (liber:alias-for-symbol 'antialias-t)
      "CEnum"
      (liber:symbol-documentation 'antialias-t)
 "@version{2023-1-11}
  @begin{short}
    The @sym{cairo:antialias-t} enumeration specifies the type of antialiasing
    to do when rendering text or shapes.
  @end{short}
  As it is not necessarily clear from the above what advantages a particular
  antialias method provides, there is also a set of hints:
  @begin[code]{table}
    @entry[:fast]{Allow the backend to degrade raster quality for speed.}
    @entry[:good·]{A balance between speed and quality.}
    @entry[:best]{A high-fidelity, but potentially slow, raster mode.}
  @end{table}
  These make no guarantee on how the backend will perform its rasterisation,
  if it even rasterises, nor that they have any differing effect other than to
  enable some form of antialiasing. In the case of glyph rendering, @code{:fast}
  and @code{:good} will be mapped to @code{:gray}, with @code{:best} being
  equivalent to @code{:subpixel}. The interpretation of @code{:default} is left
  entirely up to the backend, typically this will be similar to @code{:good}.
  @begin{pre}
(defcenum antialias-t
  :default
  :none
  :gray
  :subpixel
  :fast
  :good
  :best)
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Use the default antialiasing for the subsystem and target
      device.}
    @entry[:none]{Use a bilevel alpha mask.}
    @entry[:gray]{Perform single-color antialiasing, using shades of gray for
      black text on a white background, for example.}
    @entry[:subpixel]{Perform antialiasing by taking advantage of the order of
      subpixel elements on devices such as LCD panels.}
    @entry[:fast]{Hint that the backend should perform some antialiasing but
      prefer speed over quality.}
    @entry[:good]{The backend should balance quality against performance.}
    @entry[:best]{Hint that the backend should render at the highest quality,
      sacrificing speed if necessary.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:antialias}")

(export 'antialias-t)

;;; --- End of file cairo.enumerations.lisp ------------------------------------
