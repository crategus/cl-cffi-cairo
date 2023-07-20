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

;;; ----------------------------------------------------------------------------
;;; enum cairo_antialias_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum antialias-t
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
(cffi:defcenum antialias-t
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
