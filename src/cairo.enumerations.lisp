;;; ----------------------------------------------------------------------------
;;; cairo.enumerations.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library, see <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------
;;; cairo_antialias_t
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
 "@version{2025-09-02}
  @begin{declaration}
(cffi:defcenum antialias-t
  :default
  :none
  :gray
  :subpixel
  :fast
  :good
  :best)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Use the default antialiasing for the subsystem and target
        device.}
      @entry[:none]{Use a bilevel alpha mask.}
      @entry[:gray]{Perform single-color antialiasing, using shades of gray for
        black text on a white background, for example.}
      @entry[:subpixel]{Perform antialiasing by taking advantage of the order
        of subpixel elements on devices such as LCD panels.}
      @entry[:fast]{Hint that the backend should perform some antialiasing but
        prefer speed over quality.}
      @entry[:good]{Hint that the backend should balance quality against
        performance.}
      @entry[:best]{Hint that the backend should render at the highest quality,
        sacrificing speed if necessary.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:antialias-t} enumeration specifies the type of antialiasing
    to do when rendering text or shapes.
  @end{short}
  As it is not necessarily clear from the above what advantages a particular
  antialias method provides, there is also a set of hints that have the
  @code{:fast}, @code{:good}, and @code{:best} values.

  These hints make no guarantee on how the backend will perform its
  rasterisation, if it even rasterises, nor that they have any differing effect
  other than to enable some form of antialiasing. In the case of glyph
  rendering, the @code{:fast} and @code{:good} values will be mapped to the
  @code{:gray} value, with the @code{:best} value being equivalent to the
  @code{:subpixel} value. The interpretation of the @code{:default} value is
  left entirely up to the backend, typically this will be similar to the
  @code{:good} value.
  @see-symbol{cairo:context-t}
  @see-function{cairo:antialias}")

(export 'antialias-t)

;;; --- End of file cairo.enumerations.lisp ------------------------------------
