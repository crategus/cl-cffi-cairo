## cl-cffi-cairo

### General Information

The `cl-cffi-cairo` library is a Lisp binding to Cairo.

Cairo is a 2D graphics library that supports multiple output devices. Currently
supported output targets include the X Window System (via both Xlib and XCB),
Quartz, Win32, image buffers, and PostScript, PDF, and SVG file output.

Cairo is designed to produce consistent output on all output media types while
taking advantage of display hardware acceleration, such as through the X Render
Extension.

The Cairo API offers operations similar to those of the PostScript and PDF
drawing operators. These operations include stroking and filling cubic Bézier
splines, transforming and compositing translucent images, and rendering
antialiased text. All drawing operations can be transformed by any affine
transformation, such as scaling, rotating, or shearing.

This library is loaded from the
[`cl-cffi-gtk4`](https://github.com/crategus/cl-cffi-gtk4) library for a Lisp
binding to GTK 4 and the [`cl-cffi-gtk3`](https://github.com/crategus/cl-cffi-gtk3)
library for a Lisp binding to GTK 3.

### License

The `cl-cffi-cairo` library is licensed under the MIT license.

### Installation

The `cl-cffi-cairo` library is ASDF installable:
```
(asdf:load-system :cl-cffi-cairo)
```
Get the Cairo version the library is running against:
```
(cairo:version-string)
=> "1.18.4"
```
The `cl-cffi-cairo` library runs successfully with SBCL and CCL on Ubuntu. It
also runs on Windows with SBCL. It may run with other compilers and on other
systems as well.

### Documentation

The Cairo API documentation is included in the
[`cl-cffi-gtk4`](https://crategus.com/books/cl-cffi-gtk4/) API documentation.

## Cairo examples

The Cairo library comes with examples. Load the examples and switch to the
package for the examples:
```
(asdf:load-system :cairo-example)
=> T
(in-package :cairo-example)
=> #<Package "CAIRO-EXAMPLE">
```
Execute one of the demo functions, for example:
```
(demo-pdf-draw)
=>
Draw function : DRAW-STROKE (400 x 400 size)
Save PDF to   : .../cairo/example/out/draw-stroke.pdf
```
The function is executed for the `draw-stroke` function with a size of 400 x 400
points. The demo function has three optional arguments. The first takes a symbol
as the function designator for a drawing function. The second and the third
arguments specify the desired width and height of the drawing context. If the
third argument is omitted, the size of the drawing context is width x width. The
following is an example of calling the demo function with all three arguments
set to values different from the defaults:
```
(demo-pdf-draw #draw-mask 350 400)
=>
Draw function : DRAW-MASK (350 x 400 size)
Save PDF to   : .../cairo/example/out/draw-mask.pdf
```

## Cairo testsuite

The `cl-cffi-cairo` library comes with a testsuite. The testsuite can be
performed with
```
(asdf:test-system :cl-cffi-cairo)
```
or loaded and executed with
```
(asdf:load-system :cl-cffi-cairo/test)
=> T
(in-package :cairo-test)
=> #<PACKAGE "CAIRO-TEST">
(run! 'cairo-suite)
=>
Running test suite CAIRO-SUITE
 Running test suite CAIRO-VERSION-SUITE
  Running test CAIRO-VERSION-ENCODE .
  Running test CAIRO-VERSION .
  Running test CAIRO-VERSION-STRING .
 Running test suite CAIRO-VERSION-SUITE
 Running test suite CAIRO-STATUS-SUITE
  Running test CAIRO-STATUS-T ...............................................
  Running test CAIRO-STATUS-TO-STRING .....
 Running test suite CAIRO-STATUS-SUITE

...

 Running test suite CAIRO-PATH
  Running test CAIRO-PATH-STRUCTURE.1 ......................
  Running test CAIRO-PATH-STRUCTURE.2 .....
  Running test CAIRO-COPY-PATH ..........
  Running test CAIRO-COPY-PATH-FLAT .......
  Running test CAIRO-APPEND-PATH .........
  Running test CAIRO-CURRENT-POINT .......
  Running test CAIRO-NEW-PATH/SUB-PATH/CLOSE-PATH.1 ......
  Running test CAIRO-NEW-PATH/SUB-PATH/CLOSE-PATH.2 .........
  Running test CAIRO-PATH-EXTENTS ..
  Running test CAIRO-MOVE-TO/REL-MOVE-TO ..
  Running test CAIRO-LINE-TO/REL-LINE-TO ..
  Running test CAIRO-CURVE-TO ..
  Running test CAIRO-REL-CURVE-TO ..
  Running test CAIRO-RECTANGLE ..
  Running test CAIRO-ARC ..
  Running test CAIRO-ARC-NEGATIVE ..
  Running test CAIRO-GLYPH-PATH ..
  Running test CAIRO-TEXT-PATH ..
 Running test suite CAIRO-PATH
 Did 1126 checks.
    Pass: 1126 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```
Unfortunately, since many of the tests depend on the hardware or the local
environment, so they will likely fail on your system. This issue may be resolved
in the future.
