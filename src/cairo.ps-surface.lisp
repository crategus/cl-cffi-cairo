;;; ----------------------------------------------------------------------------
;;; cairo.ps-surface.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2025 Dieter Kaiser
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
;;;
;;; PostScript Surfaces
;;;
;;;     Rendering PostScript documents
;;;
;;; Types and Values
;;;
;;;     cairo_ps_level_t
;;;
;;; Functions
;;;
;;;     cairo_ps_surface_create
;;;     cairo_ps_surface_create_for_stream                  not implemented
;;;     cairo_ps_surface_restrict_to_level
;;;     cairo_ps_get_levels
;;;     cairo_ps_level_to_string
;;;     cairo_ps_surface_set_eps
;;;     cairo_ps_surface_get_eps
;;;     cairo_ps_surface_set_size
;;;     cairo_ps_surface_dsc_begin_setup
;;;     cairo_ps_surface_dsc_begin_page_setup
;;;     cairo_ps_surface_dsc_comment
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_level_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum ps-level-t
  :level-2
  :level-3)

#+liber-documentation
(setf (liber:alias-for-symbol 'ps-level-t)
      "CEnum"
      (liber:symbol-documentation 'ps-level-t)
 "@version{2025-1-29}
  @begin{declaration}
(cffi:defcenum ps-level-t
  :level-2
  :level-3)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:level-2]{The language level 2 of the PostScript specification.}
      @entry[:level-3]{The language level 3 of the PostScript specification.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:ps-level-t} enumeration is used to describe the language
    level of the PostScript Language Reference that a generated PostScript file
    will conform to.
  @end{short}
  @see-function{cairo:ps-surface-restrict-to-level}")

(export 'ps-level-t)

;;; ----------------------------------------------------------------------------
;;; cairo:with-ps-surface
;;; ----------------------------------------------------------------------------

(defmacro with-ps-surface ((surface path width height) &body body)
 #+liber-documentation
 "@version{2025-1-29}
  @syntax{(cairo:with-ps-surface (surface path width height) body) => result}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @argument[path]{a path or namestring for a filename for the PS output,
    @code{nil} may be used to specify no output, this will generate a PS surface
    that may be queried and used as a source, without generating a temporary
    file}
  @argument[width]{a number coerced to a double float for the width of the
    surface, in points (1 point == 1/72 inch)}
  @argument[height]{a number coerced to a double float for the height of the
    surface, in points (1 point == 1/72 inch)}
  @begin{short}
    The @symbol{cairo:with-ps-surface} macro allocates a new PostScript
    @symbol{cairo:surface-t} instance with the given @arg{path}, @arg{width},
    and @arg{height} and executes the body that uses the PostScript surface.
  @end{short}
  After execution of the body the allocated memory for the PostScript surface
  is released.
  @see-symbol{cairo:surface-t}"
  `(let ((,surface (ps-surface-create ,path ,width ,height)))
     (unwind-protect
       (progn ,@body)
       (surface-destroy ,surface))))

(export 'with-ps-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_create
;;; ----------------------------------------------------------------------------

(defun ps-surface-create (path width height)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[path]{a path or namestring for a filename for the PS output,
    @code{nil} may be used to specify no output, this will generate a PS surface
    that may be queried and used as a source, without generating a temporary
    file}
  @argument[width]{a number coerced to a double float for the width of the
    surface, in points (1 point == 1/72 inch)}
  @argument[height]{a number coerced to a double float for the height of the
    surface, in points (1 point == 1/72 inch)}
  @return{The newly created PostScript @symbol{cairo:surface-t} instance.}
  @begin{short}
    Creates a PostScript surface of the specified size in points to be written
    to @arg{path}.
  @end{short}
  The caller owns the surface and should call the @fun{cairo:surface-destroy}
  function when done with it.

  This function always returns a valid pointer, but it will return a pointer to
  a \"nil\" surface if an error such as out of memory occurs. You can use the
  @fun{cairo:surface-status} function to check for this.

  Note that the size of individual pages of the PostScript output can vary.
  See the @fun{cairo:ps-surface-set-size} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (cffi:foreign-funcall "cairo_ps_surface_create"
                        :string (if path (namestring path) (cffi:null-pointer))
                        :double (coerce width 'double-float)
                        :double (coerce height 'double-float)
                        (:pointer (:struct surface-t))))

(export 'ps-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_create_for_stream
;;;
;;; Creates a PostScript surface of the specified size in points to be written
;;; incrementally to the stream represented by write_func and closure .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_restrict_to_level
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_surface_restrict_to_level"
               ps-surface-restrict-to-level) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @argument[level]{a @symbol{cairo:ps-level-t} value}
  @begin{short}
    Restricts the generated PostSript file to @arg{level}.
  @end{short}
  See the @fun{cairo:ps-levels} function for a list of available level values
  that can be used here.

  This function should only be called before any drawing operations have been
  performed on the given surface. The simplest way to do this is to call this
  function immediately after creating the surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:ps-level-t}
  @see-function{cairo:ps-levels}"
  (surface (:pointer (:struct surface-t)))
  (level ps-level-t))

(export 'ps-surface-restrict-to-level)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_get_levels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_get_levels" %ps-levels) :void
  (levels :pointer)
  (num :pointer))

(defun ps-levels ()
 #+liber-documentation
 "@version{2025-1-29}
  @return{The list of @symbol{cairo:ps-level-t} values with the supported
    levels.}
  @begin{short}
    Used to retrieve the list of supported levels.
  @end{short}
  See the @fun{cairo:ps-surface-restrict-to-level} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:ps-level-t}
  @see-function{cairo:ps-surface-restrict-to-level}"
  (cffi:with-foreign-objects ((ptr :pointer) (num :int))
    (%ps-levels ptr num)
    (iter (with levels = (cffi:mem-ref ptr :pointer))
          (for n from 0 below (cffi:mem-ref num :int))
          (collect (cffi:mem-aref levels 'ps-level-t n)))))

(export 'ps-levels)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_level_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_level_to_string" ps-level-to-string) :string
 #+liber-documentation
 "@version{2025-1-29}
  @argument[level]{an integer with the level ID}
  @return{The string associated to the given @arg{level}.}
  @begin{short}
    Get the string representation of the given level ID.
  @end{short}
  This function will return @code{nil} if level ID is not valid. See the
  @fun{cairo:ps-levels} function for a way to get the list of valid level IDs.
  @begin[Examples]{dictionary}
    Get the string representations for the supported levels.
    @begin{pre}
(mapcar #'cairo:ps-level-to-string (cairo:ps-levels))
=> (\"PS Level 2\" \"PS Level 3\")
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:ps-levels}"
  (level ps-level-t))

(export 'ps-level-to-string)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_get_eps
;;; cairo_ps_surface_set_eps
;;; ----------------------------------------------------------------------------

(defun (setf ps-surface-eps) (eps surface )
  (cffi:foreign-funcall "cairo_ps_surface_set_eps"
                        (:pointer (:struct surface-t)) surface
                        :bool eps
                        :void)
  eps)

(cffi:defcfun ("cairo_ps_surface_get_eps" ps-surface-eps) :bool
 #+liber-documentation
 "@version{2025-1-29}
  @syntax{(cairo:ps-surface-eps surface) => eps}
  @syntax{(setf (cairo:ps-surface-eps surface) eps)}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @argument[eps]{@em{true} if the surface will output Encasulated PostScript}
  @begin{short}
    The @fun{cairo:ps-surface-eps} function checks whether the PostScript
    surface will output Encapsulated PostScript.
  @end{short}
  The @setf{cairo:ps-surface-eps} function will set if the PostScript surface
  will ouput Encapsulated PostScript.

  This function should only be called before any drawing operations have been
  performed on the current page. The simplest way to do this is to call this
  function immediately after creating the surface. An Encapsulated PostScript
  file should never contain more than one page.
  @see-symbol{cairo:surface-t}"

  (surface (:pointer (:struct surface-t))))

(export 'ps-surface-eps)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_set_size
;;; ----------------------------------------------------------------------------

(defun ps-surface-set-size (surface width height)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @argument[width]{a number coerced to a double float for the surface width,
    in points (1 point == 1/72 inch}
  @argument[height]{a number coerced to a double float for the surface height,
    in points}
  @begin{short}
    Changes the size of a PostScript surface for the current and subsequent
    pages.
  @end{short}

  This function should only be called before any drawing operations have been
  performed on the current page. The simplest way to do this is to call this
  function immediately after creating the surface or immediately after
  completing a page with either the @fun{cairo:show-page} or
  @fun{cairo:copy-page} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:show-page}
  @see-function{cairo:copy-page}"
  (cffi:foreign-funcall "cairo_ps_surface_set_size"
                        (:pointer (:struct surface-t)) surface
                        :double (coerce width 'double-float)
                        :double (coerce height 'double-float)
                        :void))

(export 'ps-surface-set-size)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_begin_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_surface_dsc_begin_setup" ps-surface-dsc-begin-setup)
    :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @begin{short}
    This function indicates that subsequent calls to the
    @fun{cairo:ps-surface-dsc-comment} function should direct comments to the
    Setup section of the PostScript output.
  @end{short}
  This function should be called at most once per surface, and must be called
  before any call to the @fun{cairo:ps-surface-dsc-begin-page-setup} function
  and before any drawing is performed to the surface.

  See the @fun{cairo:ps-surface-dsc-comment} function for more details.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:ps-surface-dsc-comment}
  @see-function{cairo:ps-surface-dsc-begin-page-setup}"
  (surface (:pointer (:struct surface-t))))

(export 'ps-surface-dsc-begin-setup)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_begin_page_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_surface_dsc_begin_page_setup"
               ps-surface-dsc-begin-page-setup) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @begin{short}
    This function indicates that subsequent calls to the
    @fun{cairo:ps-surface-dsc-comment} function should direct comments to the
    PageSetup section of the PostScript output.
  @end{short}

  This function call is only needed for the first page of a surface. It should
  be called after any call to the @fun{cairo:ps-surface-dsc-begin-setup} and
  before any drawing is performed to the surface.

  See the @fun{cairo:ps-surface-dsc-comment} function for more details.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:ps-surface-dsc-comment}
  @see-function{cairo:ps-surface-dsc-begin-setup}"
  (surface (:pointer (:struct surface-t))))

(export 'ps-surface-dsc-begin-page-setup)

;;; ----------------------------------------------------------------------------
;;; cairo_ps_surface_dsc_comment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_ps_surface_dsc_comment" ps-surface-dsc-comment) :void
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a PostScript @symbol{cairo:surface-t} instance}
  @argument[comment]{a comment string to be emitted into the PostScript output}
  @begin{short}
    Emit a comment into the PostScript output for the given surface.
  @end{short}
  The comment is expected to conform to the PostScript Language Document
  Structuring Conventions (DSC). Please see that manual for details on the
  available comments and their meanings. In particular, the %%IncludeFeature
  comment allows a device-independent means of controlling printer device
  features. So the PostScript Printer Description Files Specification will
  also be a useful reference.

  The comment string must begin with a percent character (%) and the total
  length of the string (including any initial percent characters) must not
  exceed 255 characters. Violating either of these conditions will place
  @arg{surface} into an error state. But beyond these two conditions, this
  function will not enforce conformance of the comment with any particular
  specification.

  The comment string should not have a trailing newline.

  The DSC specifies different sections in which particular comments can appear.
  This function provides for comments to be emitted within three sections: the
  header, the Setup section, and the PageSetup section. Comments appearing in
  the first two sections apply to the entire document while comments in the
  BeginPageSetup section apply only to a single page.

  For comments to appear in the header section, this function should be called
  after the surface is created, but before a call to the
  @fun{cairo:ps-surface-dsc-begin-setup} function.

  For comments to appear in the Setup section, this function should be called
  after a call to the @fun{cairo:ps-surface-dsc-begin-setup} function but before
  a call to the @fun{cairo:ps-surface-dsc-begin-page-setup} function.

  For comments to appear in the PageSetup section, this function should be
  called after a call to the @fun{cairo:ps-surface-dsc-begin-page-setup}
  function.

  Note that it is only necessary to call the
  @fun{cairo:ps-surface-dsc-begin-page-setup} function for the first page of any
  surface. After a call to the @fun{cairo:show-page} or @fun{cairo:copy-page}
  functions comments are unambiguously directed to the PageSetup section of the
  current page. But it does not hurt to call this function at the beginning of
  every page as that consistency may make the calling code simpler.

  As a final note, Cairo automatically generates several comments on its own.
  As such, applications must not manually generate any of the following
  comments:
  @begin[]{table}
    @entry[Header section:]{%!PS-Adobe-3.0, %%Creator, %%CreationDate, %%Pages,
      %%BoundingBox, %%DocumentData, %%LanguageLevel, %%EndComments.}
    @entry[Setup section:]{%%BeginSetup, %%EndSetup}
    @entry[PageSetup section:]{%%BeginPageSetup, %%PageBoundingBox,
      %%EndPageSetup.}
    @entry[Other sections:]{%%BeginProlog, %%EndProlog, %%Page, %%Trailer,
      %%EOF}
  @end{table}
  @begin[Examples]{dictionary}
    Here is an example sequence showing how this function might be used:
    @begin{pre}
(test cairo-ps-surface-dsc-comment
  (let* ((path (sys-path \"out/comment.ps\"))
         (width 100) (height 200)
         (surface (cairo:ps-surface-create path width height)))
    ;; Create a context for the surface
    (cairo:with-context (context surface)
      ;; Header page 1
      (cairo:ps-surface-dsc-comment surface \"%%Title: My document\")
      (cairo:ps-surface-dsc-comment surface
                                    \"%%Copyright: Copyright (C) 2014 Crategus\")
      ;; Setup page 1
      (cairo:ps-surface-dsc-begin-setup surface)
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *MediaColor White\")
      ;; Page setup page 1
      (cairo:ps-surface-dsc-begin-page-setup surface)
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *PageSize A3\")
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *InputSlot Capacity\")
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *MediaType Glossy\")
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *MediaColor Blue\")
      ;; Draw to first page here
      ...
      ;; Show first page
      (cairo:show-page context)
      ;; Header page 2
      (cairo:ps-surface-dsc-comment surface
                                    \"%%IncludeFeature: *PageSize A5\")
      ;; Draw to second page here
      ...
      ;; Show second page
      (cairo:show-page context))
    (cairo:surface-destroy surface)))
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:ps-surface-dsc-begin-setup}
  @see-function{cairo:ps-surface-dsc-begin-page-setup}"
  (surface (:pointer (:struct surface-t)))
  (comment :string))

(export 'ps-surface-dsc-comment)

;;; --- End of file cairo.ps-surface.lisp --------------------------------------
