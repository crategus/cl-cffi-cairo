;;; ----------------------------------------------------------------------------
;;; cairo.context.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;;
;;; cairo_t
;;;
;;;     The cairo drawing context
;;;
;;; Types and Values
;;;
;;;     cairo_t
;;;
;;;     cairo_antialias_t                        --> cairo.enumerations.lisp
;;;     cairo_fill_rule_t
;;;     cairo_line_cap_t
;;;     cairo_line_join_t
;;;     cairo_operator_t
;;;
;;;     cairo_rectangle_t                        --> cairo.types.lisp
;;;     cairo_rectangle_list_t                   --> cairo.types.lisp
;;;
;;; Functions
;;;
;;;     cairo_create
;;;     cairo_reference
;;;     cairo_get_reference_count
;;;     cairo_destroy
;;;     cairo_status
;;;     cairo_save
;;;     cairo_restore
;;;     cairo_get_target
;;;     cairo_push_group
;;;     cairo_push_group_with_content
;;;     cairo_pop_group
;;;     cairo_pop_group_to_source
;;;     cairo_get_group_target
;;;     cairo_set_source_rgb
;;;     cairo_set_source_rgba
;;;     cairo_set_source
;;;     cairo_set_source_surface
;;;     cairo_get_source
;;;
;;;     cairo_set_antialias
;;;     cairo_get_antialias
;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash
;;;
;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule
;;;     cairo_set_line_cap
;;;     cairo_get_line_cap
;;;
;;;     cairo_set_line_join
;;;     cairo_get_line_join
;;;     cairo_set_line_width
;;;     cairo_get_line_width
;;;     cairo_set_hairline                                 Since 1.18
;;;     cairo_get_hairline                                 Since 1.18
;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit
;;;     cairo_set_operator
;;;     cairo_get_operator
;;;     cairo_set_tolerance
;;;     cairo_get_tolerance
;;;
;;;     cairo_clipstatus
;;;     cairo_clip_preserve
;;;     cairo_clip_extents
;;;     cairo_in_clip
;;;     cairo_reset_clip
;;;
;;;     cairo_rectangle_list_destroy                       not exported
;;;     cairo_copy_clip_rectangle_list
;;;
;;;     cairo_fill
;;;     cairo_fill_preserve
;;;     cairo_fill_extents
;;;     cairo_in_fill
;;;     cairo_mask
;;;     cairo_mask_surface
;;;     cairo_paint
;;;     cairo_paint_with_alpha
;;;     cairo_stroke
;;;     cairo_stroke_preserve
;;;     cairo_stroke_extents
;;;     cairo_in_stroke
;;;     cairo_copy_page
;;;     cairo_show_page
;;;     cairo_set_user_data                                not implemented
;;;     cairo_get_user_data                                not implemented
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; enum cairo_fill_rule_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum fill-rule-t
  :winding
  :even-odd)

#+liber-documentation
(setf (liber:alias-for-symbol 'fill-rule-t)
      "CEnum"
      (liber:symbol-documentation 'fill-rule-t)
 "@version{2024-1-17}
  @begin{short}
    The @symbol{cairo:fill-rule-t} enumeration is used to select how paths are
    filled.
  @end{short}
  For both fill rules, whether or not a point is included in the fill is
  determined by taking a ray from that point to infinity and looking at
  intersections with the path. The ray can be in any direction, as long as it
  does not pass through the end point of a segment or have a tricky
  intersection such as intersecting tangent to the path. Note that filling is
  not actually implemented in this way. This is just a description of the rule
  that is applied. The default fill rule is the @code{:winding} value. New
  entries may be added in future versions.
  @begin{pre}
(cffi:defcenum fill-rule-t
  :winding
  :even-odd)
  @end{pre}
  @begin[code]{table}
    @entry[:winding]{If the path crosses the ray from left-to-right, counts +1.
      If the path crosses the ray from right to left, counts -1. Left and right
      are determined from the perspective of looking along the ray from the
      starting point. If the total count is non-zero, the point will be
      filled.}
    @entry[:even-odd]{Counts the total number of intersections, without regard
      to the orientation of the contour. If the total number of intersections
      is odd, the point will be filled.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill-rule}")

(export 'fill-rule-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_cap_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum line-cap-t
  :butt
  :round
  :square)

#+liber-documentation
(setf (liber:alias-for-symbol 'line-cap-t)
      "CEnum"
      (liber:symbol-documentation 'line-cap-t)
 "@version{2024-1-17}
  @begin{short}
    The @symbol{cairo:line-cap-t} enumeration specifies how to render the
    endpoints of the path when stroking.
  @end{short}
  The default line cap style is the @code{:butt} value.
  @begin{pre}
(cffi:defcenum line-cap-t
  :butt
  :round
  :square)
  @end{pre}
  @begin[code]{table}
    @entry[:butt]{Start (stop) the line exactly at the start (end) point.}
    @entry[:round]{Use a round ending, the center of the circle is the end
      point.}
    @entry[:square]{Use squared ending, the center of the square is the end
      point.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-cap}")

(export 'line-cap-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_join_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum line-join-t
  :miter
  :round
  :bevel)

#+liber-documentation
(setf (liber:alias-for-symbol 'line-join-t)
      "CEnum"
      (liber:symbol-documentation 'line-join-t)
 "@version{2024-1-17}
  @begin{short}
    The @symbol{cairo:line-join-t} enumeration specifies how to render the
    junction of two lines when stroking.
  @end{short}
  The default line join style is the @code{:miter} value.
  @begin{pre}
(cffi:defcenum line-join-t
  :miter
  :round
  :bevel)
  @end{pre}
  @begin[code]{table}
    @entry[:miter]{Use a sharp (angled) corner, see the @fun{cairo:miter-limit}
      function.}
    @entry[:round]{Use a rounded join, the center of the circle is the joint
      point.}
    @entry[:bevel]{Use a cut-off join, the join is cut off at half the line
      width from the joint point.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:miter-limit}
  @see-function{cairo:line-join}")

(export 'line-join-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_operator_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum operator-t
  :clear
  :source
  :over
  :in
  :out
  :atop
  :dest
  :dest-over
  :dest-in
  :dest-out
  :dest-atop
  :xor
  :add
  :saturate
  :multiply
  :screen
  :overlay
  :darken
  :lighten
  :color-dodge
  :color-burn
  :hard-light
  :soft-ligth
  :difference
  :exclusion
  :hsl-hue
  :hsl-saturation
  :hsl-color
  :hsl-luminosity)

#+liber-documentation
(setf (liber:alias-for-symbol 'operator-t)
      "CEnum"
      (liber:symbol-documentation 'operator-t)
 "@version{2024-1-17}
  @begin{short}
    The @symbol{cairo:operator-t} enumeration is used to set the compositing
    operator for all Cairo drawing operations.
  @end{short}
  The default operator is the @code{:over} value. The operators marked as
  unbounded modify their destination even outside of the mask layer, that is,
  their effect is not bound by the mask layer. However, their effect can still
  be limited by way of clipping.

  To keep things simple, the operator descriptions here document the behavior
  for when both source and destination are either fully transparent or fully
  opaque. The actual implementation works for translucent layers too. For a
  more detailed explanation of the effects of each operator, including the
  mathematical definitions, see
  @url[https://cairographics.org/operators/]{Cairo's Compositing Operators}.
  @begin{pre}
(cffi:defcenum operator-t
  :clear
  :source
  :over
  :in
  :out
  :atop
  :dest
  :dest-over
  :dest-in
  :dest-out
  :dest-atop
  :xor
  :add
  :saturate
  :multiply
  :screen
  :overlay
  :darken
  :lighten
  :color-dodge
  :color-burn
  :hard-light
  :soft-ligth
  :difference
  :exclusion
  :hsl-hue
  :hsl-saturation
  :hsl-color
  :hsl-luminosity)
  @end{pre}
  @begin[code]{table}
    @entry[:clear]{Clear destination layer (bounded).}
    @entry[:source]{Replace destination layer (bounded).}
    @entry[:over]{Draw source layer on top of destination layer (bounded).}
    @entry[:in]{Draw source where there was destination content (unbounded).}
    @entry[:out]{Draw source where there was no destination content
      (unbounded).}
    @entry[:atop]{Draw source on top of destination content and only there.}
    @entry[:dest]{Ignore the source.}
    @entry[:dest-over]{Draw destination on top of source.}
    @entry[:dest-in]{Leave destination only where there was source content
      (unbounded).}
    @entry[:dest-out]{Leave destination only where there was no source content.}
    @entry[:dest-atop]{Leave destination on top of source content and only
      there (unbounded).}
    @entry[:xor]{Source and destination are shown where there is only one of
      them.}
    @entry[:add]{Source and destination layers are accumulated.}
    @entry[:saturate]{Like over, but assuming source and dest are disjoint
      geometries.}
    @entry[:multiply]{Source and destination layers are multiplied. This causes
      the result to be at least as dark as the darker inputs.}
    @entry[:screen]{Source and destination are complemented and multiplied.
      This causes the result to be at least as light as the lighter inputs.}
    @entry[:overlay]{Multiplies or screens, depending on the lightness of the
      destination color.}
    @entry[:darken]{Replaces the destination with the source if it is darker,
      otherwise keeps the source.}
    @entry[:lighten]{Replaces the destination with the source if it is lighter,
      otherwise keeps the source.}
    @entry[:dodge]{Brightens the destination color to reflect the source color.}
    @entry[:burn]{Darkens the destination color to reflect the source color.}
    @entry[:hard-light]{Multiplies or screens, dependent on source color.}
    @entry[:soft-light]{Darkens or lightens, dependent on source color.}
    @entry[:difference]{Takes the difference of the source and destination
      color.}
    @entry[:exclusion]{Produces an effect similar to difference, but with lower
      contrast.}
    @entry[:hsl-hue]{Creates a color with the hue of the source and the
      saturation and luminosity of the target.}
    @entry[:hsl-saturation]{Creates a color with the saturation of the source
      and the hue and luminosity of the target. Painting with this mode onto a
      gray area produces no change.}
    @entry[:hsl-color]{Creates a color with the hue and saturation of the
      source and the luminosity of the target. This preserves the gray levels
      of the target and is useful for coloring monochrome images or tinting
      color images.}
    @entry[:hsl-luinosity]{Creates a color with the luminosity of the source
      and the hue and saturation of the target. This produces an inverse effect
      to the @code{:hsl-color} value.}
  @end{table}
  @see-symbol{cairo:context-t}
  @see-function{cairo:operator}")

(export 'operator-t)

;;; ----------------------------------------------------------------------------
;;; cairo_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct context-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'context-t)
      "CStruct"
      (liber:symbol-documentation 'context-t)
 "@version{2024-2-13}
  @begin{short}
    The @symbol{cairo:context-t} structure contains the current state of the
    rendering device, including coordinates of yet to be drawn shapes.
  @end{short}
  Cairo contexts, as @symbol{cairo:context-t} instances are named, are central
  to Cairo and all drawing with Cairo is always done to a
  @symbol{cairo:context-t} instance.

  Use the @fun{cairo:create} function or the @fun{cairo:with-context} macro to
  create a Cairo context for a target surface. Memory management of the
  @symbol{cairo:context-t} instance is done with the @fun{cairo:reference} and
  @fun{cairo:destroy} functions.
  @see-constructor{cairo:create}
  @see-function{cairo:create}
  @see-macro{cairo:with-context}
  @see-function{cairo:destroy}
  @see-function{cairo:reference}")

(export 'context-t)

;;; ----------------------------------------------------------------------------
;;; cairo:with-context
;;; ----------------------------------------------------------------------------

(defmacro with-context ((context target) &body body)
 #+liber-documentation
 "@version{2024-1-17}
  @syntax{(cairo:with-context (context surface) body) => result}
  @argument[context]{a newly allocated @symbol{cairo:context-t} instance}
  @argument[target]{a @symbol{cairo:surface-t} target surface}
  @begin{short}
    The @symbol{cairo:with-context} macro allocates a new
    @symbol{cairo:context-t} instance for the given @arg{target} and executes
    the body that uses the Cairo context.
  @end{short}
  After execution of the body the allocated memory for the Cairo context is
  released.

  This macro allocates the Cairo context with the @fun{cairo:create} function
  and destroys it with the @fun{cairo:destroy} function.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:create}
  @see-function{cairo:destroy}"
  `(let ((,context (create ,target)))
     (unwind-protect
       (progn ,@body)
       (destroy ,context))))

(export 'with-context)

;;; ----------------------------------------------------------------------------
;;; cairo_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_create" create) (:pointer (:struct context-t))
 #+liber-documentation
 "@version{2024-2-13}
  @argument[target]{a @symbol{cairo:surface-t} target surface for the Cairo
    context}
  @return{A newly allocated @symbol{cairo:context-t} instance.}
  @begin{short}
    Creates a new Cairo context with all graphics state parameters set to
    default values and with @arg{target} as a target surface.
  @end{short}
  The target surface should be constructed with a backend-specific function
  such as the @fun{cairo:image-surface-create} function, or any other variant.

  The initial reference count should be released with the @fun{cairo:destroy}
  function when you are done using the Cairo context. This function will always
  return a valid context. If memory cannot be allocated, a special Cairo context
  will be returned on which the @fun{cairo:status} function returns the
  @code{:no-memory} value. If you attempt to target a surface which does not
  support writing, then a @code{:write-error} value will be raised. You can use
  this object normally, but no drawing will be done.

  This function references @arg{target}, so you can immediately call the
  @fun{cairo:surface-destroy} function on it if you do not need to maintain a
  separate reference to it.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:status}
  @see-function{cairo:destroy}
  @see-function{cairo:image-surface-create}
  @see-function{cairo:surface-destroy}
  @see-macro{cairo:with-context}"
  (target (:pointer (:struct surface-t))))

(export 'create)

;;; ----------------------------------------------------------------------------
;;; cairo_reference ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_reference" reference) (:pointer (:struct context-t))
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @return{The referenced @symbol{cairo:context-t} instance.}
  @begin{short}
    Increases the reference count on @arg{cr} by one.
  @end{short}
  This prevents the Cairo context from being destroyed until a matching call to
  the @fun{cairo:destroy} function is made. The number of references to a Cairo
  context can be get using the @fun{cairo:reference-count} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:destroy}
  @see-function{cairo:reference-count}"
  (cr (:pointer (:struct context-t))))

(export 'reference)

;;; ----------------------------------------------------------------------------
;;; cairo_get_reference_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_reference_count" reference-count) :uint
 #+liber-documentation
 "@version{2024-2-13}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @return{The unsigned integer with the current reference count of @arg{cr}.}
  @begin{short}
    Returns the current reference count of the Cairo context.
  @end{short}
  If the Cairo context is a \"nil\" context, 0 will be returned.
  @see-symbol{cairo:context-t}
  @see-function{cairo:reference}"
  (cr (:pointer (:struct context-t))))

(export 'reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_destroy" destroy) :void
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Decreases the reference count on the Cairo context by one.
  @end{short}
  If the result is zero, then the Cairo context and all associated resources
  are freed. See the @fun{cairo:reference} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:reference}"
  (cr (:pointer (:struct context-t))))

(export 'destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_status ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_status" status) status-t
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @return{The @symbol{cairo:status-t} value for the current status of @arg{cr}.}
  @begin{short}
    Checks whether an error has previously occurred for the Cairo context.
  @end{short}
  See the @symbol{cairo:status-t} enumeration.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:status-t}"
  (cr (:pointer (:struct context-t))))

(export 'status)

;;; ----------------------------------------------------------------------------
;;; cairo_save ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_save" save) :void
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Makes a copy of the current state of @arg{cr} and saves it on an internal
    stack of saved states for @arg{cr}.
  @end{short}
  When the @fun{cairo:restore} function is called, the Cairo context will be
  restored to the saved state. Multiple calls to the @fun{cairo:save} and
  @fun{cairo:restore} functions can be nested. Each call to the
  @fun{cairo:restore} function restores the state from the matching paired
  @fun{cairo:save} call.

  It is not necessary to clear all saved states before a Cairo context is freed.
  If the reference count of a Cairo context drops to zero in response to a call
  to the @fun{cairo:destroy} function, any saved states will be freed along
  with the Cairo context.
  @see-symbol{cairo:context-t}
  @see-function{cairo:restore}
  @see-function{cairo:destroy}"
  (cr (:pointer (:struct context-t))))

(export 'save)

;;; ----------------------------------------------------------------------------
;;; cairo_restore ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_restore" restore) :void
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Restores @arg{cr} to the state saved by a preceding call to the
    @fun{cairo:save} function and removes that state from the stack of saved
    states.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-function{cairo:save}"
  (cr (:pointer (:struct context-t))))

(export 'restore)

;;; ----------------------------------------------------------------------------
;;; cairo_get_target ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_target" target) (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    The @symbol{cairo:surface-t} target surface. This object is owned by Cairo.
  @end{return}
  @begin{short}
    Gets the target surface for the Cairo context as passed to the
    @fun{cairo:create} function.
  @end{short}
  To keep a reference to it, you must call the @fun{cairo:surface-reference}
  function.

  This function will always return a valid pointer, but the result can be a
  \"nil\" surface if @arg{cr} is already in an error state.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:create}
  @see-function{cairo:surface-reference}"
  (cr (:pointer (:struct context-t))))

(export 'target)

;;; ----------------------------------------------------------------------------
;;; cairo_push_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_push_group" push-group) :void
 #+liber-documentation
"@version{#2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Temporarily redirects drawing to an intermediate surface known as a group.
  @end{short}
  The redirection lasts until the group is completed by a call to the
  @fun{cairo:pop-group} or @fun{cairo:pop-group-to-source} function. These
  calls provide the result of any drawing to the group as a pattern, either as
  an explicit object, or set as the source pattern.

  This group functionality can be convenient for performing intermediate
  compositing. One common use of a group is to render objects as opaque within
  the group, so that they occlude each other, and then blend the result with
  translucence onto the destination.

  Groups can be nested arbitrarily deep by making balanced calls to the
  @fun{cairo:push-group} and @fun{cairo:pop-group} functions. Each call pushes
  and pops the new target group onto and from a stack.

  The @fun{cairo:push-group} function calls the @fun{cairo:save} function so
  that any changes to the graphics state will not be visible outside the group,
  the pop group functions call the @fun{cairo:restore} function.

  By default the intermediate group will have a @code{:color-alpha} value of
  the @symbol{cairo:content-t} enumeration. Other content types can be chosen
  for the group by using the @fun{cairo:push-group-with-content} function
  instead.
  @begin[Example]{dictionary}
    As an example, here is how one might fill and stroke a path with
    translucence, but without any portion of the fill being visible under the
    stroke:
    @begin{pre}
(cairo:push-group cr)
(setf (cairo:source cr) fill-pattern)
(cairo:fill-preserve cr)
(setf (cairo:source cr) stroke-pattern)
(cairo:stroke cr)
(cairo:pop-group-to-source cr)
(cairo:paint-with-alpha cr alpha)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:save}
  @see-function{cairo:restore}
  @see-function{cairo:pop-group}
  @see-function{cairo:pop-group-to-source}
  @see-function{cairo:push-group-with-content}"
  (cr (:pointer (:struct context-t))))

(export 'push-group)

;;; ----------------------------------------------------------------------------
;;; cairo_push_group_with_content ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_push_group_with_content" push-group-with-content) :void
 #+liber-documentation
 "@version{#2024-1-22}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[content]{a @symbol{cairo:content-t} value indicating the type of
    group that will be created}
  @begin{short}
    Temporarily redirects drawing to an intermediate surface known as a group.
  @end{short}
  The redirection lasts until the group is completed by a call to the
  @fun{cairo:pop-group} or @fun{cairo:pop-group-to-source} function. These
  calls provide the result of any drawing to the group as a pattern, either as
  an explicit object, or set as the source pattern.

  The ability to control the content type is the only distinction between this
  function and the @fun{cairo:push-group} function which you should see for a
  more detailed description of group rendering.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:pop-group}
  @see-function{cairo:push-group}
  @see-function{cairo:pop-group-to-source}"
  (cr (:pointer (:struct context-t)))
  (content content-t))

(export 'push-group-with-content)

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pop_group" pop-group) (:pointer (:struct pattern-t))
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    A newly created @symbol{cairo:pattern-t} instance containing the results of
    all drawing operations performed to the group. The caller owns the returned
    instance and should call the @fun{cairo:pattern-destroy} function when
    finished with it.
  @end{return}
  @begin{short}
    Terminates the redirection begun by a call to the @fun{cairo:push-group}
    or @fun{cairo:push-group-with-content} function and returns a new pattern
    containing the results of all drawing operations performed to the group.
  @end{short}

  The @fun{cairo:pop-group} function calls the @fun{cairo:restore} function,
  balancing a call to the @fun{cairo:save} function by the push group
  function, so that any changes to the graphics state will not be visible
  outside the group.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo-push-group}
  @see-function{cairo:push-group-with-content}
  @see-function{cairo:restore}
  @see-function{cairo:save}"
  (cr (:pointer (:struct context-t))))

(export 'pop-group)

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group_to_source ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pop_group_to_source" pop-group-to-source) :void
 #+liber-documentation
 "@version{#2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Terminates the redirection begun by a call to the @fun{cairo:push-group}
    or @fun{cairo:push-group-with-content} function and installs the
    resulting pattern as the source pattern in the given Cairo context.
  @end{short}
  The behavior of this function is equivalent to the sequence of operations
  @begin{pre}
(setf group (cairo:pop-group cr))
(setf (cairo:source cr) group)
(cairo:pattern-destroy group)
  @end{pre}
  but is more convenient as their is no need for a variable to store the
  pattern.

  The @fun{cairo:pop-group-to-source} function calls the @fun{cairo:restore}
  function, balancing a call to the @fun{cairo:save} function by the push
  group function, so that any changes to the graphics state will not be visible
  outside the group.
  @see-symbol{cairo:context-t}
  @see-function{cairo:push-group}
  @see-function{cairo:push-group-with-content}
  @see-function{cairo:pop-group}
  @see-function{cairo:save}
  @see-function{cairo:restore}"
  (cr (:pointer (:struct context-t))))

(export 'pop-group-to-source)

;;; ----------------------------------------------------------------------------
;;; cairo_get_group_target ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_group_target" group-target)
    (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{#2024-2-13}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    The @symbol{cairo:surface-t} target surface. This object is owned by Cairo.
    To keep a reference to it, you must call the @fun{cairo:surface-reference}
    function.
  @end{return}
  @begin{short}
    Gets the current destination surface for the Cairo context.
  @end{short}
  This is either the original target surface as passed to the
  @fun{cairo:create} function or the target surface for the current group as
  started by the most recent call to the @fun{cairo:push-group} or
  @fun{cairo:push-group-with-content} functions.

  This function will always return a valid surface, but the result can be a
  \"nil\" surface if @arg{cr} is already in an error state. A \"nil\" surface
  is indicated by a value not equal to the @code{:success} value of the
  @symbol{cairo:status-t} enumeration.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:status-t}
  @see-function{cairo:surface-reference}
  @see-function{cairo:create}
  @see-function{cairo:push-group}
  @see-function{cairo:push-group-with-content}"
  (cr (:pointer (:struct context-t))))

(export 'group-target)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_rgb ()
;;; ----------------------------------------------------------------------------

(defun set-source-rgb (cr red green blue)
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[red]{a number with the red component of the color}
  @argument[green]{a number with the green component of the color}
  @argument[blue]{a number with the blue component of the color}
  @begin{short}
    Sets the source pattern within the Cairo context to an opaque color.
  @end{short}
  This opaque color will then be used for any subsequent drawing operation
  until a new source pattern is set. The color components are floating point
  numbers in the range 0.0 to 1.0. If the values passed in are outside that
  range, they will be clamped.

  The default source pattern is opaque black, that is, it is equivalent to
  @begin{pre}
(cairo:set-source-rgb cr 0.0 0.0 0.0)
  @end{pre}
  @begin[Note]{dictionary}
    The colors are coerced to the @code{double-float} type before being passed
    to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:set-source-rgba}"
  (cffi:foreign-funcall "cairo_set_source_rgb"
                        (:pointer (:struct context-t)) cr
                        :double (coerce red 'double-float)
                        :double (coerce green 'double-float)
                        :double (coerce blue 'double-float)))

(export 'set-source-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_rgba ()
;;; ----------------------------------------------------------------------------

(defun set-source-rgba (cr red green blue alpha)
 #+liber-documentation
 "@version{2024-1-17}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[red]{a number with the red component of the color}
  @argument[green]{a number with the green component of the color}
  @argument[blue]{a number with the blue component of the color}
  @argument[alpha]{a number with the alpha component of the color}
  @begin{short}
    Sets the source pattern within the Cairo context to a translucent color.
  @end{short}
  This color will then be used for any subsequent drawing operation until a new
  source pattern is set. The color and alpha components are floating point
  numbers in the range 0.0 to 1.0. If the values passed in are outside that
  range, they will be clamped.

  The default source pattern is opaque black, that is, it is equivalent to
  @begin{pre}
(cairo:set-source-rgba cr 0.0 0.0 0.0 1.0)
  @end{pre}
  @begin[Note]{dictionary}
    The colors are coerced to the @code{double-float} type before being passed
    to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:set-source-rgb}"
  (cffi:foreign-funcall "cairo_set_source_rgba"
                        (:pointer (:struct context-t)) cr
                        :double (coerce red 'double-float)
                        :double (coerce green 'double-float)
                        :double (coerce blue 'double-float)
                        :double (coerce alpha 'double-float)))

(export 'set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_get_source ()
;;; cairo_set_source ()
;;; ----------------------------------------------------------------------------

(defun (setf source) (source cr)
  (cffi:foreign-funcall "cairo_set_source"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct pattern-t)) source
                        :void)
  source)

(cffi:defcfun ("cairo_get_source" source) (:pointer (:struct pattern-t))
 #+liber-documentation
 "@version{2024-2-13}
  @syntax{(cairo:source cr) => source}
  @syntax{(setf (cairo:source cr) source)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[source]{a @symbol{cairo:pattern-t} instance to be used as the
    source for subsequent drawing operations}
  @begin{short}
   The @fun{cairo:source} function gets the current source pattern for
   @arg{cr}.
  @end{short}
  The @setf{cairo:source} function sets the source pattern within @arg{cr} to
  @arg{source}. This pattern will then be used for any subsequent drawing
  operation until a new source pattern is set.

  The default source pattern is a solid pattern that is opaque black, that
  is, it is equivalent to
  @begin{pre}
(cairo:set-source-rgb cr 0.0 0.0 0.0)
  @end{pre}
  This object is owned by Cairo. To keep a reference to it, you must call the
  @fun{cairo:pattern-reference} function.
  @begin[Note]{dictionary}
    The transformation matrix of the pattern will be locked to the user space
    in effect at the time of the call of the @setf{cairo:source} function. This
    means that further modifications of the current transformation matrix CTM
    will not affect the source pattern. See the @fun{cairo:pattern-matrix}
    function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-reference}
  @see-function{cairo:set-source-rgb}
  @see-function{cairo:pattern-matrix}"
  (cr (:pointer (:struct context-t))))

(export 'source)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_surface ()
;;; ----------------------------------------------------------------------------

(defun set-source-surface (cr surface x y)
 #+liber-documentation
 "@version{2024-1-26}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[surface]{a @symbol{cairo:surface-t} instance to be used to set
    the source pattern}
  @argument[x]{a number with the user-space x coordinate for surface origin}
  @argument[y]{a number with the user-space y coordinate for surface origin}
  @begin{short}
    This is a convenience function for creating a pattern from @arg{surface}
    and setting it as the source in @arg{cr} with the @fun{cairo:source}
    function.
  @end{short}

  The @arg{x} and @arg{y} parameters give the user-space coordinate at which
  the surface origin should appear. The surface origin is its upper-left corner
  before any transformation has been applied. The @arg{x} and @arg{y}
  parameters are negated and then set as translation values in the pattern
  matrix.

  Other than the initial translation pattern matrix, as described above, all
  other pattern attributes, such as its extend mode, are set to the default
  values as in the @fun{cairo:pattern-create-for-surface} function. The
  resulting pattern can be queried with the @fun{cairo:source} function so
  that these attributes can be modified if desired, e.g. to create a repeating
  pattern with with the @fun{cairo:pattern-extend} function.
  @begin[Note]{dictionary}
    The coordinates are coerced to the @code{double-float} type before being
    passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:source}
  @see-function{cairo:pattern-create-for-surface}
  @see-function{cairo:pattern-extend}"
  (cffi:foreign-funcall "cairo_set_source_surface"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct surface-t)) surface
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)))

(export 'set-source-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_get_antialias ()
;;; cairo_set_antialias ()
;;; ----------------------------------------------------------------------------

(defun (setf antialias) (value cr)
  (cffi:foreign-funcall "cairo_set_antialias"
                        (:pointer (:struct context-t)) cr
                        antialias-t value
                        :void)
  value)

(cffi:defcfun ("cairo_get_antialias" antialias) antialias-t
 #+liber-documentation
 "@version{2024-1-17}
  @syntax{(cairo:antialias context) => antialias}
  @syntax{(setf (cairo:antialias context) antialias)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[antialias]{a @symbol{cairo:antialias-t} value}
  @begin{short}
    The @fun{cairo:antialias} function gets the current shape antialiasing
    mode.
  @end{short}
  The @setf{cairo:antialias} function sets the antialiasing mode of the
  rasterizer used for drawing shapes. This value is a hint, and a particular
  backend may or may not support a particular value. At the current time, no
  backend supports the @code{:subpixel} mode when drawing shapes.

  Note that this option does not affect text rendering, instead see the
  @fun{cairo:font-options-antialias} function.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:antialias-t}
  @see-function{cairo:font-options-antialias}"
  (cr (:pointer (:struct context-t))))

(export 'antialias)

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash ()
;;; cairo_set_dash ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_set_dash" %set-dash) :void
  (cr (:pointer (:struct context-t)))
  (dashes (:pointer :double))
  (n :int)
  (offset :double))

(defun (setf dash) (dashes cr offset)
  (let ((n (length dashes)))
    (cffi:with-foreign-object (dashes-array :double n)
      (iter (for i from 0)
            (for dash in dashes)
            (setf (cffi:mem-aref dashes-array :double i)
                  (coerce dash 'double-float)))
      (%set-dash cr
                 dashes-array
                 n
                 (coerce offset 'double-float))))
  (values dashes offset))

(cffi:defcfun ("cairo_get_dash" %get-dash) :void
  (cr (:pointer (:struct context-t)))
  (dashes (:pointer :double))
  (offset (:pointer :double)))

(defun dash (cr)
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:dash cr) => dashes, offset}
  @syntax{(setf (cairo:dash cr offset) dashes)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[dashes]{a list of numbers coerced to double floats specifying
    alternate lengths of on and off stroke portions}
  @argument[offset]{a number coerced to a double float with an offset into the
    dash pattern at which the stroke should start}
  @begin{short}
    The @fun{cairo:dash} function gets the current dash list.
  @end{short}
  The @setf{cairo:dash} function sets the dash pattern to be used by the
  @fun{cairo:stroke} function. A dash pattern is specified by dashes, a list of
  positive values. Each value provides the length of alternate \"on\" and
  \"off\" portions of the stroke. The offset specifies an offset into the
  pattern at which the stroke begins.

  Each \"on\" segment will have caps applied as if the segment were a separate
  sub-path. In particular, it is valid to use an \"on\" length of 0.0 with
  @code{:round} or @code{:square} in order to distributed dots or squares along
  a path.

  If the @arg{dashes} argument is an empty list dashing is disabled. If the
  @arg{dashes} argument has one list element a symmetric pattern is assumed
  with alternating on and off portions of the size specified by the single
  value in @arg{dashes}. If any value in @arg{dashes} is negative, or if all
  values are 0, then @arg{cr} will be put into an error state with a status of
  @code{:invalid-dash}.
  @begin[Note]{dictionary}
    The length values are in user-space units as evaluated at the time of
    stroking. This is not necessarily the same as the user space at the time of
    the call of the @setf{cairo:dash} function.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}"
  (let ((n (dash-count cr)))
    (cffi:with-foreign-objects ((dash-array :double n) (offset :double))
      (%get-dash cr dash-array offset)
      (values (iter (for i below n)
                    (collect (cffi:mem-aref dash-array :double i)))
              (cffi:mem-ref offset :double)))))

(export 'dash)

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_dash_count" dash-count) :int
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    The integer with the length of the dash array, or 0 if no dash array is set.
  @end{return}
  @begin{short}
    This function returns the length of the dash array in @arg{cr}, 0 if
    dashing is not currently in effect.
  @end{short}
  See also the @fun{cairo:dash} function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:dash}"
  (cr (:pointer (:struct context-t))))

(export 'dash-count)

;;; ----------------------------------------------------------------------------
;;; cairo_get_fill_rule ()
;;; cairo_set_fill_rule ()
;;; ----------------------------------------------------------------------------

(defun (setf fill-rule) (rule cr)
  (cffi:foreign-funcall "cairo_set_fill_rule"
                        (:pointer (:struct context-t)) cr
                        fill-rule-t rule
                        :void)
  rule)

(cffi:defcfun ("cairo_get_fill_rule" fill-rule) fill-rule-t
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:fill-rule cr) => rule}
  @syntax{(setf (cairo:fill-rule cr) rule)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[rule]{a @symbol{cairo:fill-rule-t} value for the fill rule}
  @begin{short}
    The @fun{cairo:fill-rule} function gets the current fill rule.
  @end{short}
  The @setf{cairo:fill-rule} function sets the current fill rule within the
  Cairo context.

  The fill rule is used to determine which regions are inside or outside a
  complex, potentially self-intersecting, path. The current fill rule affects
  both the @fun{cairo:fill} and @fun{cairo:clip} functions. See the
  @symbol{cairo:fill-rule-t} enumeration for details on the semantics of each
  available fill rule.

  The default fill rule is the @code{:winding} value.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:fill-rule-t}
  @see-function{cairo:fill}
  @see-function{cairo:clip}"
  (cr (:pointer (:struct context-t))))

(export 'fill-rule)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_cap ()
;;; cairo_set_line_cap ()
;;; ----------------------------------------------------------------------------

(defun (setf line-cap) (cap cr)
  (cffi:foreign-funcall "cairo_set_line_cap"
                        (:pointer (:struct context-t)) cr
                        line-cap-t cap
                        :void)
  cap)

(cffi:defcfun ("cairo_get_line_cap" line-cap) line-cap-t
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:line-cap cr) => cap}
  @syntax{(setf (cairo:line-cap cr) cap)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[cap]{a @symbol{cairo:line-cap-t} value for the line cap style}
  @begin{short}
    The @fun{cairo:line-cap} function gets the current line cap style.
  @end{short}
  The @setf{cairo:line-cap} function sets the current line cap style within the
  Cairo context. See the @symbol{cairo:line-cap-t} enumeration for details about
  how the available line cap styles are drawn.

  As with the other stroke parameters, the current line cap style is examined
  by the @fun{cairo:stroke} and @fun{cairo:stroke-extents} functions, but
  does not have any effect during path construction.

  The default line cap style is the @code{:butt} value.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:line-cap-t}
  @see-function{cairo:stroke}
  @see-function{cairo:stroke-extents}"
  (cr (:pointer (:struct context-t))))

(export 'line-cap)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_join ()
;;; cairo_set_line_join ()
;;; ----------------------------------------------------------------------------

(defun (setf line-join) (join cr)
  (cffi:foreign-funcall "cairo_set_line_join"
                        (:pointer (:struct context-t)) cr
                        line-join-t join
                        :void)
  join)

(cffi:defcfun ("cairo_get_line_join" line-join) line-join-t
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:line-join cr) => join}
  @syntax{(setf (cairo:line-join cr) join)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[join]{a @symbol{cairo:line-join-t} value for the line join style}
  @begin{short}
    The @fun{cairo:line-join} funcion gets the current line join style.
  @end{short}
  The @setf{cairo:line-join} function sets the current line join style within
  the Cairo context. See the @symbol{cairo:line-join-t} enumeration for details
  about how the available line join styles are drawn.

  As with the other stroke parameters, the current line join style is examined
  by the @fun{cairo:stroke} and @fun{cairo:stroke-extents} functions, but
  does not have any effect during path construction.

  The default line join style is the @code{:miter} value.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:line-join-t}
  @see-function{cairo:stroke}
  @see-function{cairo:stroke-extents}"
  (cr (:pointer (:struct context-t))))

(export 'line-join)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_width ()
;;; cairo_set_line_width ()
;;; ----------------------------------------------------------------------------

(defun (setf line-width) (width cr)
  (cffi:foreign-funcall "cairo_set_line_width"
                        (:pointer (:struct context-t)) cr
                        :double (coerce width 'double-float)
                        :void)
  width)

(cffi:defcfun ("cairo_get_line_width" line-width) :double
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:line-width cr) => width}
  @syntax{(setf (cairo:line-width cr) width)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[width]{a number coerced to a double float with the line width}
  @begin{short}
    The @fun{cairo:line-width} function returns the current line width value.
  @end{short}
  The @setf{cairo:line-width} function sets the current line width within the
  Cairo context. Note that the value is unchanged even if the Coordinate
  Transformation Matrix (CTM) has changed between the calls to the
  @fun{cairo:line-width} and @setf{cairo:line-width} functions.

  The line width value specifies the diameter of a pen that is circular in user
  space, though device-space pen may be an ellipse in general due to
  scaling/shear/rotation of the Coordinate Transformation Matrix (CTM).

  As with the other stroke parameters, the current line width is examined by
  the @fun{cairo:stroke} and @fun{cairo:stroke-extents} functions, but does
  not have any effect during path construction.

  The default line width value is 2.0.
  @begin[Note]{dictionary}
    When the description above refers to user space and CTM it refers to the
    user space and CTM in effect at the time of the stroking operation, not
    the user space and CTM in effect at the time of the call to the
    @setf{cairo:line-width} function. The simplest usage makes both of these
    spaces identical. That is, if there is no change to the CTM between a call
    to the @setf{cairo:line-width} function and the stroking operation, then one
    can just pass user-space values to the @setf{cairo:line-width} function and
    ignore this note.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}
  @see-function{cairo:stroke-extents}"
  (cr (:pointer (:struct context-t))))

(export 'line-width)

;;; ----------------------------------------------------------------------------
;;; cairo_get_hairline ()
;;; cairo_set_hairline ()
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(defun (setf hairline) (value cr)
  (cffi:foreign-funcall "cairo_set_hairline"
                        (:pointer (:struct context-t)) cr
                        :bool value
                        :void)
  value)

#+cairo-1-18
(cffi:defcfun ("cairo_get_hairline" hairline) :bool
 #+liber-documentation
 "@version{2024-2-13}
  @syntax{(cairo:hairline cr) => setting}
  @syntax{(setf (cairo:hairline cr) setting)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[setting]{a boolean whether hairline mode is set}
  @begin{short}
    The @fun{cairo:hairline} function returns whether or not hairline mode is
    set.
  @end{short}
  The @setf{cairo:hairline} functon sets lines within the Cairo context to be
  hairlines. Hairlines are logically zero-width lines that are drawn at the
  thinnest renderable width possible in the current Cairo context.

  On surfaces with native hairline support, the native hairline functionality
  will be used. Surfaces that support hairlines include:
  @begin[arg]{table}
    @entry[pdf/ps]{Encoded as 0-width line.}
    @entry[win32_printing]{Rendered with the @code{PS_COSMETIC} pen.}
    @entry[svg]{Encoded as 1 px non-scaling-stroke.}
    @entry[script]{Encoded with the @code{set-hairline} function.}
  @end{table}
  Cairo will always render hairlines at 1 device unit wide, even if an
  anisotropic scaling was applied to the stroke width. In the wild, handling
  of this situation is not well-defined. Some PDF, PS, and SVG renderers match
  the output of Cairo, but some very popular implementations (Acrobat, Chrome,
  rsvg) will scale the hairline unevenly. As such, best practice is to reset
  any anisotropic scaling before calling the @fun{cairo:stroke} function. See
  @url[https://cairographics.org/cookbook/ellipses/]{Ellipses With Uniform
  Stroke Width} for an example.

  Since 1.18
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}"
  (cr (:pointer (:struct context-t))))

(export 'hairline)

;;; ----------------------------------------------------------------------------
;;; cairo_get_miter_limit ()
;;; cairo_set_miter_limit ()
;;; ----------------------------------------------------------------------------

(defun (setf miter-limit) (limit cr)
  (let ((limit (coerce limit 'double-float)))
    (cffi:foreign-funcall "cairo_set_miter_limit"
                          (:pointer (:struct context-t)) cr
                          :double limit
                          :void)
    limit))

(cffi:defcfun ("cairo_get_miter_limit" miter-limit) :double
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:miter-limit cr) => limit}
  @syntax{(setf (cairo:miter-limit cr) limit)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[limit]{a number coerced to a double float with the miter limit}
  @begin{short}
    The @fun{cairo:miter-limit} function gets the current miter limit.
    function.
  @end{short}
  The @setf{cairo:miter-limit} function sets the current miter limit within the
  Cairo context.

  If the current line join style is set to the @code{:miter} value, see the
  @fun{cairo:line-join} function, the miter limit is used to determine
  whether the lines should be joined with a bevel instead of a miter. Cairo
  divides the length of the miter by the line width. If the result is greater
  than the miter limit, the style is converted to a bevel.

  As with the other stroke parameters, the current line miter limit is examined
  by the @fun{cairo:stroke} and @fun{cairo:stroke-extents} functions, but does
  not have any effect during path construction.

  The default miter limit value is 10.0, which will convert joins with interior
  angles less than 11 degrees to bevels instead of miters. For reference, a
  miter limit of 2.0 makes the miter cutoff at 60 degrees, and a miter limit of
  1.414 makes the cutoff at 90 degrees.

  A miter limit for a desired angle in radians can be computed as:
  @begin{pre}
(setf limit (/ 1.0d0 (sin (/ angle 2.0d0))))
  @end{pre}
  @see-symbol{cairo:context-t}
  @see-function{cairo:line-join}
  @see-function{cairo:stroke}
  @see-function{cairo:stroke-extents}"
  (cr (:pointer (:struct context-t))))

(export 'miter-limit)

;;; ----------------------------------------------------------------------------
;;; cairo_get_operator ()
;;; cairo_set_operator ()
;;; ----------------------------------------------------------------------------

(defun (setf operator) (op cr)
  (cffi:foreign-funcall "cairo_set_operator"
                        (:pointer (:struct context-t)) cr
                        operator-t op
                        :void)
  op)

(cffi:defcfun ("cairo_get_operator" operator) operator-t
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:operator cr) => op}
  @syntax{(setf (cairo:operator cr) op)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[op]{a compositing operator, specified as a @symbol{cairo:operator-t}
    value}
  @begin{short}
    The @fun{cairo:operator} function gets the current compositing operator
    for a Cairo context.
  @end{short}
  The @setf{cairo:operator} function sets the compositing operator to be used
  for all drawing operations. See the @symbol{cairo:operator-t} enumeration for
  details on the semantics of each available compositing operator.

  The default operator is the @code{:over} value.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:operator-t}"
  (cr (:pointer (:struct context-t))))

(export 'operator)

;;; ----------------------------------------------------------------------------
;;; cairo_get_tolerance ()
;;; cairo_set_tolerance ()
;;; ----------------------------------------------------------------------------

(defun (setf tolerance) (tolerance cr)
  (let ((tolerance (coerce tolerance 'double-float)))
    (cffi:foreign-funcall "cairo_set_tolerance"
                          (:pointer (:struct context-t)) cr
                          :double tolerance
                          :void)
    tolerance))

(cffi:defcfun ("cairo_get_tolerance" tolerance) :double
 #+liber-documentation
 "@version{2024-1-18}
  @syntax{(cairo:tolerance cr) => tolerance}
  @syntax{(setf (cairo:tolerance cr) tolerance)}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[tolerance]{a number coerced to a double float with the tolerance,
    in device units, typically pixels}
  @begin{short}
    The @fun{cairo:tolerance} function gets the current tolerance value.
  @end{short}
  The @setf{cairo:tolerance} functions sets the tolerance used when converting
  paths into trapezoids.

  Curved segments of the path will be subdivided until the maximum deviation
  between the original path and the polygonal approximation is less than
  @arg{tolerance}. The default value is 0.1. A larger value will give better
  performance, a smaller value, better appearance. Reducing the value from the
  default value of 0.1 is unlikely to improve appearance significantly. The
  accuracy of paths within Cairo is limited by the precision of its internal
  arithmetic, and the prescribed tolerance is restricted to the smallest
  representable internal value.
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct context-t))))

(export 'tolerance)

;;; ----------------------------------------------------------------------------
;;; cairo_clip ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_clip" clip) :void
 #+liber-documentation
 "@version{2024-1-26}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Establishes a new clip region by intersecting the current clip region with
    the current path as it would be filled by the @fun{cairo:fill} function
    and according to the current fill rule, see the @fun{cairo:fill-rule}
    function.
  @end{short}

  After a call of the @fun{cairo:clip} function, the current path will be
  cleared from the Cairo context.

  The current clip region affects all drawing operations by effectively
  masking out any changes to the surface that are outside the current clip
  region.

  Calling the @fun{cairo:clip} function can only make the clip region smaller,
  never larger. But the current clip is part of the graphics state, so a
  temporary restriction of the clip region can be achieved by calling
  the @fun{cairo:clip} function within a
  @fun{cairo:save}/@fun{cairo:restore} pair. The only other means of
  increasing the size of the clip region is the @fun{cairo:reset-clip}
  function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill}
  @see-function{cairo:fill-rule}
  @see-function{cairo:save}
  @see-function{cairo:restore}
  @see-function{cairo:reset-clip}"
  (cr (:pointer (:struct context-t))))

(export 'clip)

;;; ----------------------------------------------------------------------------
;;; cairo_clip_preserve ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_clip_preserve" clip-preserve) :void
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Establishes a new clip region by intersecting the current clip region with
    the current path as it would be filled by the @fun{cairo:fill} function
    and according to the current fill rule.
  @end{short}
  See the @fun{cairo:fill-rule} function.

  Unlike the @fun{cairo:clip} function, the @fun{cairo:clip-preserve} function
  preserves the path within the Cairo context.

  The current clip region affects all drawing operations by effectively masking
  out any changes to the surface that are outside the current clip region.

  Calling the @fun{cairo:clip-preserve} function can only make the clip region
  smaller, never larger. But the current clip is part of the graphics state, so
  a temporary restriction of the clip region can be achieved by calling the
  @fun{cairo:clip-preserve} function within a
  @fun{cairo:save}/@fun{cairo:restore} pair. The only other means of
  increasing the size of the clip region is the @fun{cairo:reset-clip}
  function.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill-rule}
  @see-function{cairo:clip}
  @see-function{cairo:save}
  @see-function{cairo:restore}
  @see-function{cairo:reset-clip}"
  (cr (:pointer (:struct context-t))))

(export 'clip-preserve)

;;; ----------------------------------------------------------------------------
;;; cairo_clip_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_clip_extents" %clip-extents) :void
  (cr (:pointer (:struct context-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun clip-extents (cr)
 #+liber-documentation
 "@version{2024-1-26}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{x1} -- a double float for the left of the resulting extents @br{}
    @arg{y1} -- a double float for the top of the resulting extents @br{}
    @arg{x2} -- a double float for the right of the resulting extents @br{}
    @arg{y2} -- a double float for the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user coordinates covering the area inside the
    current clip.
  @end{short}
  @see-symbol{cairo:context-t}"
  (cffi:with-foreign-objects ((x1 :double)
                              (y1 :double)
                              (x2 :double)
                              (y2 :double))
    (%clip-extents cr x1 y1 x2 y2)
    (values (cffi:mem-ref x1 :double)
            (cffi:mem-ref y1 :double)
            (cffi:mem-ref x2 :double)
            (cffi:mem-ref y2 :double))))

(export 'clip-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_in_clip ()
;;; ----------------------------------------------------------------------------

(defun in-clip (cr x y)
 #+liber-documentation
 "@version{2024-1-26}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    point to test}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    point to test}
  @return{@em{True} if the point is inside, or @em{false} if outside.}
  @begin{short}
    Tests whether the given point is inside the area that would be visible
    through the current clip, i.e. the area that would be filled by a call to
    the @fun{cairo:paint} function.
  @end{short}

  See the @fun{cairo:clip} and @fun{cairo:clip-preserve} functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:paint}
  @see-function{cairo:clip}
  @see-function{cairo:clip-preserve}"
  (cffi:foreign-funcall "cairo_in_clip"
                        (:pointer (:struct context-t)) cr
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)
                        :bool))

(export 'in-clip)

;;; ----------------------------------------------------------------------------
;;; cairo_reset_clip ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_reset_clip" reset-clip) :void
 #+liber-documentation
 "@version{2024-1-26}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Reset the current clip region to its original, unrestricted state.
  @end{short}
  That is, set the clip region to an infinitely large shape containing the
  target surface. Equivalently, if infinity is too hard to grasp, one can
  imagine the clip region being reset to the exact bounds of the target surface.

  Note that code meant to be reusable should not call the @fun{cairo:reset-clip}
  function as it will cause results unexpected by higher-level code which calls
  the @fun{cairo:clip} function. Consider using the @fun{cairo:save} and
  @fun{cairo:restore} functions around the @fun{cairo:clip} function as a more
  robust means of temporarily restricting the clip region.
  @see-symbol{cairo:context-t}
  @see-function{cairo:clip}
  @see-function{cairo:save}
  @see-function{cairo:restore}"
  (cr (:pointer (:struct context-t))))

(export 'reset-clip)

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_list_destroy ()                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_rectangle_list_destroy" rectangle-list-destroy) :void
 #+liber-documentation
 "@version{2024-1-19}
  @argument[rectangles]{a pointer to a @symbol{cairo:rectangle-list-t} instance,
    as obtained from the @fun{cairo:copy-clip-rectangle-list} function}
  @begin{short}
    Unconditionally frees @arg{rectangles} and all associated references.
  @end{short}
  After this call, the @arg{rectangles} pointer must not be dereferenced.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:rectangle-list-t}
  @see-function{cairo:copy-clip-rectangle-list}"
  (rectangles (:pointer (:struct rectangle-list-t))))

;;; ----------------------------------------------------------------------------
;;; cairo_copy_clip_rectangle_list ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_clip_rectangle_list" %copy-clip-rectangle-list)
    (:pointer (:struct rectangle-list-t))
  (cr (:pointer (:struct context-t))))

(defun copy-clip-rectangle-list (cr)
 #+liber-documentation
 "@version{2024-1-19}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    The current clip region as a list of rectangles in user coordinates. Each
    rectangle is represented by a list with the @arg{x}, @arg{y}, @arg{width},
    @arg{height} values of the rectangle.
  @end{return}
  @begin{short}
    Gets the current clip region as a list of rectangles in user coordinates.
  @end{short}
  If the clip region cannot be represented as a list of user-space rectangles
  the @code{nil} value is returned.
  @begin[Example]{dictionary}
    @begin{pre}
;; Creat a recording surface
(cairo:with-recording-surface (surface :color)
  :; Create a context for the recording surface
  (cairo:with-context (context surface)
    ;; Two rectangles
    (cairo:rectangle context 10 10 15 15)
    (cairo:rectangle context 20 20 10 10)
    ;; Clip the context
    (cairo:clip context)
    ;; Get the current clip region
    (cairo:copy-clip-rectangle-list context)))
=> ((10.0d0 10.0d0 15.0d0 10.0d0)
    (10.0d0 20.0d0 20.0d0 5.0d0)
    (20.0d0 25.0d0 10.0d0 5.0d0))
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:context-t}"
  (let ((rectlist (%copy-clip-rectangle-list cr)))
    (when (eq :success
              (cffi:foreign-slot-value rectlist
                                       '(:struct rectangle-list-t)
                                       'status))
      (let ((n (cffi:foreign-slot-value rectlist
                                        '(:struct rectangle-list-t)
                                        'num-rectangles))
            (rectangles (cffi:foreign-slot-value rectlist
                                                 '(:struct rectangle-list-t)
                                                 'rectangles)))
        (prog1
          (iter (for i from 0 below n)
                (for rectangle = (cffi:mem-aptr rectangles
                                                '(:struct rectangle-t)
                                                i))
                (collect (list (cffi:foreign-slot-value rectangle
                                                        '(:struct rectangle-t)
                                                        'x)
                               (cffi:foreign-slot-value rectangle
                                                        '(:struct rectangle-t)
                                                        'y)
                               (cffi:foreign-slot-value rectangle
                                                        '(:struct rectangle-t)
                                                        'width)
                               (cffi:foreign-slot-value rectangle
                                                        '(:struct rectangle-t)
                                                        'height))))
          (rectangle-list-destroy rectlist))))))

(export 'copy-clip-rectangle-list)

;;; ----------------------------------------------------------------------------
;;; cairo_fill ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_fill" fill) :void
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    A drawing operator that fills the current path according to the current
    fill rule, each sub-path is implicitly closed before being filled.
  @end{short}
  After the @fun{cairo:fill} function, the current path will be cleared from
  the Cairo context. See the @fun{cairo:fill-rule} and @fun{cairo:fill-preserve}
  functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill-rule}
  @see-function{cairo:fill-preserve}"
  (cr (:pointer (:struct context-t))))

(export 'fill)

;;; ----------------------------------------------------------------------------
;;; cairo_fill_preserve ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_fill_preserve" fill-preserve) :void
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    A drawing operator that fills the current path according to the current
    fill rule, each sub-path is implicitly closed before being filled.
  @end{short}
  Unlike the @fun{cairo:fill} function, the @fun{cairo:fill-preserve} function
  preserves the path within the Cairo context.

  See the @fun{cairo:fill-rule} and @fun{cairo:fill} functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill}
  @see-function{cairo:fill-rule}"
  (cr (:pointer (:struct context-t))))

(export 'fill-preserve)

;;; ----------------------------------------------------------------------------
;;; cairo_fill_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_fill_extents" %fill-extents) :void
  (cr (:pointer (:struct context-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun fill-extents (cr)
 #+liber-documentation
 "@version{#2023-1-11}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{x1} -- a double float with the left of the resulting extents @br{}
    @arg{y1} -- a double float with the top of the resulting extents @br{}
    @arg{x2} -- a double float with the right of the resulting extents @br{}
    @arg{y2} -- a double float with the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user coordinates covering the area that would be
    affected, the \"inked\" area, by a @fun{cairo:fill} operation given the
    current path and fill parameters.
  @end{short}
  If the current path is empty, returns an empty rectangle. Surface dimensions
  and clipping are not taken into account.

  Contrast with the @fun{cairo:path-extents} function, which is similar, but
  returns non-zero extents for some paths with no inked area, such as a simple
  line segment.

  Note that the @fun{cairo:fill-extents} function must necessarily do more
  work to compute the precise inked areas in light of the fill rule, so the
  @fun{cairo:path-extents} function may be more desirable for sake of
  performance if the non-inked path extents are desired.

  See the @fun{cairo:fill}, @fun{cairo:fill-rule} and @fun{cairo:fill-preserve}
  functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill}
  @see-function{cairo:path-extents}
  @see-function{cairo:fill-rule}
  @see-function{cairo:fill-preserve}"
  (cffi:with-foreign-objects ((x1 :double)
                              (y1 :double)
                              (x2 :double)
                              (y2 :double))
    (%fill-extents cr x1 y1 x2 y2)
    (values (cffi:mem-ref x1 :double)
            (cffi:mem-ref y1 :double)
            (cffi:mem-ref x2 :double)
            (cffi:mem-ref y2 :double))))

(export 'fill-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_in_fill ()
;;; ----------------------------------------------------------------------------

(defun in-fill (cr x y)
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    point to test}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    point to test}
  @return{@em{True} if the point is inside, or @em{false} if outside.}
  @begin{short}
    Tests whether the given point is inside the area that would be affected by
    a @fun{cairo:fill} operation given the current path and filling
    parameters.
  @end{short}
  Surface dimensions and clipping are not taken into account.

  See the @fun{cairo:fill}, @fun{cairo:fill-rule} and @fun{cairo:fill-preserve}
  functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:fill}
  @see-function{cairo:fill-rule}
  @see-function{cairo:fill-preserve}"
  (cffi:foreign-funcall "cairo_in_fill"
                        (:pointer (:struct context-t)) cr
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)
                        :bool))

(export 'in-fill)

;;; ----------------------------------------------------------------------------
;;; cairo_mask ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mask" mask) :void
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{short}
    A drawing operator that paints the current source using the alpha channel
    of @arg{pattern} as a mask.
  @end{short}
  Opaque areas of @arg{pattern} are painted with the source, transparent areas
  are not painted.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:pattern-t}"
  (cr (:pointer (:struct context-t)))
  (pattern (:pointer (:struct pattern-t))))

(export 'mask)

;;; ----------------------------------------------------------------------------
;;; cairo_mask_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mask_surface" %mask-surface) :void
  (cr (:pointer (:struct context-t)))
  (surface (:pointer (:struct surface-t)))
  (x :double)
  (y :double))

(defun mask-surface (cr surface x y)
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate at
    which to place the origin of @arg{surface}}
  @argument[y]{a number coerced to a double float with the y coordinate at
    which to place the origin of @arg{surface}}
  @begin{short}
    A drawing operator that paints the current source using the alpha channel
    of @arg{surface} as a mask.
  @end{short}
  Opaque areas of @arg{surface} are painted with the source, transparent areas
  are not painted.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}"
  (%mask-surface cr
                 surface
                 (coerce x 'double-float)
                 (coerce y 'double-float)))

(export 'mask-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_paint ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_paint" paint) :void
 #+liber-documentation
 "@version{2024-2-13}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    A drawing operator that paints the current source everywhere within the
    current clip region.
  @end{short}
  @begin[Example]{dictionary}
    Code fragment to paint the background with a given color.
    @begin{pre}
;; Paint the white color on the background
(cairo:set-source-rgb cr 1.0 1.0 1.0)
(cairo:paint cr)
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct context-t))))

(export 'paint)

;;; ----------------------------------------------------------------------------
;;; cairo_paint_with_alpha ()
;;; ----------------------------------------------------------------------------

(defun paint-with-alpha (cr alpha)
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[alpha]{a number coerced to a double float with the alpha value,
    between 0.0 (transparent) and 1.0 (opaque)}
  @begin{short}
    A drawing operator that paints the current source everywhere within the
    current clip region using a mask of constant alpha value @arg{alpha}.
  @end{short}
  The effect is similar to the @fun{cairo:paint} function, but the drawing is
  faded out using the alpha value.
  @see-symbol{cairo:context-t}
  @see-function{cairo:paint}"
  (cffi:foreign-funcall "cairo_paint_with_alpha"
                        (:pointer (:struct context-t)) cr
                        :double (coerce alpha 'double-float)))

(export 'paint-with-alpha)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_stroke" stroke) :void
 #+liber-documentation
 "@version{2023-1-11}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    A drawing operator that strokes the current path according to the current
    line width, line join, line cap, and dash settings.
  @end{short}
  After the @fun{cairo:stroke} function, the current path will be cleared from
  the Cairo context. See the @fun{cairo:line-width}, @fun{cairo:line-join},
  @fun{cairo:line-cap}, @fun{cairo:dash}, and @fun{cairo:stroke-preserve}
  functions.
  @begin[Note]{dictionary}
    Degenerate segments and sub-paths are treated specially and provide
    a useful result. These can result in two different situations:
    @begin{enumerate}
      @begin{item}
        Zero-length \"on\" segments set in the @fun{cairo:dash} function.
        If the @symbol{cairo:line-cap-t} style is @code{:round} or
        @code{:square} then these segments will be drawn as circular dots or
        squares respectively. In the case of @code{:square}, the orientation of
        the squares is determined by the direction of the underlying path.
      @end{item}
      @begin{item}
        A sub-path created by the @fun{cairo:move-to} function followed by
        either a call to the @fun{cairo:close-path} function or one or more
        calls to the @fun{cairo:line-to} function to the same coordinate as the
        the @fun{cairo:move-to} function. If the @symbol{cairo:line-cap-t} style
        is @code{:round} then these sub-paths will be drawn as circular dots.
        Note that in the case of @code{:square} a degenerate sub-path will not
        be drawn at all, since the correct orientation is indeterminate.
      @end{item}
    @end{enumerate}
    In no case will a @symbol{cairo:line-cap-t} style of @code{:butt} cause
    anything to be drawn in the case of either degenerate segments or sub-paths.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:line-cap-t}
  @see-function{cairo:line-width}
  @see-function{cairo:line-join}
  @see-function{cairo:line-cap}
  @see-function{cairo:dash}
  @see-function{cairo:stroke-preserve}
  @see-function{cairo:move-to}
  @see-function{cairo:close-path}
  @see-function{cairo:line-to}"
  (cr (:pointer (:struct context-t))))

(export 'stroke)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_preserve ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_stroke_preserve" stroke-preserve) :void
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    A drawing operator that strokes the current path according to the current
    line width, line join, line cap, and dash settings.
  @end{short}
  Unlike the @fun{cairo:stroke} function, the @fun{cairo:stroke-preserve}
  function preserves the path within the Cairo context.

  See the @fun{cairo:line-width}, @fun{cairo:line-join}, @fun{cairo:line-cap},
  and @fun{cairo:dash} functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}
  @see-function{cairo:line-width}
  @see-function{cairo:line-join}
  @see-function{cairo:line-cap}
  @see-function{cairo:dash}"
  (cr (:pointer (:struct context-t))))

(export 'stroke-preserve)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_stroke_extents" %stroke-extents) :void
  (cr (:pointer (:struct context-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun stroke-extents (cr)
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{return}
    @arg{x1} -- a double float with the left of the resulting extents @br{}
    @arg{y1} -- a double float with the top of the resulting extents @br{}
    @arg{x2} -- a double float with the right of the resulting extents @br{}
    @arg{y2} -- a double float with the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user coordinates covering the area that would be
    affected, the \"inked\" area, by a @fun{cairo:stroke} operation given the
    current path and stroke parameters.
  @end{short}
  If the current path is empty, returns an empty rectangle. Surface dimensions
  and clipping are not taken into account.

  Note that if the line width is set to exactly zero, then the
  @fun{cairo:stroke-extents} function will return an empty rectangle. Contrast
  with the @fun{cairo:path-extents} function which can be used to compute the
  non-empty bounds as the line width approaches zero.

  Note that the @fun{cairo:stroke-extents} function must necessarily do more
  work to compute the precise inked areas in light of the stroke parameters, so
  the @fun{cairo:path-extents} function may be more desirable for sake of
  performance if non-inked path extents are desired.

  See the @fun{cairo:stroke}, @fun{cairo:line-width}, @fun{cairo:line-join},
  @fun{cairo:line-cap}, @fun{cairo:dash}, and @fun{cairo:stroke-preserve}
  functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}
  @see-function{cairo:path-extents}
  @see-function{cairo:line-width}
  @see-function{cairo:line-join}
  @see-function{cairo:line-cap}
  @see-function{cairo:dash}
  @see-function{cairo:stroke-preserve}"
  (cffi:with-foreign-objects ((x1 :double)
                              (y1 :double)
                              (x2 :double)
                              (y2 :double))
    (%stroke-extents cr x1 y1 x2 y2)
    (values (cffi:mem-ref x1 :double)
            (cffi:mem-ref y1 :double)
            (cffi:mem-ref x2 :double)
            (cffi:mem-ref y2 :double))))

(export 'stroke-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_in_stroke ()
;;; ----------------------------------------------------------------------------

(defun in-stroke (cr x y)
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    point to test}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    point to test}
  @return{@em{True} if the point is inside, or @em{false} if outside.}
  @begin{short}
    Tests whether the given point is inside the area that would be affected by
    a @fun{cairo:stroke} operation given the current path and stroking
    parameters.
  @end{short}
  Surface dimensions and clipping are not taken into account.

  See the @fun{cairo:stroke}, @fun{cairo:line-width}, @fun{cairo:line-join},
  @fun{cairo:line-cap}, @fun{cairo:dash}, and @fun{cairo:stroke-preserve}
  functions.
  @see-symbol{cairo:context-t}
  @see-function{cairo:stroke}
  @see-function{cairo:line-width}
  @see-function{cairo:line-join}
  @see-function{cairo:line-cap}
  @see-function{cairo:dash}
  @see-function{cairo:stroke-preserve}"
  (cffi:foreign-funcall "cairo_in_stroke"
                        (:pointer (:struct context-t)) cr
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)
                        :bool))

(export 'in-stroke)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_page ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_copy_page" copy-page) :void
 #+liber-documentation
 "@version{#2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Emits the current page for backends that support multiple pages, but does
    not clear it, so, the contents of the current page will be retained for the
    next page too.
  @end{short}
  Use the @fun{cairo:show-page} function if you want to get an empty page
  after the emission.

  This is a convenience function that simply calls the
  @fun{cairo:surface-copy-page} function on the target of @arg{cr}.
  @see-symbol{cairo:context-t}
  @see-function{cairo:show-page}
  @see-function{cairo:surface-copy-page}"
  (cr (:pointer (:struct context-t))))

(export 'copy-page)

;;; ----------------------------------------------------------------------------
;;; cairo_show_page ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_show_page" show-page) :void
 #+liber-documentation
 "@version{2024-1-18}
  @argument[cr]{a @symbol{cairo:context-t} instance}
  @begin{short}
   Emits and clears the current page for backends that support multiple pages.
  @end{short}
  Use the @fun{cairo:copy-page} function if you do not want to clear the page.

  This is a convenience function that simply calls the
  @fun{cairo:surface-show-page} function on the target of @arg{cr}.
  @see-symbol{cairo:context-t}
  @see-function{cairo:copy-page}
  @see-function{cairo:surface-show-page}"
  (cr (:pointer (:struct context-t))))

(export 'show-page)

;;; ----------------------------------------------------------------------------
;;; cairo_set_user_data ()
;;;
;;; cairo_status_t cairo_set_user_data (cairo_t *cr,
;;;                                     const cairo_user_data_key_t *key,
;;;                                     void *user_data,
;;;                                     cairo_destroy_func_t destroy);
;;;
;;; Attach user data to cr. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_user_data ()
;;;
;;; void * cairo_get_user_data (cairo_t *cr, const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to cr using the specified key. If no
;;; user data has been attached with the given key this function returns NULL.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.context.lisp -----------------------------------------
