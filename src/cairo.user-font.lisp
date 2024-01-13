;;; ----------------------------------------------------------------------------
;;; cairo.user-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2024 Dieter Kaiser
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
;;; User Fonts
;;;
;;;     Font support with font data provided by the user
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_USER_FONT
;;;
;;; Functions
;;;
;;;     cairo_user_scaled_font_init_func_t
;;;     cairo_user_scaled_font_render_glyph_func_t
;;;     cairo_user_scaled_font_text_to_glyphs_func_t
;;;     cairo_user_scaled_font_unicode_to_glyph_func_t
;;;
;;;     cairo_user_font_face_create
;;;     cairo_user_font_face_set_init_func
;;;     cairo_user_font_face_get_init_func
;;;     cairo_user_font_face_set_render_glyph_func
;;;     cairo_user_font_face_get_render_glyph_func
;;;     cairo_user_font_face_set_render_color_glyph_func   Since 1.18
;;;     cairo_user_font_face_get_render_color_glyph_func   Since 1.18
;;;     cairo_user_font_face_set_unicode_to_glyph_func
;;;     cairo_user_font_face_get_unicode_to_glyph_func
;;;     cairo_user_font_face_set_text_to_glyphs_func
;;;     cairo_user_font_face_get_text_to_glyphs_func
;;;     cairo_user_scaled_font_get_foreground_marker       Since 1.18
;;;     cairo_user_scaled_font_get_foreground_source       Since 1.18
;;;
;;; Description
;;;
;;; The user-font feature allows the cairo user to provide drawings for glyphs
;;; in a font. This is most useful in implementing fonts in non-standard
;;; formats, like SVG fonts and Flash fonts, but can also be used by games and
;;; other application to draw "funky" fonts.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_USER_FONT
;;;
;;; #define CAIRO_HAS_USER_FONT 1
;;;
;;; Defined if the user font backend is available. This macro can be used to
;;; conditionally compile backend-specific code. The user font backend is
;;; always built in versions of cairo that support this feature (1.8 and later).
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_init_func_t ()
;;;
;;; cairo_status_t
;;; (*cairo_user_scaled_font_init_func_t) (cairo_scaled_font_t *scaled_font,
;;;                                        cairo_t *cr,
;;;                                        cairo_font_extents_t *extents);
;;;
;;; cairo_user_scaled_font_init_func_t is the type of function which is called
;;; when a scaled-font needs to be created for a user font-face.
;;;
;;; The cairo context cr is not used by the caller, but is prepared in font
;;; space, similar to what the cairo contexts passed to the render_glyph method
;;; will look like. The callback can use this context for extents computation
;;; for example. After the callback is called, cr is checked for any error
;;; status.

;;; The extents argument is where the user font sets the font extents for
;;; scaled_font . It is in font space, which means that for most cases its
;;; ascent and descent members should add to 1.0. extents is preset to hold a
;;; value of 1.0 for ascent, height, and max_x_advance, and 0.0 for descent and
;;; max_y_advance members.
;;;
;;; The callback is optional. If not set, default font extents as described in
;;; the previous paragraph will be used.
;;;
;;; Note that scaled_font is not fully initialized at this point and trying to
;;; use it for text operations in the callback will result in deadlock.
;;;
;;; scaled_font :
;;;     the scaled-font being created
;;;
;;; cr :
;;;     a cairo context, in font space
;;;
;;; extents :
;;;     font extents to fill in, in font space
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS upon success, or an error status on error.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_render_glyph_func_t ()
;;;
;;; cairo_status_t
;;; (*cairo_user_scaled_font_render_glyph_func_t)
;;;                                (cairo_scaled_font_t *scaled_font,
;;;                                 unsigned long  glyph,
;;;                                 cairo_t *cr,
;;;                                 cairo_text_extents_t *extents);
;;;
;;; cairo_user_scaled_font_render_glyph_func_t is the type of function which is
;;; called when a user scaled-font needs to render a glyph.
;;;
;;; The callback is mandatory, and expected to draw the glyph with code glyph
;;; to the cairo context cr . cr is prepared such that the glyph drawing is
;;; done in font space. That is, the matrix set on cr is the scale matrix of
;;; scaled_font , The extents argument is where the user font sets the font
;;; extents for scaled_font . However, if user prefers to draw in user space,
;;; they can achieve that by changing the matrix on cr . All cairo rendering
;;; operations to cr are permitted, however, the result is undefined if any
;;; source other than the default source on cr is used. That means, glyph
;;; bitmaps should be rendered using cairo_mask() instead of cairo_paint().
;;;
;;; Other non-default settings on cr include a font size of 1.0 (given that it
;;; is set up to be in font space), and font options corresponding to
;;; scaled_font .
;;;
;;; The extents argument is preset to have x_bearing, width, and y_advance of
;;; zero, y_bearing set to -font_extents.ascent, height to
;;; font_extents.ascent+font_extents.descent, and x_advance to
;;; font_extents.max_x_advance. The only field user needs to set in majority of
;;; cases is x_advance. If the width field is zero upon the callback returning
;;; (which is its preset value), the glyph extents are automatically computed
;;; based on the drawings done to cr . This is in most cases exactly what the
;;; desired behavior is. However, if for any reason the callback sets the
;;; extents, it must be ink extents, and include the extents of all drawing
;;; done to cr in the callback.
;;;
;;; scaled_font :
;;;     user scaled-font
;;;
;;; glyph :
;;;     glyph code to render
;;;
;;; cr :
;;;     cairo context to draw to, in font space
;;;
;;; extents :
;;;     glyph extents to fill in, in font space
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS upon success, or CAIRO_STATUS_USER_FONT_ERROR or
;;;     any other error status on error.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_text_to_glyphs_func_t ()
;;;
;;; cairo_status_t
;;;( *cairo_user_scaled_font_text_to_glyphs_func_t)
;;;                                (cairo_scaled_font_t *scaled_font,
;;;                                 const char *utf8,
;;;                                 int utf8_len,
;;;                                 cairo_glyph_t **glyphs,
;;;                                 int *num_glyphs,
;;;                                 cairo_text_cluster_t **clusters,
;;;                                 int *num_clusters,
;;;                                 cairo_text_cluster_flags_t *cluster_flags);
;;;
;;; cairo_user_scaled_font_text_to_glyphs_func_t is the type of function which
;;; is called to convert input text to an array of glyphs. This is used by the
;;; cairo_show_text() operation.
;;;
;;; Using this callback the user-font has full control on glyphs and their
;;; positions. That means, it allows for features like ligatures and kerning,
;;; as well as complex shaping required for scripts like Arabic and Indic.
;;;
;;; The num_glyphs argument is preset to the number of glyph entries available
;;; in the glyphs buffer. If the glyphs buffer is NULL, the value of num_glyphs
;;; will be zero. If the provided glyph array is too short for the conversion
;;; (or for convenience), a new glyph array may be allocated using
;;; cairo_glyph_allocate() and placed in glyphs . Upon return, num_glyphs should
;;; contain the number of generated glyphs. If the value glyphs points at has
;;; changed after the call, the caller will free the allocated glyph array using
;;; cairo_glyph_free(). The caller will also free the original value of glyphs ,
;;; so the callback shouldn't do so. The callback should populate the glyph
;;; indices and positions (in font space) assuming that the text is to be shown
;;; at the origin.
;;;
;;; If clusters is not NULL, num_clusters and cluster_flags are also non-NULL,
;;; and cluster mapping should be computed. The semantics of how cluster array
;;; allocation works is similar to the glyph array. That is, if clusters
;;; initially points to a non-NULL value, that array may be used as a cluster
;;; buffer, and num_clusters points to the number of cluster entries available
;;; there. If the provided cluster array is too short for the conversion (or for
;;; convenience), a new cluster array may be allocated using
;;; cairo_text_cluster_allocate() and placed in clusters . In this case, the
;;; original value of clusters will still be freed by the caller. Upon return,
;;; num_clusters should contain the number of generated clusters. If the value
;;; clusters points at has changed after the call, the caller will free the
;;; allocated cluster array using cairo_text_cluster_free().
;;;
;;; The callback is optional. If num_glyphs is negative upon the callback
;;; returning or if the return value is CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED,
;;; the unicode_to_glyph callback is tried. See
;;; cairo_user_scaled_font_unicode_to_glyph_func_t.
;;;
;;; Note: While cairo does not impose any limitation on glyph indices, some
;;; applications may assume that a glyph index fits in a 16-bit unsigned
;;; integer. As such, it is advised that user-fonts keep their glyphs in the 0
;;; to 65535 range. Furthermore, some applications may assume that glyph 0 is a
;;; special glyph-not-found glyph. User-fonts are advised to use glyph 0 for
;;; such purposes and do not use that glyph value for other purposes.
;;;
;;; scaled_font :
;;;     the scaled-font being created
;;;
;;; utf8 :
;;;     a string of text encoded in UTF-8
;;;
;;; utf8_len :
;;;     length of utf8 in bytes
;;;
;;; glyphs :
;;;     pointer to array of glyphs to fill, in font space
;;;
;;; num_glyphs :
;;;     pointer to number of glyphs
;;;
;;; clusters :
;;;     pointer to array of cluster mapping information to fill, or NULL
;;;
;;; num_clusters :
;;;     pointer to number of clusters
;;;
;;; cluster_flags :
;;;     pointer to location to store cluster flags corresponding to the output
;;;     clusters
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS upon success,
;;;     CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED if fallback options should be
;;;     tried, or CAIRO_STATUS_USER_FONT_ERROR or any other error status on
;;;     error.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_unicode_to_glyph_func_t ()
;;;
;;; cairo_status_t
;;; (*cairo_user_scaled_font_unicode_to_glyph_func_t)
;;;                                (cairo_scaled_font_t *scaled_font,
;;;                                 unsigned long  unicode,
;;;                                 unsigned long *glyph_index);
;;;
;;; cairo_user_scaled_font_unicode_to_glyph_func_t is the type of function which
;;; is called to convert an input Unicode character to a single glyph. This is
;;; used by the cairo_show_text() operation.
;;;
;;; This callback is used to provide the same functionality as the
;;; text_to_glyphs callback does (see
;;; cairo_user_scaled_font_text_to_glyphs_func_t) but has much less control on
;;; the output, in exchange for increased ease of use. The inherent assumption
;;; to using this callback is that each character maps to one glyph, and that
;;; the mapping is context independent. It also assumes that glyphs are
;;; positioned according to their advance width. These mean no ligatures,
;;; kerning, or complex scripts can be implemented using this callback.
;;;
;;; The callback is optional, and only used if text_to_glyphs callback is not
;;; set or fails to return glyphs. If this callback is not set or if it returns
;;; CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED, an identity mapping from Unicode
;;; code-points to glyph indices is assumed.
;;;
;;; Note: While cairo does not impose any limitation on glyph indices, some
;;; applications may assume that a glyph index fits in a 16-bit unsigned
;;; integer. As such, it is advised that user-fonts keep their glyphs in the 0
;;; to 65535 range. Furthermore, some applications may assume that glyph 0 is a
;;; special glyph-not-found glyph. User-fonts are advised to use glyph 0 for
;;; such purposes and do not use that glyph value for other purposes.
;;;
;;; scaled_font :
;;;     the scaled-font being created
;;;
;;; unicode :
;;;     input unicode character code-point
;;;
;;; glyph_index :
;;;     output glyph index
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS upon success,
;;;     CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED if fallback options should be
;;;     tried, or CAIRO_STATUS_USER_FONT_ERROR or any other error status on
;;;     error.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_create ()
;;;
;;; cairo_font_face_t * cairo_user_font_face_create (void);
;;;
;;; Creates a new user font-face.
;;;
;;; Use the setter functions to associate callbacks with the returned user font.
;;; The only mandatory callback is render_glyph.
;;;
;;; After the font-face is created, the user can attach arbitrary data (the
;;; actual font data) to it using cairo_font_face_set_user_data() and access it
;;; from the user-font callbacks by using cairo_scaled_font_get_font_face()
;;; followed by cairo_font_face_get_user_data().
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_set_init_func ()
;;;
;;; void
;;; cairo_user_font_face_set_init_func (
;;;                                cairo_font_face_t *font_face,
;;;                                cairo_user_scaled_font_init_func_t init_func)
;;;
;;; Sets the scaled-font initialization function of a user-font. See
;;; cairo_user_scaled_font_init_func_t for details of how the callback works.
;;;
;;; The font-face should not be immutable or a CAIRO_STATUS_USER_FONT_IMMUTABLE
;;; error will occur. A user font-face is immutable as soon as a scaled-font is
;;; created from it.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; init_func :
;;;     The init callback, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_get_init_func ()
;;;
;;; cairo_user_scaled_font_init_func_t
;;; cairo_user_font_face_get_init_func (cairo_font_face_t *font_face);
;;;
;;; Gets the scaled-font initialization function of a user-font.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; Returns :
;;;     The init callback of font_face or NULL if none set or an error has
;;;     occurred.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_set_render_glyph_func ()
;;;
;;; void
;;; cairo_user_font_face_set_render_glyph_func
;;;               (cairo_font_face_t *font_face,
;;;                cairo_user_scaled_font_render_glyph_func_t render_glyph_func)
;;;
;;; Sets the glyph rendering function of a user-font. See
;;; cairo_user_scaled_font_render_glyph_func_t for details of how the callback
;;; works.
;;;
;;; The font-face should not be immutable or a CAIRO_STATUS_USER_FONT_IMMUTABLE
;;; error will occur. A user font-face is immutable as soon as a scaled-font is
;;; created from it.
;;;
;;; The render_glyph callback is the only mandatory callback of a user-font. If
;;; the callback is NULL and a glyph is tried to be rendered using font_face ,
;;; a CAIRO_STATUS_USER_FONT_ERROR will occur.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; render_glyph_func :
;;;     The render_glyph callback, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_get_render_glyph_func ()
;;;
;;; cairo_user_scaled_font_render_glyph_func_t
;;; cairo_user_font_face_get_render_glyph_func
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the glyph rendering function of a user-font.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; Returns :
;;;     The render_glyph callback of font_face or NULL if none set or an error
;;;     has occurred.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_set_render_color_glyph_func ()
;;;
;;; void
;;; cairo_user_font_face_set_render_color_glyph_func
;;;              (cairo_font_face_t *font_face,
;;;               cairo_user_scaled_font_render_glyph_func_t render_glyph_func);
;;;
;;; Sets the color glyph rendering function of a user-font. See
;;; cairo_user_scaled_font_render_glyph_func_t for details of how the callback
;;; works.
;;;
;;; The font-face should not be immutable or a CAIRO_STATUS_USER_FONT_IMMUTABLE
;;; error will occur. A user font-face is immutable as soon as a scaled-font is
;;; created from it.
;;;
;;; The render_glyph callback is the only mandatory callback of a user-font. At
;;; least one of cairo_user_font_face_set_render_color_glyph_func() or
;;; cairo_user_font_face_set_render_glyph_func() must be called to set a render
;;; callback. If both callbacks are set, the color glyph render callback is
;;; invoked first. If the color glyph render callback returns
;;; CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED, the non-color version of the
;;; callback is invoked.
;;;
;;; If the callback is NULL and a glyph is tried to be rendered using font_face,
;;; a CAIRO_STATUS_USER_FONT_ERROR will occur.
;;;
;;; font_face
;;;     A user font face
;;;
;;; render_glyph_func
;;;     The render_glyph callback, or NULL
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_get_render_color_glyph_func ()
;;;
;;; cairo_user_scaled_font_render_glyph_func_t
;;; cairo_user_font_face_get_render_color_glyph_func
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the color glyph rendering function of a user-font.
;;;
;;; font_face
;;;     A user font face
;;;
;;; Returns
;;;     The render_glyph callback of font_face or NULL if none set or an error
;;;     has occurred.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_set_unicode_to_glyph_func ()
;;;
;;; void
;;; cairo_user_font_face_set_unicode_to_glyph_func
;;;       (cairo_font_face_t *font_face,
;;;        cairo_user_scaled_font_unicode_to_glyph_func_t unicode_to_glyph_func)
;;;
;;; Sets the unicode-to-glyph conversion function of a user-font. See
;;; cairo_user_scaled_font_unicode_to_glyph_func_t for details of how the
;;; callback works.
;;;
;;; The font-face should not be immutable or a CAIRO_STATUS_USER_FONT_IMMUTABLE
;;; error will occur. A user font-face is immutable as soon as a scaled-font is
;;; created from it.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; unicode_to_glyph_func :
;;;     The unicode_to_glyph callback, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_get_unicode_to_glyph_func ()
;;;
;;; cairo_user_scaled_font_unicode_to_glyph_func_t
;;; cairo_user_font_face_get_unicode_to_glyph_func
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the unicode-to-glyph conversion function of a user-font.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; Returns :
;;;     The unicode_to_glyph callback of font_face or NULL if none set or an
;;;     error occurred.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_set_text_to_glyphs_func ()
;;;
;;; void
;;; cairo_user_font_face_set_text_to_glyphs_func
;;;           (cairo_font_face_t *font_face,
;;;            cairo_user_scaled_font_text_to_glyphs_func_t text_to_glyphs_func)
;;;
;;; Sets th text-to-glyphs conversion function of a user-font. See
;;; cairo_user_scaled_font_text_to_glyphs_func_t for details of how the callback
;;; works.
;;;
;;; The font-face should not be immutable or a CAIRO_STATUS_USER_FONT_IMMUTABLE
;;; error will occur. A user font-face is immutable as soon as a scaled-font is
;;; created from it.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; text_to_glyphs_func :
;;;     The text_to_glyphs callback, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_font_face_get_text_to_glyphs_func ()
;;;
;;; cairo_user_scaled_font_text_to_glyphs_func_t
;;; cairo_user_font_face_get_text_to_glyphs_func
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the text-to-glyphs conversion function of a user-font.
;;;
;;; font_face :
;;;     A user font face
;;;
;;; Returns :
;;;     The text_to_glyphs callback of font_face or NULL if none set or an error
;;;     occurred.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_get_foreground_marker ()
;;;
;;; cairo_pattern_t *
;;; cairo_user_scaled_font_get_foreground_marker
;;;                                (cairo_scaled_font_t *scaled_font);
;;;
;;; Gets the foreground pattern of the glyph currently being rendered. A
;;; cairo_user_scaled_font_render_glyph_func_t function that has been set with
;;; cairo_user_font_face_set_render_color_glyph_func() may call this function to
;;; retrieve the current foreground pattern for the glyph being rendered. The
;;; function should not be called outside of a
;;; cairo_user_font_face_set_render_color_glyph_func() callback.
;;;
;;; The foreground marker pattern contains an internal marker to indicate that
;;; it is to be substituted with the current source when rendered to a surface.
;;; Querying the foreground marker will reveal a solid black color, however this
;;; is not representative of the color that will actually be used. Similarly,
;;; setting a solid black color will render black, not the foreground pattern
;;; when the glyph is painted to a surface. Using the foreground marker as the
;;; source instead of cairo_user_scaled_font_get_foreground_source() in a color
;;; render callback has the following benefits:
;;;
;;; Cairo only needs to call the render callback once as it can cache the
;;; recording. Cairo will substitute the actual foreground color when rendering
;;; the recording.
;;;
;;; On backends that have the concept of a foreground color in fonts such as
;;; PDF, PostScript, and SVG, cairo can generate more optimal output. The glyph
;;; can be included in an embedded font.
;;;
;;; The one drawback of the using foreground marker is the render callback can
;;; not access the color components of the pattern as the actual foreground
;;; pattern is not available at the time the render callback is invoked. If the
;;; render callback needs to query the foreground pattern, use
;;; cairo_user_scaled_font_get_foreground_source().
;;;
;;; If the render callback simply wants to call cairo_set_source() with the
;;; foreground pattern, cairo_user_scaled_font_get_foreground_marker() is the
;;; preferred function to use as it results in better performance than
;;; cairo_user_scaled_font_get_foreground_source().
;;;
;;; scaled_font
;;;     A user scaled font
;;;
;;; Returns
;;;     the current foreground source marker pattern. This object is owned by
;;;     cairo. This object must not be modified or used outside of a color
;;;     render callback. To keep a reference to it, you must call
;;;     cairo_pattern_reference().
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_user_scaled_font_get_foreground_source ()
;;;
;;; cairo_pattern_t *
;;; cairo_user_scaled_font_get_foreground_source
;;;                                (cairo_scaled_font_t *scaled_font);
;;;
;;; Gets the foreground pattern of the glyph currently being rendered. A
;;; cairo_user_scaled_font_render_glyph_func_t function that has been set with
;;; cairo_user_font_face_set_render_color_glyph_func() may call this function to
;;; retrieve the current foreground pattern for the glyph being rendered. The
;;; function should not be called outside of a
;;; cairo_user_font_face_set_render_color_glyph_func() callback.
;;;
;;; This function returns the current source at the time the glyph is rendered.
;;; Compared with cairo_user_scaled_font_get_foreground_marker(), this function
;;; returns the actual source pattern that will be used to render the glyph. The
;;; render callback is free to query the pattern and extract color components or
;;; other pattern data. For example if the render callback wants to create a
;;; gradient stop based on colors in the foreground source pattern, it will need
;;; to use this function in order to be able to query the colors in the
;;; foreground pattern.
;;;
;;; While this function does not have the restrictions on using the pattern that
;;; cairo_user_scaled_font_get_foreground_marker() has, it does incur a
;;; performance penalty. If a render callback calls this function:
;;;
;;; Cairo will call the render callback whenever the current pattern of the
;;; context in which the glyph is rendered changes.
;;;
;;; On backends that support font embedding (PDF, PostScript, and SVG), cairo
;;; can not embed this glyph in a font. Instead the glyph will be emitted as an
;;; image or sequence of drawing operations each time it is used.
;;;
;;; scaled_font
;;;     A user scaled font
;;;
;;; Returns
;;;     the current foreground source pattern. This object is owned by cairo.
;;;     To keep a reference to it, you must call cairo_pattern_reference().
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.user-font.lisp ---------------------------------------
