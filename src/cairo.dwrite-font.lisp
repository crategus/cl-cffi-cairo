;;; ----------------------------------------------------------------------------
;;; cairo.dwrite-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;; DWrite Fonts
;;;
;;;     Font support for Microsoft DirectWrite
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_DWRITE_FONT
;;;
;;; Functions
;;;
;;;     cairo_dwrite_font_face_create_for_dwrite_fontface
;;;     cairo_dwrite_font_face_get_rendering_params
;;;     cairo_dwrite_font_face_set_rendering_params
;;;     cairo_dwrite_font_face_get_measuring_mode
;;;     cairo_dwrite_font_face_set_measuring_mode
;;;
;;; Description
;;;
;;;     The Microsoft DirectWrite font backend is primarily used to render text
;;;     on Microsoft Windows systems.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_DWRITE_FONT
;;;
;;; #define CAIRO_HAS_DWRITE_FONT
;;;
;;; Defined if the Microsoft DWrite font backend is available. This macro can
;;; be used to conditionally compile backend-specific code.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_dwrite_font_face_create_for_dwrite_fontface ()
;;;
;;; cairo_font_face_t *
;;; cairo_dwrite_font_face_create_for_dwrite_fontface
;;;                                (IDWriteFontFace *dwrite_font_face);
;;;
;;; Creates a new font for the DWrite font backend based on a DWrite font face.
;;; This font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create().
;;;
;;; Here is an example of how this function might be used:
;;;
;;; #include <cairo-dwrite.h>
;;; #include <dwrite.h>
;;;
;;; IDWriteFactory* dWriteFactory = NULL;
;;; HRESULT hr = DWriteCreateFactory(
;;;     DWRITE_FACTORY_TYPE_SHARED,
;;;     __uuidof(IDWriteFactory),
;;;    reinterpret_cast<IUnknown**>(&dWriteFactory));
;;;
;;; IDWriteFontCollection *systemCollection;
;;; hr = dWriteFactory->GetSystemFontCollection(&systemCollection);
;;;
;;; UINT32 idx;
;;; BOOL found;
;;; systemCollection->FindFamilyName(L"Segoe UI Emoji", &idx, &found);
;;;
;;; IDWriteFontFamily *family;
;;; systemCollection->GetFontFamily(idx, &family);
;;;
;;; IDWriteFont *dwritefont;
;;; DWRITE_FONT_WEIGHT weight = DWRITE_FONT_WEIGHT_NORMAL;
;;; DWRITE_FONT_STYLE style = DWRITE_FONT_STYLE_NORMAL;
;;; hr = family->GetFirstMatchingFont(weight, DWRITE_FONT_STRETCH_NORMAL,
                                              style,
                                              &dwritefont);
;;;
;;; IDWriteFontFace *dwriteface;
;;; hr = dwritefont->CreateFontFace(&dwriteface);
;;;
;;; cairo_font_face_t *face;
;;; face = cairo_dwrite_font_face_create_for_dwrite_fontface(dwriteface);
;;; cairo_set_font_face(cr, face);
;;; cairo_set_font_size(cr, 70);
;;; cairo_move_to(cr, 100, 100);
;;; cairo_show_text(cr, "😃");
;;;
;;; Note: When printing a DWrite font to a CAIRO_SURFACE_TYPE_WIN32_PRINTING
;;; surface, the printing surface will substitute each DWrite font with a Win32
;;; font created from the same underlying font file. If the matching font file
;;; can not be found, the CAIRO_SURFACE_TYPE_WIN32_PRINTING surface will convert
;;; each glyph to a filled path. If a DWrite font was not created from a system
;;; font, it is recommended that the font used to create the DWrite font be made
;;; available to GDI to avoid the undesirable fallback to emitting paths. This
;;; can be achieved using the GDI font loading functions such as
;;; AddFontMemResourceEx().
;;;
;;; dwrite_font_face
;;;     A pointer to an IDWriteFontFace specifying the DWrite font to use.
;;;
;;; Returns
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_dwrite_font_face_get_rendering_params ()
;;;
;;; IDWriteRenderingParams *
;;; cairo_dwrite_font_face_get_rendering_params
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the IDWriteRenderingParams object of font_face .
;;;
;;; font_face
;;;     The cairo_dwrite_font_face_t object to query
;;;
;;; Returns
;;;     the IDWriteRenderingParams object or NULL if none.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_dwrite_font_face_set_rendering_params ()
;;;
;;; void
;;; cairo_dwrite_font_face_set_rendering_params
;;;                                (cairo_font_face_t *font_face,
;;;                                 IDWriteRenderingParams *params);
;;;
;;; Sets the IDWriteRenderingParams object to font_face . This
;;; IDWriteRenderingParams is used to render glyphs if default values of font
;;; options are used. If non-defalut values of font options are specified when
;;; creating a cairo_scaled_font_t, cairo creates a new IDWriteRenderingParams
;;; object for the cairo_scaled_font_t object by overwriting the corresponding
;;; parameters.
;;;
;;; font_face
;;;     The cairo_dwrite_font_face_t object to modify
;;;
;;; params
;;;     The IDWriteRenderingParams object
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_dwrite_font_face_get_measuring_mode ()
;;;
;;; DWRITE_MEASURING_MODE
;;; cairo_dwrite_font_face_get_measuring_mode
;;;                                (cairo_font_face_t *font_face);
;;;
;;; Gets the DWRITE_MEASURING_MODE enum of font_face .
;;;
;;; font_face
;;;     The cairo_dwrite_font_face_t object to query
;;;
;;; Returns
;;;     The DWRITE_MEASURING_MODE enum of font_face .
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_dwrite_font_face_set_measuring_mode ()
;;;
;;; void
;;; cairo_dwrite_font_face_set_measuring_mode
;;;                                (cairo_font_face_t *font_face,
;;;                                 DWRITE_MEASURING_MODE mode);
;;;
;;; Sets the DWRITE_MEASURING_MODE enum to font_face .
;;;
;;; font_face
;;;     The cairo_dwrite_font_face_t object to modify
;;;
;;; mode
;;;     The DWRITE_MEASURING_MODE enum.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.dwrite-font.lisp -------------------------------------
