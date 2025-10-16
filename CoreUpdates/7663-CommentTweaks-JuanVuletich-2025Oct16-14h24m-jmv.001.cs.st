'From Cuis7.5 [latest update: #7662] on 16 October 2025 at 2:30:01 pm'!

!TrueTypeFont commentStamp: '<historical>' prior: 0!
TrueType fonts are the most widely used format for pixel independent vector Fonts.
See https://en.wikipedia.org/wiki/Font.

In Cuis what is usually called a TrueType Font is represented by TTFontDescription.

A TrueTypeFont refers to a TTFontDescription, but adds a specific pointSize, and is the object actually used in text editors. They may have an emphasis added, like bold, italic, underlined, strikeTrhough, superscript and subscript. If they  have an emphasis, then they also know their base font (without any emphasis at all).

They also hold cached FormGlyph instances, that are used to speed up text display in BitBltCanvas and HybridCanvas.

See FontFamily.
See TTFontDescription.!


!FontFamily commentStamp: '<historical>' prior: 0!
A FontFamily is a collection of TrueType fonts that belong together and share a font name.
For example, Cuis ships with the DejaVu Sans font family.

Also called Typeface. See https://en.wikipedia.org/wiki/Typeface

See AbstractFont / TrueTypeFont.
See TTFontDescription.!


!TTFontDescription commentStamp: '<historical>' prior: 0!
Holds a TrueType font from a .ttf file in memory.
My instances are members of a FontFamily (a Typeface), and are of a specific style.
Examples: DejaVu Sans Oblique, DejaVu Sans Book. They are menbers of the DejaVu Sans typeface (FontFamily).

See AbstractFont / TrueTypeFont.
See FontFamily.

See
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://en.wikipedia.org/wiki/Font.!

