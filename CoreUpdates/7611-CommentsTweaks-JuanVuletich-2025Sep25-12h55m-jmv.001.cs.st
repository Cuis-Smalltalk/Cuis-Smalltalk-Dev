'From Cuis7.5 [latest update: #7610] on 25 September 2025 at 12:57:53 pm'!

!AbstractFont commentStamp: 'jmv 9/25/2025 10:38:18' prior: 0!
AbstractFont defines the generic interface that all fonts need to implement.
See
https://en.wikipedia.org/wiki/Font.
https://en.wikipedia.org/wiki/Computer_font
See TrueTypeFont.!


!TrueTypeFont commentStamp: 'jmv 9/25/2025 10:37:25' prior: 0!
TrueType fonts are the most widely used format for pixel independent vector Fonts.
See https://en.wikipedia.org/wiki/Font.

In Cuis what is usually called a TrueType Font is represented by TTFontDescription.

A TrueTypeFont refers to a TTFontDescription, but adds a specific pointSize, and is the object actually  used in text editors. They may have an emphasis added, like bold, italic, underlined, strikeTrhough, superscript and subscript. If they  have an emphasis, they also know their base font.

They also hold cached FormGlyph instances, that are used to speed up text display in BitBltCanvas and HybridCanvas.!


!FontFamily commentStamp: '<historical>' prior: 0!
Also called Typeface. See https://en.wikipedia.org/wiki/Typeface
See TrueTypeFontFamily!


!TrueTypeFontFamily commentStamp: '<historical>' prior: 0!
Also called Typeface. See https://en.wikipedia.org/wiki/Typeface
Also see AbstractFont / TrueTypeFont
Also see TTFontDescription

My instances have a base TTFontDescription (such as DejaVu Sans Book) and (if available) a dictionary of TTFontDescription by emphasis (such as DejaVu Sans Oblique and DejaVu Sans Bold).

They also have a dictionary of base TrueTypeFont instances, indexed by pointSize, which in turn know their derivative for various emphases.!


!TTFontDescription commentStamp: '<historical>' prior: 0!
Holds a TrueType font from a .ttf file in memory.
My instances are members of a FontFamily (a Typeface), and are of a specific style.
Examples: DejaVu Sans Oblique, DejaVu Sans Book. They are menbers of the DejaVu Sans typeface (FontFamily).

See
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://en.wikipedia.org/wiki/Font.!


!TTGlyph commentStamp: '<historical>' prior: 0!
This class represents a glyph of a TrueType font.

Instance variables:
	boundsLeft 					xMin in TrueType parlance
	boundsRight 			 	xMax in TrueType parlance
	boundsBottom 				yMin in TrueType parlance (usually close to baseline, i.e. 0)
	boundsTop 					yMax in TrueType parlance (usually positive and large, in TrueType, y axis points up)
	contours						<Array of: TTContourConstruction>
	advanceWidth				<Integer>	advance width of the glyph
	leftSideBearing  				<Integer>	left side bearing
	rightSideBearing  			<Integer>	right side bearing!

