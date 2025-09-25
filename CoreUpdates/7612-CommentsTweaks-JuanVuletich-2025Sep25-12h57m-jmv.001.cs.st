'From Cuis7.5 [latest update: #7610] on 25 September 2025 at 1:06:23 pm'!

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 9/25/2025 13:03:40'!
basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph firstByte maxHeight byteIndex n ascent lineGap deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseGlyphTopOffset deltaYForDiacriticalMark |
	deltaXForBaseGlyph := 0.
	baseGlyphTopOffset := 0.
	ascent := aTrueTypeFont ascent.
	lineGap := aTrueTypeFont lineGap.
	destY := aPoint y.
	fdestX := aPoint x.
	maxHeight := 0.
	byteIndex := byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph := aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4.
				firstByte := byte1 ] 
			into: nil.

		"$i followed by a Combining Diacritical Mark is replaced by CodePoint 305 Dotless i, UTF-8 #[196 177]
		(UnicodeString fromUtf8Bytes: #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137	]) edit
		This should also be done for VectorEngineWithPlugin at
			displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:
		"
		(firstByte = 105 and: [ byteIndex < (byteStopIndex-1) ] and: [
			(aByteArray at: byteIndex+1) = 204 or: [ (aByteArray at: byteIndex+1) = 205 and: [ (aByteArray at: byteIndex+2) < 176 ]]])
				ifTrue: [ glyph := aTrueTypeFont glyphAtUtf8Byte1: 196 byte2: 177 byte3: nil byte4: nil ].

		"Center diacriticals marks.
		See building of FormGlyph at
		formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:
		Implement equivalent behavior at 
			displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:
		(Done in Smalltalk VectorEngine, still to be done for VectorEngineWithPlugin)."
		"Tested at various sizes of DejaVu Sans, DejaVu Serif, JetBrains Mono NL, Kiwi Maru Light (except ~ and `), Kurinto Sans, Neo Euler, Source Sans 3
		Test text (at sizes 10, 13, 144):
		(UnicodeString fromUtf8Bytes: #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97	]) edit
		"
		deltaXToCenter := glyph deltaXToCenter.
		deltaXForDiacriticalMark := 0.
		deltaYForDiacriticalMark := 0.
		glyph advanceWidth = 0.0
			ifFalse: [ "Regular glyph"
				deltaXForBaseGlyph := deltaXToCenter.
				baseGlyphTopOffset := glyph topOffset ]
			ifTrue: [ "Diacritical mark"
				deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph.
				"Alternative implementation, perhaps closer to what  is done at VectorEngine >> #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:"
				"
				xtraSpaceForAntiAliasingAndRounding := 3. 'see #formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:'.
				baseBoundsTop := baseGlyphTopOffset-xtraSpaceForAntiAliasingAndRounding.
				boundsBottom := glyph topOffset-xtraSpaceForAntiAliasingAndRounding - (glyph form height-xtraSpaceForAntiAliasingAndRounding).
				boundsBottom := glyph topOffset - glyph form height.
				baseBoundsTop * 0.9 > boundsBottom ifTrue: [
					deltaYForDiacriticalMark :=  baseBoundsTop - boundsBottom ].
				"
				"Only if placed over a glyph with a top offset"
				baseGlyphTopOffset = 0 					ifFalse: [
					deltaYForDiacriticalMark :=  baseGlyphTopOffset - (glyph topOffset * 0.76).
					deltaYForDiacriticalMark := deltaYForDiacriticalMark max: 0 ]].
		destX := (fdestX + glyph leftOffset + deltaXForDiacriticalMark) roundedHAFZ.
		destY := (aPoint y + ascent + lineGap-1 - glyph topOffset - deltaYForDiacriticalMark) roundedHAFZ.
		"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
		aTrueTypeFont isSuperscript
			ifTrue: [destY := destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY := destY + (ascent*0.3)]].
		destY := destY roundedHAFZ.
		sourceForm := glyph form.
		height := sourceForm height.
		width := sourceForm width.
		self copyBits.
		fdestX := fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight := maxHeight max: height.
		byteIndex := byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight).! !


!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 9/25/2025 10:46:25'!
ascent
	"The distance from the baseline to the highest or upper grid coordinate used to place an outline point.
	It is a positive value, due to the grid's orientation with the Y axis upwards.
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription ascent * pointSize / ttFontDescription letterMTopSideBearing! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 9/25/2025 13:05:30'!
descent
	"The distance from the baseline to the lowest grid coordinate used to place an outline point.
	It is a positive value (although in the TTFontDescription it is negative).
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription descent negated * pointSize / ttFontDescription letterMTopSideBearing! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 9/25/2025 13:05:12'!
lineGap
	"The distance that must be placed between two lines of text. The baseline-to-baseline distance 
	should be computed as
		linespace = ascent + descent + linegap
	if you use the typographic values.
	See https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^ 0.0 + ttFontDescription lineGap * pointSize / ttFontDescription letterMTopSideBearing! !

