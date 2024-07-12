'From Cuis7.1 [latest update: #6515] on 12 July 2024 at 11:51:50 am'!

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 7/12/2024 11:51:28'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseGlyphTopOffset deltaYForDiacriticalMark |
	deltaXForBaseGlyph := 0.
	baseGlyphTopOffset := 0.
	ascent := aTrueTypeFont ascent.		  "expensive"
	lineGap := aTrueTypeFont lineGap."expensive"
	destY := aPoint y.
	fdestX := aPoint x.
	maxHeight := 0.
	byteIndex := byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph := aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ] 
			into: nil.
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

