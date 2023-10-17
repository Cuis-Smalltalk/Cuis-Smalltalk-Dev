'From Cuis 6.0 [latest update: #6045] on 17 October 2023 at 1:47:47 pm'!

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 10/17/2023 13:45:57'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap deltaXForBaseGlyph deltaXForDiacriticalMark baseGlyphTopOffset deltaYForDiacriticalMark |
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
		"Center diacriticals marks."
		glyph advanceWidth = 0.0
			ifFalse: [
				deltaXForBaseGlyph := glyph deltaXToCenter.
				baseGlyphTopOffset := glyph topOffset.
				deltaXForDiacriticalMark := 0.0.
				deltaYForDiacriticalMark := 0.0 ]
			ifTrue: [
				deltaXForDiacriticalMark := glyph deltaXToCenter.
				deltaXForDiacriticalMark = 0 ifFalse: [ "Only for those who specify a delta to center."
					deltaXForDiacriticalMark := deltaXForDiacriticalMark - deltaXForBaseGlyph ].
				deltaYForDiacriticalMark := baseGlyphTopOffset = 0
					ifTrue: [0]
					ifFalse: [ baseGlyphTopOffset - (glyph topOffset * 0.8) ]].
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
	^ fdestX ceiling@(destY+maxHeight)! !

