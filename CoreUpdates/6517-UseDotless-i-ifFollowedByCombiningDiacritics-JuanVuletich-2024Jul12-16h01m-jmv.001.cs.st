'From Cuis7.1 [latest update: #6516] on 12 July 2024 at 4:05:33 pm'!

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 7/12/2024 15:51:06'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph firstByte maxHeight byteIndex n ascent lineGap deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseGlyphTopOffset deltaYForDiacriticalMark |
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


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 7/12/2024 15:51:39'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours numBeziers baseIndex x y cx cy contourStart deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseBoundsTop deltaYForDiacriticalMark byte1 byte2OrNil |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	byte1 := nil.
	byte2OrNil := nil.
	self fillColor: aColor do: [
		nextGlyphX := aPoint x.
		nextGlyphY := aPoint y.
		baseIndex := 0.
		byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
			byte := aByteArray at: byteIndex.
			byte1 isNil ifTrue: [byte1 := byte ] ifFalse: [ byte2OrNil isNil ifTrue: [ byte2OrNil := byte ]].
			i := contourDataIndexes at: baseIndex + byte + 1.

			"$i followed by a Combining Diacritical Mark is replaced by CodePoint 305 Dotless i, UTF-8 #[196 177]
			(UnicodeString fromUtf8Bytes: #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137	]) edit
			This should also be done for VectorEngineWithPlugin
			"
			(byte1 = 105 and: [ byteIndex < (byteStopIndex-1) ] and: [
				(aByteArray at: byteIndex+1) = 204 or: [ (aByteArray at: byteIndex+1) = 205 and: [ (aByteArray at: byteIndex+2) < 176 ]]])
					ifTrue: [
						i := contourDataIndexes at: 196 + 1.
						i := contourDataIndexes at: i negated + 177 + 1 						].

			i < 0
				ifTrue: [ baseIndex := i negated ]
				ifFalse: [
					advanceWidth := contourData at: i. i := i + 1.
					boundsLeft := contourData at: i. i := i + 1.
					boundsRight := contourData at: i. i := i + 1.
					boundsBottom := contourData at: i. i := i + 1.
					boundsTop := contourData at: i. i := i + 1.
					numContours := (contourData at: i) asInteger. i := i + 1.
					"TrueType doesn't speciy how to position diacritics wrt base character glyph.
					Horizontally center diacriticals wrt base glyph.
					See
						basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:
						formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:
					This should also be done for VectorEngineWithPlugin
					Apparently OpenType can do better than TrueType, as it includes the GPOS table."
					deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
					byte1 = 204 ifTrue: [
						byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
						byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) 			]].

					"DoubleWidth code points that combine above previous and following characters.
					Codepoints between 16r035C and 16r0363.
					Example: o͢o
					Don't center them"
					(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
						deltaXToCenter := 0.0 ].

					deltaXForDiacriticalMark := 0.0.
					deltaYForDiacriticalMark := 0.
					advanceWidth = 0.0
						ifFalse: [ "Regular glyph"
							deltaXForBaseGlyph := deltaXToCenter.
							baseBoundsTop := boundsTop ]
						ifTrue: [ "Diacritical mark"
							deltaXForBaseGlyph = 0.0  						"For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
								ifFalse: [
									deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph * sx.
									baseBoundsTop > boundsBottom ifTrue: [
										deltaYForDiacriticalMark :=  baseBoundsTop - boundsBottom + 100 * sy ]]].
					nextGlyphX := nextGlyphX + deltaXForDiacriticalMark.
					numContours timesRepeat: [
						numBeziers := (contourData at: i) asInteger. i := i + 1.
						ttMoveToX := contourData at: i. i := i + 1.
						ttMoveToY := contourData at: i. i := i + 1.
						x := ttMoveToX * sx + nextGlyphX.
						y := ttMoveToY * sy + nextGlyphY + deltaYForDiacriticalMark.
						contourStart := x@y.
						self moveTo: contourStart.
						numBeziers timesRepeat: [
							ttEndX := contourData at: i. i := i + 1.
							ttEndY := contourData at: i. i := i + 1.
							ttControlX := contourData at: i. i := i + 1.
							ttControlY := contourData at: i. i := i + 1.
							cx := ttControlX * sx + x.
							cy := ttControlY * sy + y.
							x := ttEndX * sx + x.
							y := ttEndY * sy + y.
							self quadraticBezierTo: x@y control: cx@cy
							].
						self lineTo: contourStart.
						].
					nextGlyphX := nextGlyphX + (advanceWidth * sx) - deltaXForDiacriticalMark.
					baseIndex := 0.
					byte1 := nil.
					byte2OrNil := nil.
				]
			]
	].
	^ nextGlyphX@nextGlyphY! !

