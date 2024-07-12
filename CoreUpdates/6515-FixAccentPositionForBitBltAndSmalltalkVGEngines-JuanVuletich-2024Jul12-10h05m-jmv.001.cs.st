'From Cuis7.1 [latest update: #6514] on 12 July 2024 at 10:23:01 am'!

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 7/11/2024 17:14:13'!
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
	^ fdestX ceiling@(destY+maxHeight)! !


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 7/12/2024 10:09:09'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsLeft boundsRight boundsBottom boundsTop i deltaXToCenter |

	byte1 := aByte1.
	byte2OrNil := aByte2OrNil.
	byte3OrNil := aByte3OrNil.
	byte4OrNil := aByte4OrNil.
	i := contourDataIndexesByUtf8 at: byte1 + 1.
	utf8ByteArray := ByteArray with: byte1.
	byte2OrNil notNil ifTrue: [
		i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		utf8ByteArray := ByteArray with: byte1 with: byte2OrNil.
		byte3OrNil notNil ifTrue: [
			i := contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			utf8ByteArray := ByteArray with: byte1 with: byte2OrNil with: byte3OrNil.
			byte4OrNil notNil ifTrue: [
				i := contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1.
				utf8ByteArray := ByteArray with: byte1 with: byte2OrNil with: byte3OrNil with: byte4OrNil ]]].

	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i := 1 ].

	advanceWidth := contourDataForUtf8 at: i. i := i + 1.
	boundsLeft := contourDataForUtf8 at: i. i := i + 1.
	boundsRight := contourDataForUtf8 at: i. i := i + 1.
	boundsBottom := contourDataForUtf8 at: i. i := i + 1.
	boundsTop := contourDataForUtf8 at: i. i := i + 1.

	horizontalStrokesAtPixelCenter := 0.5.
	xtraSpaceForAntiAliasingAndRounding := 3.
	toPixelsScale := 0.0 + pixelSize / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset := (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth := (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset := (boundsTop * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight := (boundsTop - boundsBottom) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
	form := Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	canvas := VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			displayUtf8: utf8ByteArray
			fromByte: 1
			toByte: utf8ByteArray size
			at: leftPixelOffset negated@(topPixelOffset - horizontalStrokesAtPixelCenter)
			trueTypeFont: self
			color: Color black
			scale: 1.0 * pixelSize
			preventMirroring: true.

	"TrueType doesn't speciy how to position diacritics wrt base character glyph.
	Horizontally center diacriticals wrt base glyph.
	See
	basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:
	Apparently OpenType can do better than TrueType, as it includes the GPOS table."
	deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.51)) * toPixelsScale.
	byte1 = 204 ifTrue: [
		byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
			deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.43)) * toPixelsScale ].
		byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
			deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.65)) * toPixelsScale 			]		].

	"DoubleWidth code points that combine above previous and following characters.
	Codepoints between 16r035C and 16r0363.
	Example: o͢o
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaXToCenter := 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			deltaXToCenter: deltaXToCenter.
! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 7/12/2024 10:19:18'!
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
	byte1 := aByteArray at: byteStartIndex.
	byte2OrNil := byteStopIndex > byteStartIndex ifTrue: [ aByteArray at: byteStartIndex +1 ].
	self fillColor: aColor do: [
		nextGlyphX := aPoint x.
		nextGlyphY := aPoint y.
		baseIndex := 0.
		byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
			byte := aByteArray at: byteIndex.
			i := contourDataIndexes at: baseIndex + byte + 1.
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
					Apparently OpenType can do better than TrueType, as it includes the GPOS table."
					deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
					byte1 = 204 ifTrue: [
						byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
						byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) 			]		].

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
				]
			]
	].
	^ nextGlyphX@nextGlyphY! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FontFamily releaseClassCachedState!

