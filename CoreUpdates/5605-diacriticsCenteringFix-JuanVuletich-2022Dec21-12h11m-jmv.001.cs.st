'From Cuis 6.0 [latest update: #5604] on 21 December 2022 at 12:25:22 pm'!

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 12/21/2022 12:23:18'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers baseIndex x y cx cy contourStart deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark |
	deltaXForBaseGlyph _ 0.0.
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	self fillColor: aColor do: [
		nextGlyphX _ aPoint x.
		nextGlyphY _ aPoint y.
		baseIndex _ 0.
		byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
			byte _ aByteArray at: byteIndex.
			i _ contourDataIndexes at: baseIndex + byte + 1.
			i < 0
				ifTrue: [ baseIndex _ i negated ]
				ifFalse: [
					advanceWidth _ contourData at: i. i _ i + 1.
					boundsLeft _ contourData at: i. i _ i + 1.
					boundsRight _ contourData at: i. i _ i + 1.
					boundsTop _ contourData at: i. i _ i + 1.
					boundsBottom _ contourData at: i. i _ i + 1.
					numContours _ (contourData at: i) asInteger. i _ i + 1.
					deltaXToCenter _ advanceWidth - (boundsRight+boundsLeft * 0.5).
					"TrueType doesn't speciy how to position diacritics wrt base character glyph.
					Horizontally center diacriticals wrt base glyph.
					Apparently OpenType can do better than TrueType, as it includes the GPOS table."
					advanceWidth = 0.0
						ifFalse: [
							deltaXForBaseGlyph _ deltaXToCenter.
							deltaXForDiacriticalMark _ 0.0 ]
						ifTrue: [
							(deltaXForBaseGlyph = 0.0  						"For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
									or: [boundsLeft < 0 and: [boundsRight > 0]])   "Some fonts have diacritics already 'centered'"
								ifTrue: [
									deltaXForDiacriticalMark _ 0.0 ]
								ifFalse: [ 	deltaXForDiacriticalMark _ deltaXToCenter - deltaXForBaseGlyph * sx ]].
					nextGlyphX _ nextGlyphX + deltaXForDiacriticalMark.
					numContours timesRepeat: [
						numBeziers _ (contourData at: i) asInteger. i _ i + 1.
						ttMoveToX _ contourData at: i. i _ i + 1.
						ttMoveToY _ contourData at: i. i _ i + 1.
						x _ ttMoveToX * sx + nextGlyphX.
						y _ ttMoveToY * sy + nextGlyphY.
						contourStart _ x@y.
						self moveTo: contourStart.
						numBeziers timesRepeat: [
							ttEndX _ contourData at: i. i _ i + 1.
							ttEndY _ contourData at: i. i _ i + 1.
							ttControlX _ contourData at: i. i _ i + 1.
							ttControlY _ contourData at: i. i _ i + 1.
							cx _ ttControlX * sx + x.
							cy _ ttControlY * sy + y.
							x _ ttEndX * sx + x.
							y _ ttEndY * sy + y.
							self quadraticBezierTo: x@y control: cx@cy
							].
						self lineTo: contourStart.
						].
					nextGlyphX _ nextGlyphX + (advanceWidth * sx) - deltaXForDiacriticalMark.
					baseIndex _ 0.
				]
			]
	].
	^ nextGlyphX@nextGlyphY! !


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 12/21/2022 12:24:55'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i deltaToXCenter |

	byte1 _ aByte1.
	byte2OrNil _ aByte2OrNil.
	byte3OrNil _ aByte3OrNil.
	byte4OrNil _ aByte4OrNil.
	i _ contourDataIndexesByUtf8 at: byte1 + 1.
	utf8ByteArray _ ByteArray with: byte1.
	byte2OrNil notNil ifTrue: [
		i _ contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil.
		byte3OrNil notNil ifTrue: [
			i _ contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil with: byte3OrNil.
			byte4OrNil notNil ifTrue: [
				i _ contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1.
				utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil with: byte3OrNil with: byte4OrNil ]]].

	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i _ 1 ].

	advanceWidth _ contourDataForUtf8 at: i. i _ i + 1.
	boundsLeft _ contourDataForUtf8 at: i. i _ i + 1.
	boundsRight _ contourDataForUtf8 at: i. i _ i + 1.
	boundsTop _ contourDataForUtf8 at: i. i _ i + 1.
	boundsBottom _ contourDataForUtf8 at: i. i _ i + 1.

	horizontalStrokesAtPixelCenter _ 0.5.
	xtraSpaceForAntiAliasingAndRounding _ 3.
	toPixelsScale _ pixelSize * 1.0 / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset _ (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth _ (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset _ (boundsBottom * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight _ (boundsBottom - boundsTop) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
	form _ Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	canvas _ VectorCanvas onFormWithSubPixelAntiAliasing: form.
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
	deltaToXCenter _ (advanceWidth - (boundsRight+boundsLeft * 0.5)) * toPixelsScale.
	"DoubleWidth code points that combine above previous and following characters.
	Codepoints between 16r035C and 16r0363.
	Example: o͢o
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaToXCenter _ 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			deltaXToCenter: deltaToXCenter! !

