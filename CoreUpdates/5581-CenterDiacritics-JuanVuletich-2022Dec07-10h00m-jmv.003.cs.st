'From Cuis 6.0 [latest update: #5580] on 7 December 2022 at 10:46:19 am'!
!classDefinition: #FormGlyph category: 'Graphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth deltaXToCenter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 12/5/2022 19:47:24'!
deltaXToCenter
	^deltaXToCenter! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 12/5/2022 19:44:16'!
deltaXToCenter: aNumber
	deltaXToCenter _ aNumber! !


!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 10:44:27'!
findCannonicalCompositionAndHash
	"Find our cannonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| byteIndex noDiacriticFound stopByteIndex cannonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Do a search for combining code points. If none is found, answer self."
	noDiacriticFound _ true.
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	[ noDiacriticFound and: [byteIndex <= stopByteIndex] ] whileTrue: [ | n |
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			(UnicodeCodePoint isDiacriticalMark: codePoint)
				ifTrue: [noDiacriticFound _ false]].
		byteIndex _ byteIndex + n ].
	"No diacritics? We are already normalized to NFC."
	noDiacriticFound ifTrue: [
		^self computeHash ].

	"Find our cannonical composition"
	cannonicalComposition _ self basicCannonicalDecomposition basicCannonicallyOrdered basicCannonicalComposition.
	(String is: bytes equalTo: cannonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash _ cannonicalComposition.
			cannonicalComposition computeHash ].! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 12/7/2022 10:45:19'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap deltaXForBaseGlyph deltaXForDiacriticalMark |
	deltaXForBaseGlyph _ 0.
	ascent _ aTrueTypeFont ascent.		  "expensive"
	lineGap _ aTrueTypeFont lineGap."expensive"
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	byteIndex _ byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph _ aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ] 
			into: nil.
		"Center diacriticals marks."
		glyph advanceWidth = 0.0
			ifFalse: [
				deltaXForBaseGlyph _ glyph deltaXToCenter.
				deltaXForDiacriticalMark _ 0.0 ]
			ifTrue: [
				deltaXForDiacriticalMark _ glyph deltaXToCenter.
				deltaXForDiacriticalMark = 0 ifFalse: [ "Only for those who specify a delta to center."
					deltaXForDiacriticalMark _ deltaXForDiacriticalMark - deltaXForBaseGlyph ].
				 ].
		destX _ (fdestX + glyph leftOffset + deltaXForDiacriticalMark) roundedHAFZ.
		destY _ aPoint y + ascent + lineGap-1 - glyph topOffset.
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (ascent*0.3)]].
		destY _ destY roundedHAFZ.
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height.
		byteIndex _ byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight)! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 12/7/2022 10:45:52'!
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
							(boundsLeft < 0 and: [boundsRight > 0])
								ifTrue: [ deltaXForDiacriticalMark _ 0.0 ]
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


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 12/7/2022 10:27:21'!
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
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaToXCenter _ 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			deltaXToCenter: deltaToXCenter! !

!classDefinition: #FormGlyph category: 'Graphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth deltaXToCenter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TrueTypeFont allInstancesDo: [ :e | e releaseCachedState ].!

