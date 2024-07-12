'From Cuis7.1 [latest update: #6513] on 12 July 2024 at 9:56:39 am'!

!TTGlyph commentStamp: 'jmv 7/12/2024 08:30:22' prior: 0!
This class represents a glyph of a TrueType font.

Instance variables:
	boundsLeft 					xMin in TrueType parlance
	boundsRight 			 		xMax in TrueType parlance
	boundsBottom 					yMin in TrueType parlance (usually close to baseline, i.e. 0)
	boundsTop 					yMax in TrueType parlance (usually positive and large, in TrueType, y axis points up)
	contours					<Array of: TTContourConstruction>
	advanceWidth					<Integer>	advance width of the glyph
	leftSideBearing  					<Integer>	left side bearing
	rightSideBearing  					<Integer>	right side bearing!


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 7/12/2024 09:15:44'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsLeft boundsRight boundsBottom boundsTop i deltaToXCenter |

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
	deltaToXCenter := (advanceWidth - (boundsRight+boundsLeft * 0.5)) * toPixelsScale.
	"DoubleWidth code points that combine above previous and following characters.
	Codepoints between 16r035C and 16r0363.
	Example: o͢o
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaToXCenter := 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			deltaXToCenter: deltaToXCenter! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 7/12/2024 08:36:07'!
processCompositeGlyphFrom: entry
	"Read a composite glyph from the font data."
	| glyph flags glyphIndex hasInstructions ofsX ofsY iLen geometryTransformation xMin yMin xMax yMax |
	glyph := TTCompositeGlyph new.
	"See, for instance, https://learn.microsoft.com/en-us/typography/opentype/otspec190/glyf"
	xMin := entry nextInt16.
	yMin := entry nextInt16.
	xMax := entry nextInt16.
	yMax := entry nextInt16.
	glyph boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin.
	hasInstructions := false.
	[
		flags := entry nextUint16.
		glyphIndex := entry nextUint16 + 1.

		(flags allMask: 1 "ARG_1_AND_2_ARE_WORDS") ifTrue: [
			"'Hay Offset short x@y' print."
			ofsX := entry nextInt16.
			ofsY := entry nextInt16.
		] ifFalse: [
			"'Hay Offset byte x@y' print."
			(ofsX := entry nextByte) > 127 ifTrue: [ofsX := ofsX - 256].
			(ofsY := entry nextByte) > 127 ifTrue: [ofsY := ofsY - 256]].

		(flags allMask: 2 "ARGS_ARE_XY_VALUES") ifFalse: [self error: 'Unsupported TrueType format']. "Unsupported"
		"ROUND_XY_TO_GRID = 4. Ignored"
		(flags anyMask: 8+64+128) ifFalse: [
			geometryTransformation := MorphicTranslation withTranslation: ofsX@ofsY ].
		(flags allMask: 8 "WE_HAVE_A_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation withScale: entry nextInt16 asFloat / 16r4000 ].
		(flags allMask: 64 "WE_HAVE_AN_X_AND_Y_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation
				withPointScale: (entry nextInt16 asFloat / 16r4000)@(entry nextInt16 asFloat / 16r4000)
				position: ofsX@ofsY ].
		(flags allMask: 128 "WE_HAVE_A_TWO_BY_TWO") ifTrue: [
			"2x2 transformation"
			geometryTransformation := AffineTransformation new
				a11: entry nextInt16 asFloat / 16r4000;
				a21: entry nextInt16 asFloat / 16r4000;
				a12: entry nextInt16 asFloat / 16r4000;
				a22: entry nextInt16 asFloat / 16r4000;
				a13: ofsX;
				a23: ofsY ].

		glyph addGlyph: glyphIndex transformation: geometryTransformation.
		hasInstructions := hasInstructions or: [ flags allMask: 256 "WE_HAVE_INSTRUCTIONS"].
		"Continue as long as the MORE_COMPONENTS bit is set"
		flags allMask: 32 "MORE_COMPONENTS"
	] whileTrue.
	hasInstructions ifTrue: [
		"Ignore them"
		iLen := entry nextUint16.
		entry skip: iLen].
	^glyph! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 7/12/2024 08:37:25'!
processSimpleGlyphContours: nContours from: entry

	| glyph endPts nPts iLength flags xMin yMin xMax yMax |
	glyph := TTGlyph new.
	"See, for instance, https://learn.microsoft.com/en-us/typography/opentype/otspec190/glyf"
	xMin := entry nextInt16.
	yMin := entry nextInt16.
	xMax := entry nextInt16.
	yMax := entry nextInt16.
	glyph boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin.
	endPts := Array new: nContours.
	1 to: nContours do: [ :i | endPts at: i put: entry nextUint16 ].
	glyph initializeContours: nContours with: endPts.
	nContours = 0 ifTrue: [^ self].
	nPts := endPts last + 1.
	iLength := entry nextUint16. "instruction length"
	entry skip: iLength.
	flags := self getGlyphFlagsFrom: entry size: nPts.
	self readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	self readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	glyph makePointsAbsolute.
	^glyph! !


!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 08:31:51'!
boundsBottom
	"yMin in TrueType parlance (usually close to baseline, i.e. 0)"
	^boundsBottom! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 08:30:58'!
boundsLeft
	"xMin in TrueType parlance"
	^boundsLeft! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 08:36:50'!
boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin
	boundsLeft := xMin.
	boundsRight := xMax.
	boundsTop := yMax.
	boundsBottom := yMin.! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 08:31:18'!
boundsRight
	"xMax in TrueType parlance"
	^boundsRight! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 08:31:36'!
boundsTop
	"yMax in TrueType parlance (usually positive and large, in TrueType, y axis points up)"
	^boundsTop! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 7/12/2024 09:49:04'!
topSideBearing
	"
	https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^boundsTop! !

!TTGlyph methodsFor: 'primitive format' stamp: 'jmv 7/12/2024 08:45:22'!
addGlyphDataTo: stream tx: aGeometryTransformationOrNil
	| aw lb rt |
	aw := self advanceWidth.
	lb := self boundsLeft @ self boundsBottom.
	rt := self boundsRight @ self boundsTop.
	aGeometryTransformationOrNil ifNotNil: [
		aw := aGeometryTransformationOrNil externalizeScalar: aw.
		lb := aGeometryTransformationOrNil externalizePosition: lb.
		rt := aGeometryTransformationOrNil externalizePosition: rt ].
	stream nextPut: aw.
	stream nextPut: lb x. "left = xMin"
	stream nextPut: rt x. "right = xMax"
	stream nextPut: lb y. "bottom = yMin"
	stream nextPut: rt y. "top = yMax"
	stream nextPut: self contourCount.
	self addContourDataTo: stream tx: aGeometryTransformationOrNil! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 7/12/2024 08:46:36'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours numBeziers baseIndex x y cx cy contourStart deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark |
	deltaXForBaseGlyph := 0.0.
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
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
					deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.5).
					"TrueType doesn't speciy how to position diacritics wrt base character glyph.
					Horizontally center diacriticals wrt base glyph.
					Apparently OpenType can do better than TrueType, as it includes the GPOS table."
					advanceWidth = 0.0
						ifFalse: [
							deltaXForBaseGlyph := deltaXToCenter.
							deltaXForDiacriticalMark := 0.0 ]
						ifTrue: [
							(deltaXForBaseGlyph = 0.0  						"For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
									or: [boundsLeft < 0 and: [boundsRight > 0]])   "Some fonts have diacritics already 'centered'"
								ifTrue: [
									deltaXForDiacriticalMark := 0.0 ]
								ifFalse: [ 	deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph * sx ]].
					nextGlyphX := nextGlyphX + deltaXForDiacriticalMark.
					numContours timesRepeat: [
						numBeziers := (contourData at: i) asInteger. i := i + 1.
						ttMoveToX := contourData at: i. i := i + 1.
						ttMoveToY := contourData at: i. i := i + 1.
						x := ttMoveToX * sx + nextGlyphX.
						y := ttMoveToY * sy + nextGlyphY.
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
TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'.
FontFamily releaseClassCachedState.
!

