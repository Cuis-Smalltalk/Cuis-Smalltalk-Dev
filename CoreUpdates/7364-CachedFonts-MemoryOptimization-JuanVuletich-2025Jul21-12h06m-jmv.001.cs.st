'From Cuis7.5 [latest update: #7363] on 21 July 2025 at 12:09:47 pm'!

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 7/21/2025 11:47:00'!
contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil
	"Answer the position in contourDataForUtf8 for requested UTF-8 encoded code point.
	Answer 0 if we don't include it.
	Entries in contourDataIndexesByUtf8 may be:
		> 1: actual index in countourData for this CodePoint
		= 1: No contourData for this CodePoint. Use the UnknownGlyph, stored at position 1. (If byte1 > 0)
		< 0: Another UTF-8 byte is needed.
		= 0: No contourData for this CodePoint. Use the UnknownGlyph, stored at position 1."

	| i |
	i := contourDataIndexesByUtf8 at: byte1 + 1.
	byte2OrNil notNil ifTrue: [
		i < 0 ifFalse: [ ^0 ].
		i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		byte3OrNil notNil ifTrue: [
			i < 0 ifFalse: [ ^0 ].
			i := contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			byte4OrNil notNil ifTrue: [
				i < 0 ifFalse: [ ^0 ].
				i := contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1 ]]].
	
	^i! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 7/21/2025 11:48:07'!
includesContourDataForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil
	"Answer true if we include a glyph definition for the requested UTF-8 encoded code point.
	Answer false if we don't include it.
	See comment at #contourDataIndexForUtf8Byte1:byte2:byte3:byte4:"

	"The unknown glyph is indeed included.
	See #appendGlyphDataTo:glyphsArray:indexes:base:bar:unknown:scaleArrowGlyphs:"
	byte1 = 1 ifTrue: [
		^true ].

	"But any other glyph redirected to it is not included."
	^(self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil) > 1! !


!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 7/21/2025 12:05:02'!
glyphAtUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil
	"Needed only for BitBltCanvasEngine"

	| lastArray lastIndex |
	
	"Avoid creating multiple cached instances for the unknown glyph. Just keep one!!"
	(ttFontDescription includesContourDataForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil)
		 ifFalse: [
			^self glyphAtUtf8Byte1: 1 byte2: nil byte3: nil byte4: nil ].
	lastArray := glyphFormsByUtf8 .
	lastIndex := byte1 + 1.
	byte2OrNil notNil ifTrue: [
		(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
		lastArray := lastArray at: lastIndex.
		lastIndex := (byte2OrNil bitAnd: 63) + 1.
		byte3OrNil notNil ifTrue: [
			(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
			lastArray := lastArray at: lastIndex.
			lastIndex := (byte3OrNil bitAnd: 63) + 1.
			byte4OrNil notNil ifTrue: [
				(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
				lastArray := lastArray at: lastIndex.
				lastIndex := (byte4OrNil bitAnd: 63) + 1 ]]].
	^ (lastArray at: lastIndex) ifNil: [
		| formGlyph |
		formGlyph := (ttFontDescription
				formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil
				byte3: byte3OrNil byte4: byte4OrNil
				pixelSize: pointSize)
			ifNotNil: [ :theGlyph |
				self isUnderlined ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				self isStruckThrough ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		lastArray at: lastIndex put: formGlyph.
		formGlyph ]! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 7/21/2025 12:08:11'!
appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex bar: aProgressBarOrNil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

		glyphsArray withIndexDo: [ :glyphOrArrayOrNumberNil :index |
			aProgressBarOrNil ifNotNil: [ :bar | bar value: index ].
			glyphOrArrayOrNumberNil ifNotNil: [
				glyphOrArrayOrNumberNil == theUnknownGlyph
					ifTrue: [ indexes at: baseIndex + index put: 1 ] "Set to unknownGlyph. UnknownGlyph is index 1."
					ifFalse: [
						glyphOrArrayOrNumberNil isArray
							ifTrue: [
								| nextBaseIndex |
								nextBaseIndex := indexes size - 128.
								indexes addAll: (IntegerArray new: 64).
								indexes at: baseIndex + index put: nextBaseIndex negated.
								self appendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNumberNil indexes: indexes
									base: nextBaseIndex bar: nil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]
							ifFalse: [
								glyphOrArrayOrNumberNil isNumber
									ifTrue: [ | glyph tx |
										glyph := arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx := AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing * 0.7.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].! !

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 7/21/2025 11:55:48'!
formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"

	| utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset form canvas advanceWidth boundsLeft boundsRight boundsBottom boundsTop i deltaXToCenter xtraRoom usedRect |

	"Get the position in contourData for our glyph definition."
	i := self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil.
	
	advanceWidth := contourDataForUtf8 at: i. i := i + 1.
	boundsLeft := contourDataForUtf8 at: i. i := i + 1.
	boundsRight := contourDataForUtf8 at: i. i := i + 1.
	boundsBottom := contourDataForUtf8 at: i. i := i + 1.
	boundsTop := contourDataForUtf8 at: i. i := i + 1.

	toPixelsScale := 0.0 + pixelSize / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed.
	Be sure to leave extra room so there's no risk of missing pixels."
	leftPixelOffset := (boundsLeft * toPixelsScale) rounded.
	formWidth := ((boundsRight - boundsLeft) * toPixelsScale) ceiling.
	xtraRoom := formWidth * 2 //10 + 3.
	leftPixelOffset := leftPixelOffset - xtraRoom.
	formWidth := formWidth + xtraRoom + xtraRoom.

	"Usually positive. Needed pixels above the baseline.
	Be sure to leave extra room so there's no risk of missing pixels."
	topPixelOffset := (boundsTop * toPixelsScale) ceiling.
	formHeight := ((boundsTop - boundsBottom) * toPixelsScale) ceiling.
	xtraRoom := formHeight * 2 //10 + 3.
	topPixelOffset := topPixelOffset + xtraRoom.
	formHeight := formHeight + xtraRoom + xtraRoom.
	
	"Draw our glyph."
	utf8ByteArray := { byte1. byte2OrNil. byte3OrNil. byte4OrNil} select: [ :b | b notNil ] :: asByteArray.
	form := Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
"form fillColor: (Color r: 01.0 g: 0.9 b: 0.9)." "Debug aid"
	canvas := VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			drawUtf8String: utf8ByteArray
			fromByte: 1
			toByte: utf8ByteArray size
			at: leftPixelOffset negated@topPixelOffset
			trueTypeFont: self
			color: Color black
			scale: 1.0 * pixelSize
			preventMirroring: true.

	"Now only take the pixels that are actually needed."
	usedRect := form boundingBox intersect: canvas engine morphBoundsAfterDraw.
	"usedRect := usedRect outsetBy: 1." "Debug aid"
	form := form copy: usedRect.
	leftPixelOffset := leftPixelOffset + usedRect left.
	topPixelOffset := topPixelOffset - usedRect top.

"Debug Aid"
"
f2 := form copy.
f2 fill: (form boundingBox insetBy: 1) fillColor: (f2 colorAt: 0@0).
(f2 tallyPixelValues count: [ :v | v > 0 ]) > 1 ifTrue: [
	Tota ifNil: [ Tota := OrderedCollection new ].
	Tota add: form.
	Tota2 ifNil: [ Tota2 := OrderedCollection new ].
	Tota2 add: utf8ByteArray.
].
"

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
			deltaXToCenter := (advanceWidth - (boundsRight+boundsLeft * 0.65)) * toPixelsScale ]].

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
			deltaXToCenter: deltaXToCenter.! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 7/21/2025 11:21:27'!
widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil

	| i advanceWidth |
	
	i := self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil.
	
	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i := 1 ].
	advanceWidth := contourDataForUtf8 at: i.
	^ advanceWidth / letterMTopSideBearing.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FontFamily releaseClassCachedState.!

