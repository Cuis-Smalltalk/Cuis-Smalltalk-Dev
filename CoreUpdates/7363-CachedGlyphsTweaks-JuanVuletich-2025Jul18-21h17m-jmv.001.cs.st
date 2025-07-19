'From Cuis7.5 [latest update: #7362] on 19 July 2025 at 11:44:29 am'!

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 7/19/2025 11:42:12'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset form canvas advanceWidth boundsLeft boundsRight boundsBottom boundsTop i deltaXToCenter xtraRoom usedRect |

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
	usedRect := canvas engine morphBoundsAfterDraw.
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FontFamily releaseClassCachedState.!

