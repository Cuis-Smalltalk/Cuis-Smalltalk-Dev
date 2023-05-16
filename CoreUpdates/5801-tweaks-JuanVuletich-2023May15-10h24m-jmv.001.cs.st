'From Cuis 6.0 [latest update: #5799] on 15 May 2023 at 10:26:16 am'!

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/15/2023 10:21:11'!
ascent
	^ 0.0 + ttFontDescription ascent * pointSize / ttFontDescription letterMTopSideBearing! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/15/2023 10:21:23'!
descent
	^ 0.0 + ttFontDescription descent negated * pointSize / ttFontDescription letterMTopSideBearing! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/15/2023 10:21:34'!
lineGap
	^ 0.0 + ttFontDescription lineGap * pointSize / ttFontDescription letterMTopSideBearing! !


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 5/15/2023 10:24:34'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i deltaToXCenter |

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
	boundsTop := contourDataForUtf8 at: i. i := i + 1.
	boundsBottom := contourDataForUtf8 at: i. i := i + 1.

	horizontalStrokesAtPixelCenter := 0.5.
	xtraSpaceForAntiAliasingAndRounding := 3.
	toPixelsScale := 0.0 + pixelSize / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset := (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth := (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset := (boundsBottom * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight := (boundsBottom - boundsTop) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
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

