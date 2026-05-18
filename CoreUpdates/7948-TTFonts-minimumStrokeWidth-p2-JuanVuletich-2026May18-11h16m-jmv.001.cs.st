'From Cuis7.7 [latest update: #7947] on 18 May 2026 at 11:17:06 am'!

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 18/May/2026 11:09:42'!
drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	"
	| e f s byteStartIndex byteStopIndex |
	'-----Try all FOUR of them!!' print.
	Display fillColor: Color veryLightGray.
	e := VectorEngineWholePixel onForm: Display.
	e := VectorEngineSubPixel onForm: Display.
	e := VectorEngineWithPluginWholePixel onForm: Display.
	e := VectorEngineWithPluginSubPixel onForm: Display.
	f := 	FontFamily defaultFamilyAndPointSize fontDescription.
	s := 'Hello World!! This is TrueType text rasterization in VectorEngine.' asUnicodeString.
	byteStartIndex := s byteIndexAt: 1.
	byteStopIndex := (s byteIndexAt: s size+1)-1.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@150 trueTypeFont: f color: Color random scale: 130 preventMirroring: true] timeToRun print.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@300 trueTypeFont: f color: Color random strokeWidth: 2 strokeColor: Color black scale: 130 preventMirroring: true] timeToRun print.
	Display forceToScreen.
	"
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	aTTFontDescription minimumStrokeWidth = 0.0 ifFalse: [
		^self drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor
			strokeWidth: aTTFontDescription minimumStrokeWidth strokeColor: aColor
			scale: scale preventMirroring: doPreventMirroring ].
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := aPoint x asFloat. "If #fillColor:do: decides to evaluate this block more than once, set correct start x each time."
		nextGlyphX := self
			displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.! !


!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 18/May/2026 11:06:30'!
drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	"
	| e f s byteStartIndex byteStopIndex |
	'-----Try all FOUR of them!!' print.
	Display fillColor: Color veryLightGray.
	e := VectorEngineWholePixel onForm: Display.
	e := VectorEngineSubPixel onForm: Display.
	e := VectorEngineWithPluginWholePixel onForm: Display.
	e := VectorEngineWithPluginSubPixel onForm: Display.
	f := 	FontFamily defaultFamilyAndPointSize fontDescription.
	s := 'Hello World!! This is TrueType text rasterization in VectorEngine.' asUnicodeString.
	byteStartIndex := s byteIndexAt: 1.
	byteStopIndex := (s byteIndexAt: s size+1)-1.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@150 trueTypeFont: f color: Color random scale: 130 preventMirroring: true] timeToRun print.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@300 trueTypeFont: f color: Color random strokeWidth: 2 strokeColor: Color black scale: 130 preventMirroring: true] timeToRun print.
	Display forceToScreen.
	"
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	aTTFontDescription minimumStrokeWidth = 0.0 ifFalse: [
		^self drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor
			strokeWidth: aTTFontDescription minimumStrokeWidth strokeColor: aColor
			scale: scale preventMirroring: doPreventMirroring ].
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.! !

