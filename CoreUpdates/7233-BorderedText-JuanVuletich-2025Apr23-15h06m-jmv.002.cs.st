'From Cuis7.3 [latest update: #7145] on 23 April 2025 at 3:18:09 pm'!

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 4/23/2025 14:41:01'!
drawOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color borderWidth: sw borderColor: sc

	| byteStartIndex byteStopIndex p answer d |
	byteStartIndex := self byteIndexAt: startIndex.
	byteStopIndex := (self byteIndexAt: stopIndex+1)-1.
	"Please keep consistent with #basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:"
	p := aTrueTypeFont isSuperscript
		ifTrue: [ aPoint x  @ (aPoint y - (aTrueTypeFont ascent*0.55)) ]
		ifFalse: [
			aTrueTypeFont isSubscript
				ifTrue: [aPoint x @ (aPoint y + (aTrueTypeFont ascent*0.3))]
				ifFalse: [ aPoint ]].
	answer := engine
		drawUtf8String: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		strokeWidth: sw strokeColor: sc
		scale: aTrueTypeFont pointSize
		preventMirroring: true.
	"Please keep consistent with #makeUnderlinedGlyph:"
	aTrueTypeFont isUnderlined ifTrue: [
		d := aTrueTypeFont descent *0.7.
		engine strokeWidth: 2 color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := aTrueTypeFont ascent * 0.25.
		engine strokeWidth: 1 color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/23/2025 15:16:16'!
drawString: s at: pt font: fontOrNil color: aColor borderWidth: sw borderColor: sc
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor borderWidth: sw borderColor: sc! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/23/2025 15:13:45'!
drawString: s atBaseline: pt font: fontOrNil color: aColor borderWidth: sw borderColor: sc
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atBaseline: pt font: font color: aColor borderWidth: sw borderColor: sc! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/23/2025 15:15:21'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor borderWidth: sw borderColor: sc
	"Eventually, convert all senders to the 'Baseline' protocol"

	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0@(font ascent + font lineGap-1))
		font: font color: aColor
		borderWidth: sw borderColor: sc! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/23/2025 15:13:16'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc
	"Ignore border if not supported"
	^self drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 4/23/2025 14:43:34'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
(c drawString: s from: 1 to: s size atBaseline: 100@180 font: f color: Color black borderWidth: 3 borderColor: Color red) print.
c strokeWidth: 1 color: Color black do: [ c moveTo: 100@180; lineTo: 1000@180 ].
Display forceToScreen
	"
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	^ aString asUnicodeString drawOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc.! !


!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/23/2025 15:11:57'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc

	^vectorCanvas drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/23/2025 15:08:19'!
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

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/23/2025 15:08:34'!
drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor strokeWidth: sw strokeColor: sc scale: scale preventMirroring: doPreventMirroring
	"Allows adding a colored border to text display.
	TrueType assumes a Math like coordinate system (Y increasing upwards).
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
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self strokeWidth: sw color: sc fillColor: aColor do: [
		nextGlyphX := aPoint x asFloat. "If #strokeWidth:color:fillColor:do: decides to evaluate this block more than once, set correct start x each time."
		nextGlyphX := self
			displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.! !

