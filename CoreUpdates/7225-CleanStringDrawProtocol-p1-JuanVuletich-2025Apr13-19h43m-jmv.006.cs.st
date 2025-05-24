'From Cuis7.3 [latest update: #7132] on 13 April 2025 at 7:51:55 pm'!

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 4/13/2025 19:45:19'!
drawOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	| byteStartIndex byteStopIndex |
	byteStartIndex := self byteIndexAt: startIndex.
	byteStopIndex := (self byteIndexAt: stopIndex+1)-1.
	^engine
		drawUtf8: bytes
		fromByte: byteStartIndex
		toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 4/13/2025 19:44:38'!
drawOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color

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
		displayUtf8: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
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


!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 4/13/2025 19:46:37'!
drawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	sourceForm := nil. "Set it for each glyph"
	sourceX := 0.
	sourceY := 0.
	height := nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap := colorMap.
	sourceDepth := aTrueTypeFont cachedFormDepth.
	destDepth := destForm depth.
	colorMap := sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 4/13/2025 19:46:11'!
basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
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


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 4/13/2025 19:44:46'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString asUnicodeString drawOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/13/2025 19:43:40'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	| p1 answer |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	currentTransformation isPureTranslation ifTrue: [
		(Preferences at: #cacheTrueTypeGlyphs) ifTrue: [
			p1 := currentTransformation transform: aPoint roundedHAFZ.
			p1 := p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 := p1 roundedHAFZ.
			answer := aString asUnicodeString drawOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/13/2025 19:43:44'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 := currentTransformation transform: aPoint roundedHAFZ.
	p1 := p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 := p1 roundedHAFZ.
	engine colorMap: nil.
	^aString asUnicodeString drawOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/13/2025 19:43:48'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s asUnicodeString drawOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor! !

!methodRemoval: BitBltCanvasEngine #basicDisplayUtf8:fromByte:toByte:at:trueTypeFont: stamp: 'jmv 4/13/2025 19:46:42'!
BitBltCanvasEngine removeSelector: #basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:!
!methodRemoval: BitBltCanvasEngine #displayUtf8:fromByte:toByte:at:trueTypeFont:color: stamp: 'jmv 4/13/2025 19:45:23'!
BitBltCanvasEngine removeSelector: #displayUtf8:fromByte:toByte:at:trueTypeFont:color:!
!methodRemoval: UnicodeString #displayOnVectorEngine:from:to:at:font:color: stamp: 'jmv 4/13/2025 19:44:49'!
UnicodeString removeSelector: #displayOnVectorEngine:from:to:at:font:color:!
!methodRemoval: UnicodeString #displayOnBitBltCanvasEngine:from:to:at:font:color: stamp: 'jmv 4/13/2025 19:43:52'!
UnicodeString removeSelector: #displayOnBitBltCanvasEngine:from:to:at:font:color:!
