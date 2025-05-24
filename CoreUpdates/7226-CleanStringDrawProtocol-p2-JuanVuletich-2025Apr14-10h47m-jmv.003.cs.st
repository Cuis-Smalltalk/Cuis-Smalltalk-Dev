'From Cuis7.3 [latest update: #7133] on 14 April 2025 at 10:50:11 am'!

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:42:27'!
drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try BOTH!!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World!! (Ascii Only. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.'.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers x y cx cy contourStart |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.	 "Should use a specific table with 256 entries"
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8. "Should use a specific table with 256 entries"
	self fillColor: aColor do: [
		nextGlyphX := aPoint x.
		nextGlyphY := aPoint y.
		startIndex to: stopIndex do: [ :charIndex | | latin1Code i |
			latin1Code := (aString at: charIndex) asciiValue.
			i := contourDataIndexes at: latin1Code + 1.
			advanceWidth := contourData at: i. i := i + 1.
			boundsLeft := contourData at: i. i := i + 1.
			boundsRight := contourData at: i. i := i + 1.
			boundsBottom := contourData at: i. i := i + 1.
			boundsTop := contourData at: i. i := i + 1.
			numContours := (contourData at: i) asInteger. i := i + 1.
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
					self quadraticBezierTo: x@y control: cx@cy ].
				self lineTo: contourStart ].
			nextGlyphX := nextGlyphX + (advanceWidth * sx) ]].
	^ nextGlyphX@nextGlyphY! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:43:52'!
drawUtf32String: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f utf32 |
'Try BOTH!!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.
e drawUtf32String: utf32 from: 1 to: utf32 size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"
"
| e f utf8a utf8b utf8c utf32a utf32b utf32c |
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf8a := #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137 ].
utf8b := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97	].
utf8c := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 105 195 173 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97 105 105 204 129 	].
utf32a := #(105 105 769 32 105 768 32 105 770 32 105 771 32 105 772 32 105 773 32 105 774 32 105 775 32 105 776 32 105 777) asWordArray.
utf32b :=  #(97 233 97 232 97 241 97 209 252 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97) asWordArray.
utf32c :=  #(97 233 97 232 97 241 97 209 252 105 237 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97 105 105 769) asWordArray.
e drawUtf8String: utf8a fromByte: 1 toByte: utf8a size at: 10@120 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32a from: 1 to: utf32a size at: 10@220 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8b fromByte: 1 toByte: utf8b size at: 10@320 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32b from: 1 to: utf32b size at: 10@420 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8c fromByte: 1 toByte: utf8c size at: 10@520 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32c from: 1 to: utf32c size at: 10@620 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers x y cx cy contourStart deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseBoundsTop deltaYForDiacriticalMark |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	self fillColor: aColor do: [
		nextGlyphX := aPoint x.
		nextGlyphY := aPoint y.
		startIndex to: stopIndex do: [ :utf32Index | | utf32 utf8Byte1 uft8Byte2 utf8Byte3 utf8Byte4 i |
			utf32 := aWordArray at: utf32Index.
			
			"$i followed by a Combining Diacritical Mark is replaced by CodePoint 16r131 Dotless i
			This should also be done for VectorEngineWithPlugin"
			(utf32 = 16r69 and: [ utf32Index < stopIndex ] and: [
				(aWordArray at: utf32Index+1) between: 16r300 and: 16r36F ])
					ifTrue: [ utf32 := 16r131 ].
			
			utf32 <= 16r7F
				ifTrue: [
					utf8Byte1 := utf32.
					i := contourDataIndexes at: utf8Byte1 + 1 ]
				ifFalse: [ utf32 <= 16r7FF
				ifTrue: [
					utf8Byte1 := (utf32 bitShift: -6) bitOr: 2r11000000.
					uft8Byte2 := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte1 + 1.
					i := contourDataIndexes at: uft8Byte2 - i + 1 ]
				ifFalse: [ utf32 <= 16rFFFF
				ifTrue: [
					utf8Byte1 := (utf32 bitShift: -12) bitOr: 2r11100000.
					uft8Byte2 := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte3 := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte1 + 1.
					i := contourDataIndexes at: uft8Byte2 - i + 1.
					i := contourDataIndexes at: utf8Byte3 - i + 1 ]
				ifFalse: [
					utf8Byte1 := (utf32 bitShift: -18) bitOr: 2r11110000.
					uft8Byte2 := ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte3 := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte4 := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte1 + 1.
					i := contourDataIndexes at: uft8Byte2 - i + 1.
					i := contourDataIndexes at: utf8Byte3 - i + 1.
					i := contourDataIndexes at: utf8Byte4 - i + 1 ]]].
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
				displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:
			This should also be done for VectorEngineWithPlugin
			Apparently OpenType can do better than TrueType, as it includes the GPOS table..."
			deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
			utf32 = 16r300 ifTrue: [	"Combining Grave Accent"
				deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
			utf32 = 16r301 ifTrue: [ 	"Combining Acute Accent"
				deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) ].
			"DoubleWidth code points that combine above previous and following characters.
			Codepoints between 16r035C and 16r0363.
			Example: o͢o
			Don't center them"
			(utf32 between: 16r035C and: 16r0363) ifTrue: [
				deltaXToCenter := 0.0 ].
					
			deltaXForDiacriticalMark := 0.0.
			deltaYForDiacriticalMark := 0.
			advanceWidth = 0.0
				ifFalse: [ "Regular glyph"
					deltaXForBaseGlyph := deltaXToCenter.
					baseBoundsTop := boundsTop ]
				ifTrue: [ "Diacritical mark"
					deltaXForBaseGlyph = 0.0  "For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
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
					self quadraticBezierTo: x@y control: cx@cy ].
				self lineTo: contourStart ].
			nextGlyphX := nextGlyphX + (advanceWidth * sx) - deltaXForDiacriticalMark ]].
	^ nextGlyphX@nextGlyphY! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:36:40'!
drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
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
	byte1 := nil.
	byte2OrNil := nil.
	self fillColor: aColor do: [
		nextGlyphX := aPoint x.
		nextGlyphY := aPoint y.
		baseIndex := 0.
		byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
			byte := aByteArray at: byteIndex.
			byte1 isNil ifTrue: [byte1 := byte ] ifFalse: [ byte2OrNil isNil ifTrue: [ byte2OrNil := byte ]].
			i := contourDataIndexes at: baseIndex + byte + 1.
			
			"$i followed by a Combining Diacritical Mark is replaced by CodePoint 305 Dotless i, UTF-8 #[196 177]
			(UnicodeString fromUtf8Bytes: #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32
				105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137 ]) edit
			This should also be done for VectorEngineWithPlugin"
			(byte1 = 105 and: [ byteIndex < (byteStopIndex-1) ] and: [
				(aByteArray at: byteIndex+1) = 204 or: [ (aByteArray at: byteIndex+1) = 205 and: [ (aByteArray at: byteIndex+2) < 176 ]]])
					ifTrue: [
						i := contourDataIndexes at: 196 + 1.
						i := contourDataIndexes at: i negated + 177 + 1 ].
			
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
						displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:
					This should also be done for VectorEngineWithPlugin
					Apparently OpenType can do better than TrueType, as it includes the GPOS table..."
					deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
					byte1 = 204 ifTrue: [
						byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
						byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
							deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) ]].
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
							deltaXForBaseGlyph = 0.0  "For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
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
							self quadraticBezierTo: x@y control: cx@cy ].
						self lineTo: contourStart ].
					nextGlyphX := nextGlyphX + (advanceWidth * sx) - deltaXForDiacriticalMark.
					baseIndex := 0.
					byte1 := nil.
					byte2OrNil := nil ]]].
	^ nextGlyphX@nextGlyphY! !


!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:40:56'!
drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try BOTH!!'.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World!! (Ascii Only. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.'.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.	 "Should use a specific table with 256 entries"
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8. "Should use a specific table with 256 entries"
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:41:45'!
drawUtf32String: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f utf32 |
'Try BOTH!!'.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.
e drawUtf32String: utf32 from: 1 to: utf32 size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen."

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
		nextGlyphX := self
			primDisplayUtf32: aWordArray from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/14/2025 10:37:30'!
drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

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
		nextGlyphX := self
			primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !


!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 4/14/2025 10:22:23'!
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
		drawUtf8String: bytes
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


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 4/14/2025 10:22:32'!
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
			drawUtf8String: utf8ByteArray
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
			deltaXToCenter: deltaXToCenter.
! !

!methodRemoval: VectorEngineWithPlugin #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngineWithPlugin removeSelector: #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngineWithPlugin #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngineWithPlugin removeSelector: #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngineWithPlugin #displayByteString:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngineWithPlugin removeSelector: #displayByteString:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngine #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngine removeSelector: #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngine #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngine removeSelector: #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngine #displayByteString:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 4/14/2025 10:49:20'!
VectorEngine removeSelector: #displayByteString:from:to:at:trueTypeFont:color:scale:preventMirroring:!
