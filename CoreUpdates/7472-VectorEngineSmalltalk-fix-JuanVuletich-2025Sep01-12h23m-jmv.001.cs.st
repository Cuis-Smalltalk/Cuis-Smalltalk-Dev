'From Cuis7.5 [latest update: #7471] on 1 September 2025 at 12:26:15 pm'!

!VectorEngine methodsFor: 'private' stamp: 'jmv 9/1/2025 12:25:55'!
displayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.0.
	nextGlyphX := destX.
	startIndex to: stopIndex do: [ :utf32Index | | utf32 i |
		utf32 := aWordArray at: utf32Index.
		
		"$i followed by a Combining Diacritical Mark is replaced by CodePoint 16r131 Dotless i
		This should also be done for VectorEngineWithPlugin"
		(utf32 = 16r69 and: [ utf32Index < stopIndex ] and: [
			(aWordArray at: utf32Index+1) between: 16r300 and: 16r36F ])
				ifTrue: [ utf32 := 16r131 ].
		i :=  self contourDataIndexForUtf32: utf32 contourDataIndexes: contourDataIndexes.
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
		
		self
			glyphContours: numContours
			atx: nextGlyphX + deltaXForDiacriticalMark y: destY + deltaYForDiacriticalMark
			scalex: sx y: sy
			contourData: contourData i: i.
		
		nextGlyphX := nextGlyphX + (advanceWidth * sx) ].
	^nextGlyphX! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 9/1/2025 12:26:02'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX byte1 byte2OrNil baseIndex advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.0.
	nextGlyphX := destX.
	byte1 := nil.
	byte2OrNil := nil.
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
				
				self
					glyphContours: numContours
					atx: nextGlyphX + deltaXForDiacriticalMark y: destY + deltaYForDiacriticalMark
					scalex: sx y: sy
					contourData: contourData i: i.
				
				nextGlyphX := nextGlyphX + (advanceWidth * sx).
				baseIndex := 0.
				byte1 := nil.
				byte2OrNil := nil ]].
	^nextGlyphX! !

