'From Cuis7.5 [latest update: #7364] on 21 July 2025 at 4:26:35 pm'!

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 7/21/2025 16:00:49'!
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
								"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
								as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
								indexes addAll: ((IntegerArray new: 64) atAllPut: 1).
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

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 7/21/2025 16:01:10'!
glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph := glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing := (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
	as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
	contourDataIndexesByUtf8 := ((IntegerArray new: 256) atAllPut: 1) as: OrderedCollection.
	contourDataForUtf8 := Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
		self fullName, ' - Processing glyph data' 
			displayProgressAt: 100@100
			from: 1 
			to: glyphsByUTF8 size 
			during: [ :bar |
				self appendGlyphDataTo: stream glyphsArray: glyphsByUTF8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]].
	contourDataIndexesByUtf8 := contourDataIndexesByUtf8 as: IntegerArray.
	"Any incomplete array sequence will point at 1. VectorEngine will show (Character codePoint: 500001)
	as four invalid glyphs and not just one. Real solution is for VectorEngine to skip ignored bytes."
	contourDataIndexesByLatin1 := (IntegerArray new: 256)  atAllPut: 1.
	0 to: 255 do: [ :cp | | i |
		Character
			evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
				i := contourDataIndexesByUtf8 at: byte1 + 1.
				(byte2OrNil notNil and: [ i < 0 ]) ifTrue: [
					i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1 ].
				contourDataIndexesByLatin1 at: cp+1 put: i ]
			withUtf8BytesOf: cp ].! !


!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 7/21/2025 16:03:24'!
readTrueTypeFontsIn: aDirectoryEntry
	"
	TTFontDescription readTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory
	"
	| read |
	read := OrderedDictionary new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf := TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
			self add: ttf to: read ]
		matches: [ :file | '*.ttf' match: file name ].
	aDirectoryEntry
		allFilesDo: [ :file | | ttc |
			ttc := TTFontReader readTTCFrom: file binaryContents.
			ttc do: [ :eachTtf |
				eachTtf folderName: file parent name.
				self add: eachTtf to: read ]]
		matches: [ :file | '*.ttc' match: file name ].
	Descriptions ifNil: [ Descriptions := Dictionary new ].
	read keysAndValuesDo: [ :familyName :emphasisDict |
		emphasisDict keysAndValuesDo: [ :emphasis :ttf |
			(Descriptions at: familyName ifAbsentPut: [ Dictionary new]) at: emphasis put: ttf ]].
	^read keys.! !

!TTFontDescription class methodsFor: 'class initialization' stamp: 'jmv 7/21/2025 16:17:15'!
initialize
	"
	TTFontDescription initialize.
	FontFamily releaseClassCachedState.
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'.
	"
	Descriptions := nil.! !


!VectorEngine methodsFor: 'private' stamp: 'jmv 7/21/2025 14:45:21'!
contourDataIndexForUtf32: codePoint contourDataIndexes: contourDataIndexes
	"contourDataIndexes is designed to be directly accessible via UTF-8 bytes of a CodePoint.
	This allows for a compact and efficient packing of TrueType contour data.
	In order to be used for a plain Code Point (as expressed in UTF-32), the UTF-8 bytes of
	the code point need to be extracted and used to access contourDataIndexes."

	| i utf8Byte1 utf8Byte2 utf8Byte3 utf8Byte4 |
	codePoint <= 16r7F
		ifTrue: [
			utf8Byte1 := codePoint.
			i := contourDataIndexes at: utf8Byte1 + 1 ]
		ifFalse: [
			codePoint <= 16r7FF
				ifTrue: [
					utf8Byte1 := (codePoint bitShift: -6) bitOr: 2r11000000.
					utf8Byte2 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte1 + 1.
					i < 0 ifFalse: [ ^0 ].
					i := contourDataIndexes at: i negated + utf8Byte2 + 1 ]
				ifFalse: [
					codePoint <= 16rFFFF
						ifTrue: [
							utf8Byte1 := (codePoint bitShift: -12) bitOr: 2r11100000.
							utf8Byte2 := ((codePoint bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte3 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
							i := contourDataIndexes at: utf8Byte1 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte2 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte3 + 1 ]
						ifFalse: [
							utf8Byte1 := (codePoint bitShift: -18) bitOr: 2r11110000.
							utf8Byte2 := ((codePoint bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte3 := ((codePoint bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte4 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
							i := contourDataIndexes at: utf8Byte1 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte2 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte3 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte4 + 1 ]]].
	^i! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 7/21/2025 16:24:51'!
displayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours |
	nextGlyphX := destX.
	startIndex to: stopIndex do: [ :charIndex | | latin1Code i |
		latin1Code := (aString at: charIndex) codePoint.
		i := contourDataIndexes at: latin1Code + 1.
		i < 1 ifTrue: [ i := 1 ].
		advanceWidth := contourData at: i. i := i + 1.
		boundsLeft := contourData at: i. i := i + 1.
		boundsRight := contourData at: i. i := i + 1.
		boundsBottom := contourData at: i. i := i + 1.
		boundsTop := contourData at: i. i := i + 1.
		numContours := (contourData at: i) asInteger. i := i + 1.
		self glyphContours: numContours atx: nextGlyphX y: destY scalex: sx y: sy contourData: contourData i: i.
		nextGlyphX := nextGlyphX + (advanceWidth * sx) ].
	^nextGlyphX! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 7/21/2025 16:25:00'!
displayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
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

!VectorEngine methodsFor: 'private' stamp: 'jmv 7/21/2025 16:25:08'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX byte1 byte2OrNil baseIndex advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
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

!VectorEngine methodsFor: 'private' stamp: 'jmv 7/21/2025 16:24:46'!
glyphContours: numContours atx: nextGlyphX y: nextGlyphY scalex: sx y: sy contourData: contourData i: contourDataIndex

		| i contourStart numBeziers ttMoveToX ttMoveToY x y |
		i := contourDataIndex.
		numContours timesRepeat: [
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := contourData at: i. i := i + 1.
			ttMoveToY := contourData at: i. i := i + 1.
			x := ttMoveToX * sx + nextGlyphX.
			y := ttMoveToY * sy + nextGlyphY.
			contourStart := x@y.
			self moveTo: contourStart.
			numBeziers timesRepeat: [ | cx cy ttControlX ttControlY ttEndX ttEndY |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				cx := ttControlX * sx + x.
				cy := ttControlY * sy + y.
				x := ttEndX * sx + x.
				y := ttEndY * sy + y.
				self quadraticBezierTo: x@y control: cx@cy ].
			self lineTo: contourStart ].! !

	TTFontDescription initialize.
	FontFamily releaseClassCachedState.
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'.
	PreferenceSet setDefaultFont: FontFamily defaultFamilyName.!
