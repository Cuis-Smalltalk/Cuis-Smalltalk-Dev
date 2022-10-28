'From Cuis 6.0 [latest update: #5527] on 26 October 2022 at 3:14:07 pm'!
!classDefinition: #TTFontDescription category: 'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForIso8859s15 contourDataIndexesByIso8859s15 contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing '
	classVariableNames: 'Descriptions '
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 10/26/2022 15:02:28'!
glyphsByUtf8Bytes: arrayUtf8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph _ arrayUtf8 at: $M asciiValue + 1.
	letterMTopSideBearing _ (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	contourDataIndexesByUtf8 _ (IntegerArray new: 256) as: OrderedCollection.
	contourDataForUtf8 _ Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
		self fullName, ' - Processing glyph data' 
			displayProgressAt: 100@100
			from: 1 
			to: arrayUtf8 size 
			during: [ :bar |
				self appendGlyphDataTo: stream glyphsArray: arrayUtf8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph ]].
	contourDataIndexesByUtf8 _ contourDataIndexesByUtf8 as: IntegerArray.! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/26/2022 15:04:50'!
prefFont
	^ #( 
		#(aaFontsColormapDepth 4)
		#(cacheTrueTypeGlyphs true )
		#(guiElementsSize standardFonts #(tinyFonts verySmallFonts smallFonts standardFonts largeFonts veryLargeFonts hugeFonts) defaultFontSize: )
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/26/2022 14:17:34'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s asUtf8String displayOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 10/26/2022 14:47:58'!
scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	^sourceString asUtf8String scanCharactersFrom: startIndex to: stopIndex with: self rightX: rightX stopConditions: stops! !


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 10/26/2022 14:18:39'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString asUtf8String displayOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/26/2022 14:17:55'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
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
			p1 _ currentTransformation transform: aPoint roundedHAFZ.
			p1 _ p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 _ p1 roundedHAFZ.
			answer _ aString asUtf8String displayOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/26/2022 14:18:18'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint roundedHAFZ.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 roundedHAFZ.
	engine colorMap: nil.
	^aString asUtf8String displayOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor! !


!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 10/26/2022 14:44:16'!
widthOf: aCharacterOrCodePoint

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil ]
		withUtf8BytesOfUnicodeCodePoint: aCharacterOrCodePoint codePoint.! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 10/26/2022 15:09:32'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes carriageReturn |
	unknownGlyph _ glyphs first.
	nullGlyph _ glyphs second.
	mapSize _ characterMap size.
	glyphsByUTF8 _ Array new: 256.

	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes _ #(8592 8593 8594 8595 8617) collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs _ arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	mapSize > 8617
		ifTrue: [
			carriageReturn _ glyphs at: (characterMap at: 8617+1)+1.
			carriageReturn == unknownGlyph ifTrue: [ carriageReturn _ FallbackArrowGlyphs at: 5 ]]
		ifFalse: [ carriageReturn _ FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint _ codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint _ 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint _ 16r2217 ]. "CENTERED ASTERISK"
		g _ glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g _ glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array _ glyphsByUTF8.
		prevByte _ nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array _ (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte _ byte.
				]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph _ g ].

		].

	Character separators do: [ :s |
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: (FallbackArrowGlyphs at: i) ].
	glyphsByUTF8 at: 13 +1 put: carriageReturn.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: FallbackArrowGlyphs! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 10/26/2022 15:06:26'!
processCharacterMappingTable: entry
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| initialOffset nSubTables platformID platformSpecificEncodingID offset offsets platformIDsAndEncodings orderOfPreference found |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUShort.
	platformIDsAndEncodings _ Array new: nSubTables.
	offsets _ Array new: nSubTables.
	1 to: nSubTables do: [ :i |
		platformID := entry nextUShort.
		platformSpecificEncodingID := entry nextUShort.
		offset := entry nextULong.
		platformIDsAndEncodings at: i put: {platformID. platformSpecificEncodingID}.
		offsets  at: i put: offset  ].
	"see, for instance, https://docs.microsoft.com/en-us/typography/opentype/spec/cmap"
	orderOfPreference _ 
		#(
			#(0 4)  			"platform=Unicode, encoding=4 (Unicode, full range)"
			#(0 10) 			"platform=Unicode, encoding=10 (Undocumented but widely used. Appears equivalent to #(0 4) or #(3 10))"
			#(3 10)  			"platform=Windows, encoding=10(Unicode, full range), format=12(Segment coverage)"
			#(0 3)  			"platform=Unicode, encoding=3 (Unicode, BMP only)"
			#(3 1)  			"platform=Windows, encoding=1 (Unicode, BMP only)"
			#(0 0)  			"platform=Unicode, encoding=3(Unicode 1.0, deprecated)"
		).
	orderOfPreference do: [ :preferredPlatformAndEncoding |
		found _ platformIDsAndEncodings indexOf: preferredPlatformAndEncoding.
		found = 0 ifFalse: [
			"Go to the beginning of the table"
			offset _ offsets at: found.
			entry offset: initialOffset + offset.
			^ self decodeCmapFmtTable: entry ]].

	^nil! !

!methodRemoval: TTFontDescription #formGlyphFor:pixelSize: stamp: 'jmv 10/26/2022 14:35:49'!
TTFontDescription removeSelector: #formGlyphFor:pixelSize:!
!methodRemoval: TTFontDescription #widthOfCharacter: stamp: 'jmv 10/26/2022 14:44:53'!
TTFontDescription removeSelector: #widthOfCharacter:!
!methodRemoval: TTFontDescription #contourDataForIso8859s15 stamp: 'jmv 10/26/2022 14:54:44'!
TTFontDescription removeSelector: #contourDataForIso8859s15!
!methodRemoval: TTFontDescription #widthOfCodePoint: stamp: 'jmv 10/26/2022 14:51:00'!
TTFontDescription removeSelector: #widthOfCodePoint:!
!methodRemoval: TTFontDescription #glyphsByIso8859s15:glyphsByUtf8Bytes:unknownGlyph:scaleArrowGlyphs: stamp: 'jmv 10/26/2022 15:03:32'!
TTFontDescription removeSelector: #glyphsByIso8859s15:glyphsByUtf8Bytes:unknownGlyph:scaleArrowGlyphs:!
!methodRemoval: TTFontDescription #contourDataIndexesByIso8859s15 stamp: 'jmv 10/26/2022 14:54:42'!
TTFontDescription removeSelector: #contourDataIndexesByIso8859s15!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 10/26/2022 14:37:33'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 10/26/2022 14:37:19'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 10/26/2022 14:37:33'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 10/26/2022 14:37:17'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPlugin #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 10/26/2022 14:30:42'!
VectorEngineWithPlugin removeSelector: #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngineWithPlugin #displayString:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 10/26/2022 14:37:07'!
VectorEngineWithPlugin removeSelector: #displayString:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngine #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 10/26/2022 14:30:41'!
VectorEngine removeSelector: #displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: VectorEngine #displayString:from:to:at:trueTypeFont:color:scale:preventMirroring: stamp: 'jmv 10/26/2022 14:21:56'!
VectorEngine removeSelector: #displayString:from:to:at:trueTypeFont:color:scale:preventMirroring:!
!methodRemoval: CharacterScanner #scanStringCharsFrom:to:in:rightX:stopConditions: stamp: 'jmv 10/26/2022 14:49:26'!
CharacterScanner removeSelector: #scanStringCharsFrom:to:in:rightX:stopConditions:!
!methodRemoval: CharacterScanner #scanUtf32StringFrom:to:in:rightX:stopConditions: stamp: 'jmv 10/26/2022 14:49:37'!
CharacterScanner removeSelector: #scanUtf32StringFrom:to:in:rightX:stopConditions:!
!methodRemoval: TrueTypeFont #widthOfCodePoint: stamp: 'jmv 10/26/2022 14:50:54'!
TrueTypeFont removeSelector: #widthOfCodePoint:!
!methodRemoval: TrueTypeFont #glyphAt: stamp: 'jmv 10/26/2022 14:35:10'!
TrueTypeFont removeSelector: #glyphAt:!
!methodRemoval: BitBltCanvasEngine #displayString:from:to:at:trueTypeFont:color: stamp: 'jmv 10/26/2022 14:27:47'!
BitBltCanvasEngine removeSelector: #displayString:from:to:at:trueTypeFont:color:!
!methodRemoval: BitBltCanvasEngine #basicDisplayString:from:to:at:trueTypeFont: stamp: 'jmv 10/26/2022 14:34:19'!
BitBltCanvasEngine removeSelector: #basicDisplayString:from:to:at:trueTypeFont:!
!methodRemoval: BitBltCanvasEngine #displayUtf32:from:to:at:trueTypeFont:color: stamp: 'jmv 10/26/2022 14:30:56'!
BitBltCanvasEngine removeSelector: #displayUtf32:from:to:at:trueTypeFont:color:!
!methodRemoval: BitBltCanvasEngine #basicDisplayUtf32:from:to:at:trueTypeFont: stamp: 'jmv 10/26/2022 14:33:38'!
BitBltCanvasEngine removeSelector: #basicDisplayUtf32:from:to:at:trueTypeFont:!
!methodRemoval: Utf32String #scanCharactersFrom:to:with:rightX:stopConditions: stamp: 'jmv 10/26/2022 14:49:07'!
Utf32String removeSelector: #scanCharactersFrom:to:with:rightX:stopConditions:!
!methodRemoval: Utf32String #displayOnVectorEngine:from:to:at:font:color: stamp: 'jmv 10/26/2022 14:28:50'!
Utf32String removeSelector: #displayOnVectorEngine:from:to:at:font:color:!
!methodRemoval: Utf32String #displayOnBitBltCanvasEngine:from:to:at:font:color: stamp: 'jmv 10/26/2022 14:28:54'!
Utf32String removeSelector: #displayOnBitBltCanvasEngine:from:to:at:font:color:!
!methodRemoval: String #scanCharactersFrom:to:with:rightX:stopConditions: stamp: 'jmv 10/26/2022 14:49:06'!
String removeSelector: #scanCharactersFrom:to:with:rightX:stopConditions:!
!methodRemoval: String #displayOnVectorEngine:from:to:at:font:color: stamp: 'jmv 10/26/2022 14:28:36'!
String removeSelector: #displayOnVectorEngine:from:to:at:font:color:!
!methodRemoval: String #displayOnBitBltCanvasEngine:from:to:at:font:color: stamp: 'jmv 10/26/2022 14:28:39'!
String removeSelector: #displayOnBitBltCanvasEngine:from:to:at:font:color:!
!methodRemoval: UnicodeCodePoint #widthIn: stamp: 'jmv 10/26/2022 14:44:42'!
UnicodeCodePoint removeSelector: #widthIn:!
!methodRemoval: Character class #latin1ToMacRoman: stamp: 'jmv 10/26/2022 14:38:42'!
Character class removeSelector: #latin1ToMacRoman:!
!methodRemoval: Character #widthIn: stamp: 'jmv 10/26/2022 14:44:42'!
Character removeSelector: #widthIn:!

!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('services' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty)
('paragraph support' encompassLine: encompassParagraph:)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash sameAs:)
('converting' asUtf32String asUtf32Words asUtf8Bytes)
('testing' isAscii isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('initialization' initialize)
!

!classDefinition: #TTFontDescription category: 'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
