'From Cuis 6.0 [latest update: #5593] on 14 December 2022 at 1:22:10 pm'!

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 12/14/2022 13:21:19'!
appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex	 bar: aProgressBarOrNil unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

		glyphsArray withIndexDo: [ :glyphOrArrayOrNumberNil :index |
			aProgressBarOrNil ifNotNil: [ :bar | bar value: index ].
			glyphOrArrayOrNumberNil ifNotNil: [
				glyphOrArrayOrNumberNil == theUnknownGlyph
					ifTrue: [ indexes at: baseIndex + index put: 1 ]
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
										glyph _ arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx _ AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 12/14/2022 13:21:35'!
glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph |
	
	mGlyph _ glyphsByUTF8 at: $M asciiValue + 1.
	letterMTopSideBearing _ (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	contourDataIndexesByUtf8 _ (IntegerArray new: 256) as: OrderedCollection.
	contourDataForUtf8 _ Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
		self fullName, ' - Processing glyph data' 
			displayProgressAt: 100@100
			from: 1 
			to: glyphsByUTF8 size 
			during: [ :bar |
				self appendGlyphDataTo: stream glyphsArray: glyphsByUTF8 indexes: contourDataIndexesByUtf8
					base: 0 bar: bar unknown: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs ]].
	contourDataIndexesByUtf8 _ contourDataIndexesByUtf8 as: IntegerArray.! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 12/14/2022 13:22:02'!
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
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturn.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: FallbackArrowGlyphs! !

!methodRemoval: TTFontDescription #appendGlyphDataTo:glyphsArray:indexes:base:bar:unknown: stamp: 'jmv 12/14/2022 13:21:44'!
TTFontDescription removeSelector: #appendGlyphDataTo:glyphsArray:indexes:base:bar:unknown:!
