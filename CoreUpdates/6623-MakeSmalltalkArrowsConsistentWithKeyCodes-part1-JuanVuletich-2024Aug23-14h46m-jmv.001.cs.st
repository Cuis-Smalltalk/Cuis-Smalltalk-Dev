'From Cuis7.1 [latest update: #6622] on 23 August 2024 at 3:06:46 pm'!

!TTFontReader methodsFor: 'processing' stamp: 'jmv 8/23/2024 14:57:38'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes leftArrowGlyph upArrowGlyph rightArrowGlyph downArrowGlyph carriageReturnGlyph leftArrowGlyphCodePoint upArrowGlyphCodePoint rightArrowGlyphCodePoint downArrowGlyphCodePoint carriageReturnGlyphCodePoint |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.
	leftArrowGlyphCodePoint := self class smalltalkAssignmentArrowGlyphCodePoint.
	upArrowGlyphCodePoint := self class smalltalkReturnArrowGlyphCodePoint.
	rightArrowGlyphCodePoint := 8594.
	downArrowGlyphCodePoint := 8595.
	carriageReturnGlyphCodePoint := 8617.
	
	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := {leftArrowGlyphCodePoint. upArrowGlyphCodePoint. rightArrowGlyphCodePoint. downArrowGlyphCodePoint. carriageReturnGlyphCodePoint}
					collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	"Take arrow glyphs if present, or use fallbacks if not."
	leftArrowGlyph := upArrowGlyph := rightArrowGlyph := downArrowGlyph := carriageReturnGlyph := unknownGlyph.
	mapSize > downArrowGlyphCodePoint ifTrue: [
		leftArrowGlyph := glyphs at: (characterMap at: leftArrowGlyphCodePoint+1)+1.
		upArrowGlyph := glyphs at: (characterMap at: upArrowGlyphCodePoint+1)+1.
		rightArrowGlyph := glyphs at: (characterMap at: rightArrowGlyphCodePoint+1)+1.
		downArrowGlyph := glyphs at: (characterMap at: downArrowGlyphCodePoint+1)+1 ].
	leftArrowGlyph == unknownGlyph ifTrue: [ leftArrowGlyph := FallbackArrowGlyphs at: 1 ].
	upArrowGlyph == unknownGlyph ifTrue: [ upArrowGlyph := FallbackArrowGlyphs at: 2 ].
	rightArrowGlyph == unknownGlyph ifTrue: [ rightArrowGlyph := FallbackArrowGlyphs at: 3 ].
	downArrowGlyph == unknownGlyph ifTrue: [ downArrowGlyph := FallbackArrowGlyphs at: 4 ].
	mapSize > carriageReturnGlyphCodePoint ifTrue: [
		carriageReturnGlyph := glyphs at: (characterMap at: carriageReturnGlyphCodePoint+1)+1 ].
	carriageReturnGlyph == unknownGlyph ifTrue: [ carriageReturnGlyph := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOf: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ Character zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOf: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		s codePoint < 256 ifTrue: [
			glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ]].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph. rightArrowGlyph. upArrowGlyph. downArrowGlyph. carriageReturnGlyph}! !


!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 8/23/2024 15:35:18'!
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
										glyph := arrowGlyphs at: glyphOrArrayOrNumberNil.
										tx := AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing * 0.7.
										indexes at: baseIndex + index put: stream position+1.
										glyph addGlyphDataTo: stream tx: tx
										]
									ifFalse: [
										indexes at: baseIndex + index put: stream position+1.
										glyphOrArrayOrNumberNil addGlyphDataTo: stream tx: nil ]]]]].! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/23/2024 14:47:14'!
smalltalkRightArrow
	"
	The use of code 29 for a right arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkRightArrow
	"
	^ `Character codePoint: 29`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/23/2024 14:47:36'!
smalltalkUpArrow
	"
	The use of code 30 for an up arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.

	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkUpArrow
	"
	^ `Character codePoint: 30`! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'!

