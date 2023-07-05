'From Cuis 6.0 [latest update: #5895] on 5 July 2023 at 12:35:57 pm'!
!classDefinition: #TTFontReader category: 'Graphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'glyphs nGlyphs kernPairs fontDescription '
	classVariableNames: 'FallbackArrowGlyphs SmalltalkReturnArrowGlyphCodePoint SmalltalkAssignmentArrowGlyphCodePoint '
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!TTFontReader class methodsFor: 'setup' stamp: 'jmv 7/5/2023 12:35:12'!
smalltalkAssignmentArrowGlyphCodePoint
	SmalltalkAssignmentArrowGlyphCodePoint isNil ifTrue: [
		SmalltalkAssignmentArrowGlyphCodePoint := 8592 ].
	^SmalltalkAssignmentArrowGlyphCodePoint! !

!TTFontReader class methodsFor: 'setup' stamp: 'jmv 7/5/2023 12:34:14'!
smalltalkAssignmentArrowGlyphCodePoint: aNumber

	SmalltalkAssignmentArrowGlyphCodePoint := aNumber! !

!TTFontReader class methodsFor: 'setup' stamp: 'jmv 7/5/2023 12:34:57'!
smalltalkReturnArrowGlyphCodePoint
	SmalltalkReturnArrowGlyphCodePoint isNil ifTrue: [
		SmalltalkReturnArrowGlyphCodePoint := 8593 ].
	^SmalltalkReturnArrowGlyphCodePoint! !

!TTFontReader class methodsFor: 'setup' stamp: 'jmv 7/5/2023 12:34:22'!
smalltalkReturnArrowGlyphCodePoint: aNumber

	SmalltalkReturnArrowGlyphCodePoint := aNumber! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 7/5/2023 12:35:39'!
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
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ UnicodeCodePoint zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOfUnicodeCodePoint: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph.​ upArrowGlyph.​ rightArrowGlyph.​ downArrowGlyph. carriageReturnGlyph}! !

!classDefinition: #TTFontReader category: 'Graphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'glyphs nGlyphs kernPairs fontDescription'
	classVariableNames: 'FallbackArrowGlyphs SmalltalkAssignmentArrowGlyphCodePoint SmalltalkReturnArrowGlyphCodePoint'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!TTFontReader class reorganize!
('instance creation' readTTCFrom: readTTFFrom:)
('setup' smalltalkAssignmentArrowGlyphCodePoint smalltalkAssignmentArrowGlyphCodePoint: smalltalkReturnArrowGlyphCodePoint smalltalkReturnArrowGlyphCodePoint:)
!

