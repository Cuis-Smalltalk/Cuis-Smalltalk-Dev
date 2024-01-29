'From Cuis6.3 [latest update: #6183] on 9 January 2024 at 11:19:13 am'!

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 10:39:52'!
isDiacriticalMark
	"
	(Character codePoint: 16r0041) isDiacriticalMark
	(Character codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i := self codePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 10:43:56'!
isSeparator
	"Answer whether the receiver is one of the separator characters"
	"Also see #separators"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		8203"UnicodeCodePoint zeroWidthSpace")
			statePointsTo: self codePoint! !

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 11:14:11'!
isValidInFilenames
	"Can be part of a filename?
		$< isValidInFilenames
		$a isValidInFilenames
		$| isValidInFilenames
		$^ isValidInFilenames
		$: isValidInFilenames
	"
self flag: #unicodeNeedsImprovement.
	self isNullCharacter ifTrue: [ ^false ].
	self isSeparator ifTrue: [ ^false ].
	^(#( $: $< $> $| $/ $\ $? $* $") statePointsTo: self) not.! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 10:44:23'!
separators
	"Answer the separator characters"
	"Also see #isSeparator"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		8203"UnicodeCodePoint zeroWidthSpace")
			collect: [ :v | Character codePoint: v ]! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/9/2024 10:53:18'!
zeroWidthSpace
	^self codePoint: 16r200B! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/9/2024 10:45:34'!
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
		s codePoint < 256 ifTrue: [
			glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ]].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph.​ upArrowGlyph.​ rightArrowGlyph.​ downArrowGlyph. carriageReturnGlyph}! !

