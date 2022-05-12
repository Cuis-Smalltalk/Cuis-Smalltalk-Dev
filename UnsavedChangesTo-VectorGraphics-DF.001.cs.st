'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 5:23:23 pm'!

!TTFontReader methodsFor: 'processing' stamp: 'DF 5/12/2022 17:20:43'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByIso5559s15 glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes carriageReturn |
	unknownGlyph _ glyphs first.
	nullGlyph _ glyphs second.
	mapSize _ characterMap size.
	glyphsByIso5559s15 _ Array new: 256.
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
		codePoint = $- codePoint ifTrue: [ preferredCodePoint _ 16r2212 ]. "WIDE MINUS"
		codePoint = $* codePoint ifTrue: [ preferredCodePoint _ 16r2217 ]. "CENTERED ASTERISK"
		g _ glyphs at: (characterMap at: preferredCodePoint+1 ifAbsent:[glyphs size +1])+1 ifAbsent: [ unknownGlyph ].	
		g == unknownGlyph ifTrue: [
			g _ glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		(Character iso8859s15CodeForUnicodeCodePoint: codePoint) ifNotNil: [ :iso8859s15 |
			g == unknownGlyph
				ifFalse: [
					glyphsByIso5559s15 at: iso8859s15 + 1 put: g ]
				ifTrue: [				"Tell that we need an alternative arrow glyph"
					(FallbackArrowGlyphs notNil and: [ codePoint between: 8592 and: 8595 ]) ifTrue: [
						glyphsByIso5559s15 at: iso8859s15 + 1 put: codePoint - 8591 ]]].

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
		glyphsByIso5559s15 at: s codePoint +1 put: spaceGlyph.
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph.
	].
	glyphsByIso5559s15 at: 13 +1 put: carriageReturn.
	glyphsByUTF8 at: 13 +1 put: carriageReturn.
	fontDescription
		glyphsByIso8859s15: glyphsByIso5559s15 glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: FallbackArrowGlyphs! !

!TTFontReader methodsFor: 'private' stamp: 'DF 5/12/2022 17:18:37'!
decodeCmapFmtTableFormat4: entry
	"segment mapping to deltavalues"

	| length entryCount segCount segments offset cmap |
	length := entry nextUShort.
	entry skip: 2. "skip version"
	segCount := entry nextUShort // 2.
	entry skip: 6. "skip searchRange, entrySelector, rangeShift"
	segments := Array new: segCount.
	segments := (1 to: segCount) collect: [ :e | Array new: 4 ].
	1 to: segCount do: [ :i | (segments at: i) at: 2 put: entry nextUShort ]. "endCount"
	entry skip: 2. "skip reservedPad"
	1 to: segCount do: [ :i | (segments at: i) at: 1 put: entry nextUShort]. "startCount"
	1 to: segCount do: [ :i | (segments at: i) at: 3 put: entry nextShort ]. "idDelta"
	offset := entry offset.
	1 to: segCount do: [ :i | (segments at: i) at: 4 put: entry nextUShort ]. "idRangeOffset"
	
	"do not process the end-of-search segment"
	segments _ segments reject:[:each | (each at:1) = 0xFFFF and:[(each at:2) = 0xFFFF] ].
	
	entryCount := segments inject: 0 into: [ :max :seg | max max: seg second ].
	cmap := WordArray new: entryCount+1.
	segments withIndexDo: [ :seg :si | | code |
		seg first to: seg second do: [ :i |
			seg last > 0 ifTrue: ["offset to glypthIdArray - this is really C-magic!!"
				entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 
				code := entry nextUShort.
				code > 0 ifTrue: [code := code + seg third ]]
			ifFalse: ["simple offset"
				code := i + seg third ].
			cmap at: i + 1 put: code ]].
	^ cmap! !

