"
TTFontReader builds a TTFontDescription from a TrueType font file (.ttf).
"
Class {
	#name : 'TTFontReader',
	#superclass : 'Object',
	#instVars : [
		'glyphs',
		'nGlyphs',
		'kernPairs',
		'fontDescription'
	],
	#classVars : [
		'FallbackArrowGlyphs',
		'SmalltalkAssignmentArrowGlyphCodePoint',
		'SmalltalkReturnArrowGlyphCodePoint'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'instance creation' }
TTFontReader class >> readTTCFrom: aByteArray [

	^self new readTTCFrom: aByteArray
]

{ #category : 'instance creation' }
TTFontReader class >> readTTFFrom: aByteArray [

	^self new readTTFFrom: aByteArray
]

{ #category : 'setup' }
TTFontReader class >> smalltalkAssignmentArrowGlyphCodePoint [
	SmalltalkAssignmentArrowGlyphCodePoint isNil ifTrue: [
		SmalltalkAssignmentArrowGlyphCodePoint := 8592 ].
	^SmalltalkAssignmentArrowGlyphCodePoint
]

{ #category : 'setup' }
TTFontReader class >> smalltalkAssignmentArrowGlyphCodePoint: aNumber [

	SmalltalkAssignmentArrowGlyphCodePoint := aNumber
]

{ #category : 'setup' }
TTFontReader class >> smalltalkReturnArrowGlyphCodePoint [
	SmalltalkReturnArrowGlyphCodePoint isNil ifTrue: [
		SmalltalkReturnArrowGlyphCodePoint := 8593 ].
	^SmalltalkReturnArrowGlyphCodePoint
]

{ #category : 'setup' }
TTFontReader class >> smalltalkReturnArrowGlyphCodePoint: aNumber [

	SmalltalkReturnArrowGlyphCodePoint := aNumber
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTable: entry [

	| cmapFmt |
	cmapFmt := entry nextUint16.
	cmapFmt = 0 ifTrue: [ "byte encoded table"
		^self decodeCmapFmtTableFormat0: entry ].
	cmapFmt = 4 ifTrue: [ "segment mapping to deltavalues"
		^ self decodeCmapFmtTableFormat4: entry ].
	cmapFmt = 6 ifTrue: [ "trimmed table"
		^self decodeCmapFmtTableFormat6: entry ].
	cmapFmt = 10 ifTrue: [ "trimmed array"
		^self decodeCmapFmtTableFormat10: entry ].
	cmapFmt = 12 ifTrue: [ "segmented coverage"
		^ self decodeCmapFmtTableFormat12: entry ].
	^ nil
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTableFormat0: entry [
	"byte encoded table"

	| length cmap |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	length := length - 6. 		"should be always 256"
	length <= 0 ifTrue: [ ^ nil ].	"but sometimes, this table is empty"
	cmap := WordArray new: length.
	entry nextBytes: length into: cmap startingAt: entry offset.
	^ cmap
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTableFormat10: entry [
	"trimmed array"

	| length entryCount cmap firstCode |
	entry skip: 2. "skip reserved"
	length := entry nextUint32.
	entry skip: 4. "skip language"
	firstCode := entry nextUint32.
	entryCount := entry nextUint32.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUint16 ].
	^ cmap
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTableFormat12: entry [
	"segmented coverage"

	| length cmap numGroups |
	entry skip: 2. "reserved"
	length := entry nextUint32.
	entry skip: 4. "language"
	numGroups := entry nextUint32.
	cmap := WordArray new.
	numGroups timesRepeat: [ | endCharCode prev startCharCode startGlyphId |
		startCharCode := entry nextUint32.
		endCharCode := entry nextUint32.
		startGlyphId := entry nextUint32.
		"
{startCharCode. endCharCode. startGlyphId} print.
"
"de ahi para arriba es banana
http://www.alanwood.net/unicode/unicode_samples.html
"
endCharCode < 16rE0000 ifTrue: [
		prev := cmap.
		cmap := WordArray new: endCharCode+1.
		cmap replaceFrom: 1 to: prev size with: prev. 
		startCharCode+1 to: endCharCode+1 do: [ :i |
			cmap at: i put: startGlyphId.
			startGlyphId := startGlyphId + 1 ]]
].
"
self halt.
"
	^ cmap
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTableFormat4: entry [
	"segment mapping to deltavalues"

	| length entryCount segCount segments offset cmap |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	segCount := entry nextUint16 // 2.
	entry skip: 6. "skip searchRange, entrySelector, rangeShift"
	segments := Array new: segCount.
	segments := (1 to: segCount) collect: [ :e | Array new: 4 ].
	1 to: segCount do: [ :i | (segments at: i) at: 2 put: entry nextUint16 ]. "endCount"
	entry skip: 2. "skip reservedPad"
	1 to: segCount do: [ :i | (segments at: i) at: 1 put: entry nextUint16]. "startCount"
	1 to: segCount do: [ :i | (segments at: i) at: 3 put: entry nextInt16 ]. "idDelta"
	offset := entry offset.
	1 to: segCount do: [ :i | (segments at: i) at: 4 put: entry nextUint16 ]. "idRangeOffset"
	"do not process the end-of-search segment"
	segments := segments reject:[:each | (each at:1) = 0xFFFF and:[(each at:2) = 0xFFFF] ].
	entryCount := segments inject: 0 into: [ :max :seg | max max: seg second ].
	cmap := WordArray new: entryCount+1.
	segments withIndexDo: [ :seg :si | | code |
		seg first to: seg second do: [ :i |
			seg last > 0 ifTrue: ["offset to glypthIdArray - this is really C-magic!"
				entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 
				code := entry nextUint16.
				code > 0 ifTrue: [code := code + seg third ]]
			ifFalse: ["simple offset"
				code := i + seg third ].
			cmap at: i + 1 put: code ]].
	^ cmap
]

{ #category : 'private' }
TTFontReader >> decodeCmapFmtTableFormat6: entry [
	"trimmed table"

	| length entryCount cmap firstCode |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	firstCode := entry nextUint16.
	entryCount := entry nextUint16.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUint16 ].
	^ cmap
]

{ #category : 'private' }
TTFontReader >> getGlyphFlagsFrom: entry size: nPts [
	"Read in the flags for this glyph. The outer loop gathers the flags that
	are actually contained in the table. If the repeat bit is set in a flag
	then the next byte is read from the table; this is the number of times
	to repeat the last flag. The inner loop does this, incrementing the
	outer loops index each time."
	| flags index repCount flagBits |
	flags := ByteArray new: nPts.
	index := 1.
	[index <= nPts] whileTrue:[
		flagBits := entry nextByte.
		flags at: index put: flagBits.
		(flagBits allMask: 8) ifTrue:[
			repCount := entry nextByte.
			repCount timesRepeat:[
				index := index + 1.
				flags at: index put: flagBits]].
		index := index + 1].
	^flags
]

{ #category : 'processing' }
TTFontReader >> getTableDirEntry: tagString from: fontData offset: offset [
	"Find the table named tagString in fontData and return a table directory entry for it."

	| numTables pos currentTag tag |
	numTables := fontData int16At: 5 + offset bigEndian: true.
	tag := ByteArray new: 4.
	1 to: 4 do: [ :i | tag at: i put: (tagString at: i) asciiValue ].
	tag := tag int32At: 1 bigEndian: true.
	pos := 13 + offset.
	1 to: numTables do:[:i|
		currentTag := fontData int32At: pos bigEndian: true.
		currentTag = tag ifTrue: [^TTFontTableDirEntry on: fontData at: pos].
		pos := pos+16].
	^nil
]

{ #category : 'processing' }
TTFontReader >> mapAndStoreGlyphsUsing: characterMap [
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
		(codePoint == $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint == $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
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
	1 to: 4 do: [ :i | glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph. rightArrowGlyph. upArrowGlyph. downArrowGlyph. carriageReturnGlyph}
]

{ #category : 'processing' }
TTFontReader >> processCharacterMappingTable: entry [
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| initialOffset nSubTables platformID platformSpecificEncodingID offset offsets platformIDsAndEncodings orderOfPreference found |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUint16.
	platformIDsAndEncodings := Array new: nSubTables.
	offsets := Array new: nSubTables.
	1 to: nSubTables do: [ :i |
		platformID := entry nextUint16.
		platformSpecificEncodingID := entry nextUint16.
		offset := entry nextUint32.
		platformIDsAndEncodings at: i put: {platformID. platformSpecificEncodingID}.
		offsets  at: i put: offset  ].
	"see, for instance, https://docs.microsoft.com/en-us/typography/opentype/spec/cmap"
	orderOfPreference := 
		#(
			#(0 4)  			"platform=Unicode, encoding=4 (Unicode, full range)"
			#(0 10) 			"platform=Unicode, encoding=10 (Undocumented but widely used. Appears equivalent to #(0 4) or #(3 10))"
			#(3 10)  			"platform=Windows, encoding=10(Unicode, full range), format=12(Segment coverage)"
			#(0 3)  			"platform=Unicode, encoding=3 (Unicode, BMP only)"
			#(3 1)  			"platform=Windows, encoding=1 (Unicode, BMP only)"
			#(0 0)  			"platform=Unicode, encoding=3(Unicode 1.0, deprecated)"
		).
	orderOfPreference do: [ :preferredPlatformAndEncoding |
		found := platformIDsAndEncodings indexOf: preferredPlatformAndEncoding.
		found = 0 ifFalse: [
			"Go to the beginning of the table"
			offset := offsets at: found.
			entry offset: initialOffset + offset.
			^ self decodeCmapFmtTable: entry ]].

	^nil
]

{ #category : 'processing' }
TTFontReader >> processCompositeGlyphFrom: entry [
	"Read a composite glyph from the font data."
	| glyph flags glyphIndex hasInstructions ofsX ofsY iLen geometryTransformation xMin yMin xMax yMax |
	glyph := TTCompositeGlyph new.
	"See, for instance, https://learn.microsoft.com/en-us/typography/opentype/otspec190/glyf"
	xMin := entry nextInt16.
	yMin := entry nextInt16.
	xMax := entry nextInt16.
	yMax := entry nextInt16.
	glyph boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin.
	hasInstructions := false.
	[
		flags := entry nextUint16.
		glyphIndex := entry nextUint16 + 1.

		(flags allMask: 1 "ARG_1_AND_2_ARE_WORDS") ifTrue: [
			"'Hay Offset short x@y' print."
			ofsX := entry nextInt16.
			ofsY := entry nextInt16.
		] ifFalse: [
			"'Hay Offset byte x@y' print."
			(ofsX := entry nextByte) > 127 ifTrue: [ofsX := ofsX - 256].
			(ofsY := entry nextByte) > 127 ifTrue: [ofsY := ofsY - 256]].

		(flags allMask: 2 "ARGS_ARE_XY_VALUES") ifFalse: [self error: 'Unsupported TrueType format']. "Unsupported"
		"ROUND_XY_TO_GRID = 4. Ignored"
		(flags anyMask: 8+64+128) ifFalse: [
			geometryTransformation := MorphicTranslation withTranslation: ofsX@ofsY ].
		(flags allMask: 8 "WE_HAVE_A_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation withScale: entry nextInt16 asFloat / 16r4000 ].
		(flags allMask: 64 "WE_HAVE_AN_X_AND_Y_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation
				withPointScale: (entry nextInt16 asFloat / 16r4000)@(entry nextInt16 asFloat / 16r4000)
				position: ofsX@ofsY ].
		(flags allMask: 128 "WE_HAVE_A_TWO_BY_TWO") ifTrue: [
			"2x2 transformation"
			geometryTransformation := AffineTransformation new
				a11: entry nextInt16 asFloat / 16r4000;
				a21: entry nextInt16 asFloat / 16r4000;
				a12: entry nextInt16 asFloat / 16r4000;
				a22: entry nextInt16 asFloat / 16r4000;
				a13: ofsX;
				a23: ofsY ].

		glyph addGlyph: glyphIndex transformation: geometryTransformation.
		hasInstructions := hasInstructions or: [ flags allMask: 256 "WE_HAVE_INSTRUCTIONS"].
		"Continue as long as the MORE_COMPONENTS bit is set"
		flags allMask: 32 "MORE_COMPONENTS"
	] whileTrue.
	hasInstructions ifTrue: [
		"Ignore them"
		iLen := entry nextUint16.
		entry skip: iLen].
	^glyph
]

{ #category : 'processing' }
TTFontReader >> processFontHeaderTable: entry [
"Value				Data Type    Description
unitsPerEm			USHORT      Granularity of the font's em square.
xMax				USHORT      Maximum X-coordinate for the entire font.
xMin				USHORT      Minimum X-coordinate for the entire font.
yMax				USHORT      Maximum Y-coordinate for the entire font.
yMin				USHORT      Minimum Y-coordinate for the entire font.
indexToLocFormat	SHORT       Used when processing the Index To Loc Table."
	| origin corner units indexToLocFormat |
	entry skip: 4. "Skip table version number"
	entry skip: 4. "Skip font revision number"
	entry skip: 4. "Skip check sum adjustment"
	entry skip: 4. "Skip magic number"
	entry skip: 2. "Skip flags"

	units := entry nextUint16.

	entry skip: 8. "Skip creation date"
	entry skip: 8. "Skip modification date"

	"Get min/max values of all glyphs"
	origin := entry nextInt16 @ entry nextInt16.
	corner := entry nextInt16 @ entry nextInt16.

	entry skip: 2. "Skip mac style"
	entry skip: 2. "Skip lowest rec PPEM"
	entry skip: 2. "Skip font direction hint"
	indexToLocFormat := entry nextInt16.

	fontDescription setBounds: (origin corner: corner) unitsPerEm: units.
	^indexToLocFormat
]

{ #category : 'processing' }
TTFontReader >> processGlyphDataTable: entry offsets: offsetArray [
	"Read the actual glyph data from the font.
	offsetArray contains the start offsets in the data for each glyph."
	| initialOffset |
	initialOffset := entry offset.
	glyphs := Array new: nGlyphs.
	fontDescription fullName, ' - Reading glyph data' 
		displayProgressAt: 100@100
		from: 1 
		to: nGlyphs 
		during: [ :bar |
			| glyph glyphOffset nextOffset glyphLength numberOfContours |
			1 to: nGlyphs do: [ :glyphIndex |
				bar value: glyphIndex.
				glyphOffset := offsetArray at: glyphIndex.
				nextOffset := offsetArray at: glyphIndex+1.
				glyphLength := nextOffset - glyphOffset.
				glyph := glyphLength = 0
					ifTrue: [ TTGlyph new. ]
					ifFalse: [
						entry offset: initialOffset + glyphOffset.
						numberOfContours := entry nextInt16.
						numberOfContours >= 0
							ifTrue: [ self processSimpleGlyphContours: numberOfContours from: entry ]
							ifFalse: [ self processCompositeGlyphFrom: entry ]].
				glyphs at: glyphIndex put: glyph ]].
	glyphs do: [ :g | g replaceIndexesByComponentGlyphsIn: glyphs ]
	
]

{ #category : 'processing' }
TTFontReader >> processHorizontalHeaderTable: entry [
"See 
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://docs.microsoft.com/en-us/typography/opentype/spec/hhea
https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html
http://chanae.walon.org/pub/ttf/ttf_glyphs.htm

https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6.html
"
"
ascent  						SHORT          Distance from baseline of highest ascender.
descent  					SHORT          Distance from baseline of lowest descender.
lineGap  					SHORT          Typographic line gap.
numOfLongHorMetrics  	USHORT       Number of advance widths in metrics table (HTMX Table); may be smaller than the total number of glyphs.
"
	| ascent descent lineGap numOfLongHorMetrics |
	entry skip: 4. "Skip table version"
	ascent := entry nextInt16.
	descent := entry nextInt16.
	lineGap := entry nextInt16.
	entry skip: 2. "Skip advanceWidthMax"
	entry skip: 2. "Skip minLeftSideBearing"
	entry skip: 2. "Skip minRightSideBearing"
	entry skip: 2. "Skip xMaxExtent"
	entry skip: 2. "Skip caretSlopeRise"
	entry skip: 2. "Skip caretSlopeRun"
	entry skip: 10. "Skip 5 reserved shorts"
	entry skip: 2. "Skip metricDataFormat"

	numOfLongHorMetrics := entry nextUint16.
	fontDescription setAscent: ascent descent: descent lineGap: lineGap.
	^numOfLongHorMetrics
]

{ #category : 'processing' }
TTFontReader >> processHorizontalMetricsTable: entry length: numHMetrics [
	| lastAW glyph |
	1
		to: numHMetrics
		do: [ :i |
			glyph := glyphs at: i.
			glyph advanceWidth: entry nextUint16.
			glyph leftSideBearing: entry nextInt16.
			glyph updateRightSideBearing ].
	numHMetrics = nGlyphs ifTrue: [ ^ true ].
	lastAW := (glyphs at: numHMetrics) advanceWidth.
	numHMetrics + 1
		to: nGlyphs
		do: [ :i |
			glyph := glyphs at: i.
			glyph advanceWidth: lastAW.
			glyph leftSideBearing: entry nextInt16.
			glyph updateRightSideBearing ].
]

{ #category : 'processing' }
TTFontReader >> processIndexToLocationTable: entry format: indexToLocFormat [
"glyphOffset    ULONG[numGlyphs]   An array that contains each glyph's
                                 offset into the Glyph Data Table.
"	| glyphOffset offset|
	glyphOffset := Array new: nGlyphs+1.
	1 to: nGlyphs+1 do:[:i|
		(indexToLocFormat = 0) ifTrue:[ "Format0: offset/2 is stored"
			offset := entry nextUint16 * 2.
		] ifFalse:["Format1: store actual offset"
			offset := entry nextUint32].
		glyphOffset at: i put: offset].
	^glyphOffset
]

{ #category : 'processing' }
TTFontReader >> processKerningSubTableType0: entry [

	| kp nKernPairs |
	nKernPairs := entry nextUint16.
	entry skip: 2. "Skip search range"
	entry skip: 2. "Skip entry selector"
	entry skip: 2. "Skip range shift"
	kernPairs := Array new: nKernPairs.
	1 to: nKernPairs do:[:i|
		kp := TTKernPair new.
		kp left: entry nextUint16.
		kp right: entry nextUint16.
		kp value: entry nextInt16.
		kernPairs at: i put: kp].
	^true

]

{ #category : 'processing' }
TTFontReader >> processKerningTable: entry [
	"Extract the kerning information for pairs of glyphs."
	| version nTables covLow covHigh |
	version := entry nextUint16. "Skip table version"
	nTables := entry nextUint16. "Skip number of sub tables -- we're using the first one only"
	nTables = 0 ifTrue: [" This detection is hacky "
		entry skip: -4. "rewind"
		^ self processOSXKerningTable: entry].
	entry skip: 2. "Skip current subtable number"
	entry skip: 2. "Skip length of subtable"
	covHigh := entry nextByte.
	covLow := entry nextByte.

	"Make sure the format is right (kerning table and format type 0)"
	((covLow allMask: 2) or: [ covHigh ~= 0]) ifTrue: [^false].
	"Subtable"
	^ self processKerningSubTableType0: entry.

]

{ #category : 'processing' }
TTFontReader >> processMaximumProfileTable: entry [
"
numGlyphs         USHORT      The number of glyphs in the font.
"
	entry skip: 4. "Skip Table version number"
	nGlyphs := entry nextUint16.
]

{ #category : 'processing' }
TTFontReader >> processNamingTable: entry [
"copyright         CHARPTR     The font's copyright notice.
familyName        CHARPTR     The font's family name.
subfamilyName     CHARPTR     The font's subfamily name.
uniqueName        CHARPTR     A unique identifier for this font.
fullName          CHARPTR     The font's full name (a combination of
                                          familyName and subfamilyName).
versionName       CHARPTR     The font's version string.
"	| nRecords initialOffset format storageOffset  strings |
	strings := Array new: 8.
	strings atAllPut:''.
	initialOffset := entry offset.
	format := entry nextUint16.
	format = 0 ifFalse: [self error: 'Cannot handle format 1 naming tables'].
	"Get the number of name records"
	nRecords := entry nextUint16.
	"Offset from the beginning of this table"
	storageOffset := entry nextUint16 + initialOffset.
	1 to: nRecords do:[:i| | pID sID lID nID length offset multiBytes string |
		pID := entry nextUint16.
		sID := entry nextUint16.
		lID := entry nextUint16.
		nID := entry nextUint16.
		length := entry nextUint16.
		offset := entry nextUint16.
		"Read only Macintosh or Microsoft strings"
		(pID = 1 or:[pID = 3 and:[sID = 1]]) ifTrue:[
			"MS uses Unicode all others single byte"
			multiBytes := pID = 3.
			string := entry stringAt: storageOffset + offset length: length multiByte: multiBytes.

			"Select only English names.
			Note: We prefer Macintosh strings about everything."
			((pID = 1 and: [lID = 0]) or: [pID = 3 and: [lID = 16r0409]]) ifTrue: [
				"Put the name at the right location."
				nID < strings size ifTrue:[
					(pID = 1 or:[(strings at: nID+1) = ''])
						ifTrue:[strings at: nID+1 put: string].
				].
			].
		].
	].
	fontDescription setStrings: strings.
]

{ #category : 'processing' }
TTFontReader >> processOSXKerningTable: entry [
	"From Apple's TrueType reference:
	
	Previous versions of the 'kern' table defined both the version and nTables fields in the header as UInt16 values and not UInt32 values. Use of the older format on OS X is discouraged (although AAT can sense an old kerning table and still make correct use of it). Microsoft Windows still uses the older format for the 'kern' table and will not recognize the newer one. Fonts targeted for OS X only should use the new format; fonts targeted for both OS X and Windows should use the old format.
	"
	"Extract the kerning information for pairs of glyphs."
	| version  nTables length coverage tupleIndex |
	version := entry nextUint32. "Skip table version"
	nTables := entry nextUint32. "Skip number of sub tables -- we're using the first one only"

	length := entry nextUint32. "Skip length of subtable"
	coverage := entry nextUint16.
	tupleIndex := entry nextUint16. "Skip tuple index".
	"Make sure the format is right (kerning table and format type 0)"
	coverage = 0 ifFalse: [^false].
	
	"Subtable"
	^ self processKerningSubTableType0: entry.

]

{ #category : 'processing' }
TTFontReader >> processSimpleGlyphContours: nContours from: entry [

	| glyph endPts nPts iLength flags xMin yMin xMax yMax |
	glyph := TTGlyph new.
	"See, for instance, https://learn.microsoft.com/en-us/typography/opentype/otspec190/glyf"
	xMin := entry nextInt16.
	yMin := entry nextInt16.
	xMax := entry nextInt16.
	yMax := entry nextInt16.
	glyph boundsLeft: xMin boundsRight: xMax boundsTop: yMax boundsBottom: yMin.
	endPts := Array new: nContours.
	1 to: nContours do: [ :i | endPts at: i put: entry nextUint16 ].
	glyph initializeContours: nContours with: endPts.
	nContours = 0 ifTrue: [^ self].
	nPts := endPts last + 1.
	iLength := entry nextUint16. "instruction length"
	entry skip: iLength.
	flags := self getGlyphFlagsFrom: entry size: nPts.
	self readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	self readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	glyph makePointsAbsolute.
	^glyph
]

{ #category : 'processing' }
TTFontReader >> readFrom: fontData fromOffset: offset [

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset characterMap numHMetrics indexToLocFormat |

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData offset: offset) == nil ifTrue: [
		"Transcript cr; show:'This font does not have a kerning table';endEntry" ].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	characterMap := self processCharacterMappingTable: charMapEntry.
	characterMap isNil
		ifTrue: [^self error:'This font has no Unicode character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue: [kernPairs := #()]
		ifFalse: [self processKerningTable: kerningEntry].
	self mapAndStoreGlyphsUsing: characterMap.
	"jmv kernPairs refer to glyph number (in the .ttf file) while fontDescription glyphs are indexed by Unicode code point...
	Correct this before making any use of the kernPairs data."
	false ifTrue: [
		fontDescription setKernPairs: kernPairs ].
]

{ #category : 'private' }
TTFontReader >> readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts [
	"Read the x coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits xValue contour |
	startPoint := 1.
	1 to: nContours do: [ :i |
		contour := glyph contours at: i.
		"Get the end point"
		endPoint := (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits := flags at: j.
			"If bit zero in the flag is set then this point is an on-curve
			point, if not, then it is an off-curve point."
			contour controlPointFlags bitAt: (j - startPoint + 1) put: 1-(flagBits bitAnd: 1).
			"First we check to see if bit one is set. This would indicate that
			the corresponding coordinate data in the table is 1 byte long.
			If the bit is not set, then the coordinate data is 2 bytes long."
			xValue := (flagBits allMask: 2)
				ifTrue: [ "one byte"
					(flagBits allMask: 16)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					"If bit four is set, then this coordinate is the same as the
					last one, so the relative offset (of zero) is stored. If bit
					is not set, then read in two bytes and store it as a signed value."
					(flagBits allMask: 16)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextInt16 ]].
			contour points at: (j - startPoint + 1) putX: xValue.
		].
		startPoint := endPoint + 1 ]
]

{ #category : 'private' }
TTFontReader >> readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts [
	"Read the y coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits yValue contour |
	startPoint := 1.
	1 to: nContours do: [ :i |
		contour := glyph contours at: i.
		"Get the end point"
		endPoint := (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits := flags at: j.
			"Check if this value one or two byte encoded"
			yValue := (flagBits allMask: 4)
				ifTrue: [ "one byte"
					(flagBits allMask: 32)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					(flagBits allMask: 32)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextInt16 ]].
			contour points at: (j - startPoint + 1) putY: yValue.
		].
		startPoint := endPoint + 1 ]
]

{ #category : 'public' }
TTFontReader >> readTTCFrom: fontData [
	"
	Answer an Array of TTFontDescription.
	Note: The purpose of TTC is completely defeated: there will be no memory savings, as each
		TTFontDescription will include the glyph data for all the glyphs it includes.
		Actually sharing glyph data between TTFontDescriptions needs some work (jmv, 16-Sep-2022).
	https://docs.fileformat.com/font/ttc/
	https://docs.microsoft.com/en-us/typography/opentype/spec/otff#font-collections
	"
	| offset tag majorVersion minorVersion numFonts fontDescriptions |
	tag := fontData copyFrom: 1 to: 4.
	tag = `'ttcf' asUtf8Bytes` ifFalse: [
		^self error:'This TTF Collection does not have a correct header'].
	majorVersion := fontData uint16At: 5 bigEndian: true.
	minorVersion := fontData uint16At: 7 bigEndian: true.
	numFonts := fontData uint32At: 9 bigEndian: true.
	(#((1 0) (2 0)) includes: {majorVersion. minorVersion}) ifFalse: [
		^self error: 'This TTF Collection has an unsupported header version'].
	fontDescriptions := Array new: numFonts.
	1 to: numFonts do: [ :i |
		fontDescription := TTFontDescription new.
		offset := fontData uint32At: i*4+9 bigEndian: true.
		self readFrom: fontData fromOffset: offset.
		fontDescriptions at: i put: fontDescription.
		fontDescription := nil. "To avoid any confusion"
	].
	^fontDescriptions
]

{ #category : 'public' }
TTFontReader >> readTTFFrom: aByteArray [
	"Read the raw font byte data"
	fontDescription := TTFontDescription new.
	self readFrom: aByteArray fromOffset: 0.
	^ fontDescription
]
