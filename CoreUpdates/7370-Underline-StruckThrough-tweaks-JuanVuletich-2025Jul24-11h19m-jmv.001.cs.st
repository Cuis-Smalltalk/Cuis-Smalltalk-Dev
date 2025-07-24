'From Cuis7.5 [latest update: #7370] on 24 July 2025 at 12:11:57 pm'!

!FormGlyph methodsFor: 'initialization' stamp: 'jmv 7/24/2025 11:33:12'!
form: aForm leftOffsetBy: aNumber.
	form := aForm.
	leftOffset := leftOffset + aNumber! !


!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 7/24/2025 11:57:10'!
makeStruckThroughGlyph: aFormGlyph
	| oldForm form d |
	"Please keep consistent with
	#drawOnVectorEngine:from:to:at:font:color:
	#drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:
	Similar to: #makeUnderlinedGlyph:"

	form := aFormGlyph form.
	form width = 0 ifTrue: [ ^self ].
	d := (self descent / 4) ceiling.
	true ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width+2 @ oldForm height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `1@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form leftOffsetBy: -1].
	form fillBlack: (0 @ (aFormGlyph topOffset - self descent) floor extent: form width @ d).
	"For testing it:
	FontFamily releaseClassCachedState
	"! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 7/24/2025 12:10:23'!
makeUnderlinedGlyph: aFormGlyph
	| height y oldForm form d |
	"Please keep consistent with
	#drawOnVectorEngine:from:to:at:font:color:
	#drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:
	Similar to #makeStruckThroughGlyph:"

	form := aFormGlyph form.
	form width = 0 ifTrue: [ ^self ].
	d := (self descent / 4) ceiling.
	y := aFormGlyph topOffset + d.
	height := aFormGlyph topOffset + (2*d).
	true "height > form height" ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width+2 @ height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `1@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form leftOffsetBy: -1 ].
	form fillBlack: (0 @ y extent: form width @ d).
	"For testing it:
	FontFamily releaseClassCachedState
	"! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 7/24/2025 11:41:40'!
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
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph. rightArrowGlyph. upArrowGlyph. downArrowGlyph. carriageReturnGlyph}! !


!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 7/24/2025 12:06:50'!
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
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph: and #drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := aTrueTypeFont ascent * 0.25.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 7/24/2025 12:07:04'!
drawOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color borderWidth: sw borderColor: sc

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
		strokeWidth: sw strokeColor: sc
		scale: aTrueTypeFont pointSize
		preventMirroring: true.
	"Please keep consistent with #makeUnderlinedGlyph: and #drawOnVectorEngine:from:to:at:font:color:"
	aTrueTypeFont isUnderlined ifTrue: [
		d := aTrueTypeFont descent *0.7.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := aTrueTypeFont ascent * 0.25.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !

