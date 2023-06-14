'From Cuis 6.0 [latest update: #5859] on 14 June 2023 at 6:07:56 pm'!

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 6/14/2023 18:00:27'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color

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
		displayUtf8: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true.
	"Please keep consistent with #makeUnderlinedGlyph:"
	aTrueTypeFont isUnderlined ifTrue: [
		d := (aTrueTypeFont descent / 2) ceiling.
		engine strokeWidth: 2 color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := (aTrueTypeFont ascent / 4) ceiling.
		engine strokeWidth: 2 color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 6/14/2023 17:38:47'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap deltaXForBaseGlyph deltaXForDiacriticalMark |
	deltaXForBaseGlyph := 0.
	ascent := aTrueTypeFont ascent.		  "expensive"
	lineGap := aTrueTypeFont lineGap."expensive"
	destY := aPoint y.
	fdestX := aPoint x.
	maxHeight := 0.
	byteIndex := byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph := aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ] 
			into: nil.
		"Center diacriticals marks."
		glyph advanceWidth = 0.0
			ifFalse: [
				deltaXForBaseGlyph := glyph deltaXToCenter.
				deltaXForDiacriticalMark := 0.0 ]
			ifTrue: [
				deltaXForDiacriticalMark := glyph deltaXToCenter.
				deltaXForDiacriticalMark = 0 ifFalse: [ "Only for those who specify a delta to center."
					deltaXForDiacriticalMark := deltaXForDiacriticalMark - deltaXForBaseGlyph ].
				 ].
		destX := (fdestX + glyph leftOffset + deltaXForDiacriticalMark) roundedHAFZ.
		destY := aPoint y + ascent + lineGap-1 - glyph topOffset.
		"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
		aTrueTypeFont isSuperscript
			ifTrue: [destY := destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY := destY + (ascent*0.3)]].
		destY := destY roundedHAFZ.
		sourceForm := glyph form.
		height := sourceForm height.
		width := sourceForm width.
		self copyBits.
		fdestX := fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight := maxHeight max: height.
		byteIndex := byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight)! !


!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 6/14/2023 17:42:42'!
glyphAtUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil
	"Needed only for BitBltCanvasEngine"

	| lastArray lastIndex |
	lastArray := glyphFormsByUtf8 .
	lastIndex := byte1 + 1.
	byte2OrNil notNil ifTrue: [
		(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
		lastArray := lastArray at: lastIndex.
		lastIndex := (byte2OrNil bitAnd: 63) + 1.
		byte3OrNil notNil ifTrue: [
			(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
			lastArray := lastArray at: lastIndex.
			lastIndex := (byte3OrNil bitAnd: 63) + 1.
			byte4OrNil notNil ifTrue: [
				(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
				lastArray := lastArray at: lastIndex.
				lastIndex := (byte4OrNil bitAnd: 63) + 1 ]]].
	^ (lastArray at: lastIndex) ifNil: [
		| formGlyph |
		formGlyph := (ttFontDescription
				formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil
				byte3: byte3OrNil byte4: byte4OrNil
				pixelSize: pointSize)
			ifNil: [#absentGlyph]
			ifNotNil: [ :theGlyph |
				self isUnderlined ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				self isStruckThrough ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		lastArray at: lastIndex put: formGlyph.
		formGlyph ]! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 6/14/2023 18:01:02'!
makeStruckThroughGlyph: aFormGlyph
	| form |
	"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
	form := aFormGlyph form.
	form fillBlack: (0 @ (aFormGlyph topOffset - self descent) extent: form width @ 1)! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 6/14/2023 18:00:56'!
makeUnderlinedGlyph: aFormGlyph
	| height y oldForm form d |
	"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
	d := (self descent / 4) ceiling.
	y := aFormGlyph topOffset + d.
	form := aFormGlyph form.
	height := aFormGlyph topOffset + (2*d).
	height > form height ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width @ height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `0@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form ].
	form fillBlack: (0 @ y extent: form width @ d).! !

