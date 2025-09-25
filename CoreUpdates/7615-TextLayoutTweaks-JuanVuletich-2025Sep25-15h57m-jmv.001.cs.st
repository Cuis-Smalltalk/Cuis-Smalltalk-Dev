'From Cuis7.5 [latest update: #7614] on 25 September 2025 at 3:58:15 pm'!

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 9/25/2025 15:34:58'!
lineSpacing
	"Answer the height of the receiver including any additional line gap."

	^self ascent + self descent + self lineGap! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 9/25/2025 15:42:22'!
makeStruckThroughGlyph: aFormGlyph
	| oldForm form d y |
	"Please keep consistent with
	#drawOnVectorEngine:from:to:at:font:color:
	#drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:
	Similar to: #makeUnderlinedGlyph:"

	form := aFormGlyph form.
	form width = 0 ifTrue: [ ^self ].
	d := (self descent / 4) ceiling.
	y := (aFormGlyph topOffset - self descent - d) floor.
	true ifTrue: [
		oldForm := form.
		form := Form extent: oldForm width+2 @ oldForm height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `1@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form leftOffsetBy: -1].
	form fillBlack: (0 @ y extent: form width @ d).
	"For testing it:
	FontFamily releaseClassCachedState
	"! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 9/25/2025 15:52:12'!
setActualFont: aFont
	"Keep track of max height and ascent for auto lineheight"

	| oldAscent oldLineSpacingMinusAscent |
	super setActualFont: aFont.
	lineHeight
		ifNil: [
			baseline := font ascent.
			lineHeight := font lineSpacing ]
		ifNotNil: [
			oldAscent := baseline.
			oldLineSpacingMinusAscent := lineHeight - oldAscent.
			baseline := baseline max: font ascent.
			lineHeight := lineHeight max: baseline + (oldLineSpacingMinusAscent max: font lineSpacing - font ascent) ]! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 9/25/2025 13:55:49'!
placeEmbeddedObject: anchoredFormOrMorph
	| prevBaseline h |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	prevBaseline := baseline.
	h := (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner height ]
		ifFalse: [ anchoredFormOrMorph height ].
	baseline := baseline max: h.
	lineHeight := lineHeight + baseline - prevBaseline.
	line stop: lastIndex.
	^ true! !


!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 9/25/2025 15:37:44'!
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
		d := aTrueTypeFont ascent * 0.3.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !

!UnicodeString methodsFor: 'dispatch' stamp: 'jmv 9/25/2025 15:43:07'!
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
		d := aTrueTypeFont ascent * 0.3.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/25/2025 15:49:14'!
drawString: s atWaistCenter: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font dy |
	font := self fontToUse: fontOrNil.
	dy := currentTransformation doesMirror
		ifFalse: [ font ascent * 0.25 ]
		ifTrue: [ font ascent * -0.25 ].
	^self
		drawString: s
		from: 1 to: s size
		atBaseline: pt + ((font widthOfString: s) negated / 2 @ dy)
		font: font color: aColor! !

