'From Cuis 5.0 [latest update: #4542] on 23 March 2021 at 1:03:48 pm'!

!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 3/23/2021 13:02:51'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font
	"Answer position to place next glyph"

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: font baseKern.
	^ destX@(destY+font lineSpacing)! !

!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 3/23/2021 13:03:01'!
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	self installStrikeFont: aStrikeFont foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
		combinationRule := prevRule ].
	^answer! !


!AbstractFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 3/23/2021 13:02:41'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer position to place next glyph
	Answer nil if nothing was done"

	self subclassResponsibility! !


!StrikeFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 3/23/2021 13:03:31'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer position to place next glyph.
	Answer nil if nothing was done."

	^ engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:09'!
drawString: s at: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:12'!
drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:19'!
drawString: s atBaseline: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atBaseline: pt font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:21'!
drawString: s atCenterX: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		at: pt - ((font widthOfString: s) / 2 @ 0)
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:23'!
drawString: s atWaist: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atWaist: pt font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:25'!
drawString: s atWaistRight: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		atWaist: pt - ((font widthOfString: s) @ 0)
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:03:27'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: font
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + `0@2`
			font: font
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + `0@1`
		font: font
		color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:02:43'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint rounded.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 rounded.
	engine colorMap: nil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/23/2021 13:02:45'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"
	| p1 |

	engine ifNil: [ ^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1 + `0@2`
			color: bottomColor ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + `0@1`
		color: aColor.! !

