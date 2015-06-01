'From Cuis 4.2 of 25 July 2013 [latest update: #2344] on 1 June 2015 at 1:33:45.090123 pm'!

!AbstractFont methodsFor: 'as yet unclassified' stamp: 'jmv 6/1/2015 13:29'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"
	| char fallbackFont x y answer |
	fallbackFont _ (AbstractFont familyName: 'DejaVu' aroundPointSize: self pointSize*0.9) emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!StrikeFont methodsFor: 'as yet unclassified' stamp: 'jmv 6/1/2015 13:29'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^aGrafPort
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!GrafPort methodsFor: 'text' stamp: 'jmv 6/1/2015 13:28'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font
	"Answer last affected pixel position"

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: font baseKern.
	^ destX@(destY+font height)! !

!GrafPort methodsFor: 'text' stamp: 'jmv 6/1/2015 13:28'!
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer last affected pixel position
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	self installStrikeFont: font foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font.
		combinationRule := prevRule ].
	^answer! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/1/2015 13:30'!
drawString: s at: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/1/2015 13:30'!
drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/1/2015 13:30'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + (0@2)
			font: fontOrNil
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + (0@1)
		font: fontOrNil
		color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/1/2015 13:30'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint) rounded.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font on: port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 5/31/2015 00:28'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	lastDisplayPosition < innerRectangle right ifTrue: [
		font _ AbstractFont default.
		fh _ font height.
		count _ innerRectangle height // fh-1.
		x _ innerRectangle left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
		canvas _ aForm getCanvas.
		canvas newClipRect: (innerRectangle left: lastDisplayPosition).
		(canvas drawString: string at: x@y font: font color: Color veryDarkGray) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x ]]! !

