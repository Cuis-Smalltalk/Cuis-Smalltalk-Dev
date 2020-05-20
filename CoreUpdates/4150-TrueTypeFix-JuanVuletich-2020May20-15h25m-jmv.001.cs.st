'From Cuis 5.0 [latest update: #4149] on 20 May 2020 at 3:25:56 pm'!

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/20/2020 15:18:06'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

