'From Cuis 4.1 of 12 December 2012 [latest update: #1762] on 24 July 2013 at 11:32:36.921804 am'!

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/22/2013 10:08'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	| p1 font kern insideColor |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		color: insideColor
		kern: kern! !

