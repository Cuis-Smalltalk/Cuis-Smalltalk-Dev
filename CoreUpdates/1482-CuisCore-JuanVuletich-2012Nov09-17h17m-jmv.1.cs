'From Cuis 4.0 of 21 April 2012 [latest update: #1481] on 9 November 2012 at 6:02:13 pm'!

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/9/2012 17:45'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: c kern: kernOrNil
	| font portRect bounds kern |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [port clipRect: portRect. ^self].
	port clipWidth = 0 ifTrue: [port clipRect: portRect. ^self].
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft
		strikeFont: font
		kern: kern.
	port clipRect: portRect! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/9/2012 17:44'!
drawString: s at: pt font: aFont color: aColor

	^ self drawString: s from: 1 to: s size at: pt font: aFont color: aColor kern: nil! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/9/2012 17:43'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c kern: kernOrNil

	| p1 font kern |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		kern: kern! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/9/2012 17:46'!
drawString: s in: boundsRect font: fontOrNil color: c
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c kern: nil! !

!methodRemoval: FormCanvas #drawString:from:to:at:font:color:!
FormCanvas removeSelector: #drawString:from:to:at:font:color:!
!methodRemoval: FormCanvas #drawString:from:to:in:font:color:!
FormCanvas removeSelector: #drawString:from:to:in:font:color:!
