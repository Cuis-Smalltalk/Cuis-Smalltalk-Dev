'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 14 December 2013 at 8:36:36.095003 am'!

!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 12/14/2013 08:35'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	x _ aHand morphPosition x - 20.
	y _ aHand morphPosition y + 20.
	x + self morphWidth > aHand world morphWidth ifTrue: [
		x _ aHand world morphWidth - self morphWidth ].
	y + self morphHeight > aHand world morphHeight ifTrue: [
		y _ aHand morphPosition y - self morphHeight - 12 ].
	newPos _ x@y.
	self morphPosition: newPos.
	aHand balloonHelp: self! !

