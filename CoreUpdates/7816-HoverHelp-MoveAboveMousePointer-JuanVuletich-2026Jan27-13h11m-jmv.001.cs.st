'From Cuis7.7 [latest update: #7815] on 27 January 2026 at 1:20:04 pm'!

!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 27/Jan/2026 13:18:25'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| dynamicContents newPos x y |
	dynamicContents := target
		ifNil: [ contents ]
		ifNotNil: [
			getter
				ifNotNil: [target perform: getter]
				ifNil: [target balloonText]].
	self contents: dynamicContents wrappedTo: nil.
	x := aHand morphPosition x - (extent x // 4)
		min: aHand world morphWidth - extent x - 10
		max: 10.
	y := aHand morphPosition y - extent y - 10.
	y < 5 ifTrue: [
		y := aHand morphPosition y + (Cursor currentCursor height * 3//2) ].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self.! !

