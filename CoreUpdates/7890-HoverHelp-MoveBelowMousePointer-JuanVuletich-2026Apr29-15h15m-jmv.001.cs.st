'From Cuis7.7 [latest update: #7889] on 29 April 2026 at 3:17:47 pm'!

!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 29/Apr/2026 15:15:43'!
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
	"Tooltip ABOVE mouse pointer"
	"x := aHand morphPosition x - (extent x // 4)
		min: aHand world morphWidth - extent x - 10
		max: 10.
	y := aHand morphPosition y - extent y - 10.
	y < 5 ifTrue: [
		y := aHand morphPosition y + (Cursor currentCursor height * 3//2) ]."
	"Tooltip BELOW mouse pointer (standard in most UIs)"
	x := aHand morphPosition x min: aHand world morphWidth - extent x - 10.
	y := aHand morphPosition y + Cursor currentCursor height.
	y + extent y > aHand world morphHeight ifTrue: [
		y := aHand morphPosition y - extent y - 12].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self.! !

