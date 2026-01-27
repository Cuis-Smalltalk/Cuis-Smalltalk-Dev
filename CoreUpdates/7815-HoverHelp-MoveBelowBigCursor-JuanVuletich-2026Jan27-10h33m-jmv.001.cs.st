'From Cuis7.7 [latest update: #7814] on 27 January 2026 at 10:34:26 am'!

!Cursor class methodsFor: 'current cursor' stamp: 'jmv 27/Jan/2026 10:33:37'!
currentCursor: aCursor
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor := aCursor.
	(Preferences at: #biggerCursors)
		ifTrue: [
			[ CurrentCursor := aCursor asBigCursor] 
				on: Error do: nil  "fall through "].
	CurrentCursor installCursor.! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 27/Jan/2026 10:17:11'!
normalCursor
	"Answer the instance of me that is the shape of an arrow slanted left.
	Generally use
		Cursor defaultCursor
	instead.
	"
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1000000000000000
		2r1100000000000000
		2r1110000000000000
		2r1111000000000000
		2r1111100000000000
		2r1111110000000000
		2r1111111000000000
		2r1111100000000000
		2r1111100000000000
		2r1001100000000000
		2r0000110000000000
		2r0000110000000000
		2r0000011000000000
		2r0000011000000000
		2r0000001100000000
		2r0000001100000000)
	offset: `0@0`! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 27/Jan/2026 10:28:59'!
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
	x := aHand morphPosition x - Cursor currentCursor width.
	y := aHand morphPosition y + Cursor currentCursor height.
	x + extent x > aHand world morphWidth ifTrue: [
		x := aHand world morphWidth - extent x ].
	y + extent y > aHand world morphHeight ifTrue: [
		y := aHand morphPosition y - extent y - 12 ].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self.! !

