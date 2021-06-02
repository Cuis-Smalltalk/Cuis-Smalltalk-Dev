'From Cuis 5.0 [latest update: #4624] on 2 June 2021 at 9:15:33 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 6/2/2021 09:08:33'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r r2 w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	r2 _ r origin corner: r corner-w.
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r2 topLeft to: r2 bottomRight width: w color: `Color yellow`.
	self line: r2 topRight to: r2 bottomLeft width: w color: `Color yellow`.
	currentMorph displayBoundsSetFrom: self.! !

