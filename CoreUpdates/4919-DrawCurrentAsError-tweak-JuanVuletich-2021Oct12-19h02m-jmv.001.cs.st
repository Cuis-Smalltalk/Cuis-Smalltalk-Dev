'From Cuis 5.0 [latest update: #4913] on 12 October 2021 at 7:11:31 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/12/2021 19:10:33'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBoundsForError.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft+1 to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight + (w negated@1) to: r bottomLeft + (1@ w negated) width: w color: `Color yellow`.	
	currentMorph displayBoundsSetFrom: self.! !

