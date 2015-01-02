'From Cuis 4.2 of 25 July 2013 [latest update: #2137] on 20 December 2014 at 3:36:22.27396 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/20/2014 15:35'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > (50@50)
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	self line: r topLeft to: r bottomRight-w width: w color: Color yellow.
	self line: r topRight -(w@0) to: r bottomLeft -(0@w)width: w color: Color yellow! !

