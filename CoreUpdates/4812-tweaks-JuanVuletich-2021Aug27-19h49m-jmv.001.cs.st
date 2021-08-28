'From Cuis 5.0 [latest update: #4811] on 27 August 2021 at 7:56:57 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 8/27/2021 14:06:57'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the WidgetMorph hierarchy should answer #true to wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	(aMorph displayBoundsIntersects: (privateDisplayBounds outsetBy: 1)) ifFalse: [
		^false ].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/26/2021 17:38:12'!
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
	self line: r topLeft+1 to: r bottomRight-7 width: w color: `Color yellow`.
	self line: r topRight + (-7@1) to: r bottomLeft + (1@ -7) width: w color: `Color yellow`.	
	currentMorph displayBoundsSetFrom: self.! !

