'From Cuis 5.0 [latest update: #4564] on 9 April 2021 at 2:13:37 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 4/9/2021 14:13:23'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw ]]].! !

