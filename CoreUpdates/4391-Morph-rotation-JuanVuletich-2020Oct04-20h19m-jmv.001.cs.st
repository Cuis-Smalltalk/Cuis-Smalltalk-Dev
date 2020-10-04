'From Cuis 5.0 [latest update: #4391] on 4 October 2020 at 8:20:56 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/4/2020 20:20:33'!
rotation: radians
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

    self rotation: radians scale: 1.! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/4/2020 20:20:39'!
rotateBy: radians
	"Change the rotation of this morph. Argument is an angle (possibly negative), to be added to current rotation."

	location _ location rotatedBy: radians.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

