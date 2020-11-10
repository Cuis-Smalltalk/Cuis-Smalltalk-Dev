'From Cuis 5.0 [latest update: #4444] on 10 November 2020 at 2:37:36 pm'!

!GeometryTransformation class methodsFor: 'instance creation' stamp: 'jmv 10/23/2020 19:44:01'!
identity
	^MorphicTranslation withTranslation: 0@0! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/10/2020 14:29:48'!
orbitBy: radians
	"Rotate the receiver around the origin (0@0) in owner coordinates.
	Argument is an angle (possibly negative), to be added to current rotation."

	location _ (AffineTransformation withRadians: radians) composedWith: location.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/10/2020 14:32:19'!
rotationDegrees: degrees
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

	self rotation: degrees degreesToRadians! !

