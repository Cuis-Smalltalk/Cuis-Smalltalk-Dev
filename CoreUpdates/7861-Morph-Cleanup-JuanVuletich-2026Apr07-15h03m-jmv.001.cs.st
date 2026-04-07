'From Cuis7.7 [latest update: #7858] on 7 April 2026 at 3:11:59 pm'!

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/Apr/2026 15:11:44'!
rotationDegrees: degrees
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

	self rotation: degrees degreesToRadians! !

!methodRemoval: Morph #rotationDegrees: stamp: 'jmv 7/Apr/2026 15:11:49'!
Morph removeSelector: #rotationDegrees:!
