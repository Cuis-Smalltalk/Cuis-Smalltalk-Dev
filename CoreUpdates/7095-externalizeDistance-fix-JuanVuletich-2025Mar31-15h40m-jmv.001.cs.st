'From Cuis7.3 [latest update: #7093] on 31 March 2025 at 3:41:00 pm'!

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 3/31/2025 15:37:48'!
externalizeDistance: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ (location externalizeDelta: aPoint) abs! !

