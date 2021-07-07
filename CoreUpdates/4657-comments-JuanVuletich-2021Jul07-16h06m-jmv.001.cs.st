'From Cuis 5.0 [latest update: #4656] on 7 July 2021 at 4:07:27 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 15:56:26'!
morphExtent: aPoint
	"In our own coordinates!!
	Ignored by morphs that are not resizeable."! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 15:56:17'!
morphExtentInWorld: newExtent
	"Argument is in world coordinates.
	Ignored by morphs that are not resizeable."! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 15:55:29'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not movable."! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 15:55:58'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph. Argument is in world coordinates.
	Ignored by morphs that are not movable."! !

!Morph methodsFor: 'private' stamp: 'jmv 7/7/2021 15:56:03'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not movable."! !

