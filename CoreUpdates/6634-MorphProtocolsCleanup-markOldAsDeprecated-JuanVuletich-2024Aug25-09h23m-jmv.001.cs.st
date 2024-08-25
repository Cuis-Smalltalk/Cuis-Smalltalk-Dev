'From Cuis7.1 [latest update: #6630] on 25 August 2024 at 9:26:06 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:25:28'!
morphExtentInOwner

	self flag: #deprecated.
	^self extentInOwner! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:25:06'!
morphExtentInOwner: newExtent

	self flag: #deprecated.
	^self extentInOwner: newExtent! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:24:31'!
morphExtentInWorld
	"World coordinates"

	self flag: #deprecated.
	^self extentInWorld! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:24:56'!
morphExtentInWorld: newExtent
	"world coordinates"

	self flag: #deprecated.
	^self extentInWorld: newExtent! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:24:42'!
morphHeightInOwner: newHeight

	self flag: #deprecated.
	^ self heightInOwner: newHeight! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:24:21'!
morphPosition: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self flag: #deprecated.
	^self position: newPos extent: newExtent! !

