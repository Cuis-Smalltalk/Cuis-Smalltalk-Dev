'From Cuis7.1 [latest update: #6630] on 25 August 2024 at 9:21:26 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:19:44'!
morphPositionInWorld
	"Answer is in world coordinates."

	self flag: #deprecated.
	^ self positionInWorld.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:19:56'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph. Argument is in world coordinates.
	Ignored by morphs that are not movable."

	self flag: #deprecated.
	^self positionInWorld: newPositionInWorld! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:18:38'!
morphLocalBounds

	self flag: #deprecated.
	^self localBounds! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:18:56'!
morphWidthInOwner: newWidth

	self flag: #deprecated.
	^self widthInOwner: newWidth! !

!methodRemoval: WorldMorph #morphPositionInWorld stamp: 'jmv 8/25/2024 09:20:17'!
WorldMorph removeSelector: #morphPositionInWorld!
!methodRemoval: BoxedMorph #morphLocalBoundsForError stamp: 'jmv 8/25/2024 09:18:06'!
BoxedMorph removeSelector: #morphLocalBoundsForError!
!methodRemoval: PlacedMorph #morphPositionInWorld: stamp: 'jmv 8/25/2024 09:19:58'!
PlacedMorph removeSelector: #morphPositionInWorld:!
!methodRemoval: Morph #morphLocalBoundsForError stamp: 'jmv 8/25/2024 09:18:05'!
Morph removeSelector: #morphLocalBoundsForError!
