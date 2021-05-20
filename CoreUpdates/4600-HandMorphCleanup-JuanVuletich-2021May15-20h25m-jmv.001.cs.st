'From Cuis 5.0 [latest update: #4597] on 15 May 2021 at 8:27:04 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 5/15/2021 19:44:30'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	self needsRedraw: false.
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		prevFullBounds _ nil ].! !


!WorldMorph methodsFor: 'hands' stamp: 'jmv 5/15/2021 20:26:17'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result |
	result _ OrderedCollection new.
	hands do: [:hand |
		hand needsToBeDrawn ifTrue: [
			hand isRedrawNeeded
				ifTrue: [result add: hand]
				ifFalse: [
					hand displayFullBounds ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							result add: hand]]]]].
	^ result! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 5/15/2021 20:26:07'!
checkIfUpdateNeeded

	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h isRedrawNeeded and: [h needsToBeDrawn]) ifTrue: [^true]].
	^false  "display is already up-to-date"
! !

!methodRemoval: HandMorph #hasChanged stamp: 'jmv 5/15/2021 20:26:40'!
HandMorph removeSelector: #hasChanged!
!methodRemoval: HandMorph #redrawNeeded stamp: 'jmv 5/15/2021 20:25:39'!
HandMorph removeSelector: #redrawNeeded!
