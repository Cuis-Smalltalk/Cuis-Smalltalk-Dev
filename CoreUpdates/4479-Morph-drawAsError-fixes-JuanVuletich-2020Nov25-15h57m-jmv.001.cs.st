'From Cuis 5.0 [latest update: #4478] on 25 November 2020 at 3:59:41 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 11/25/2020 13:10:56'!
drawingFails
	self world addKnownFailing: self.
	self redrawNeeded.
! !

!Morph methodsFor: 'drawing' stamp: 'jmv 11/25/2020 13:10:51'!
drawingFailsNot
	self world removeKnownFailing: self.
	self redrawNeeded.
! !


!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 11/25/2020 13:10:47'!
removeAllKnownFailing
	drawingFailingMorphs do: [ :m | m redrawNeeded ].
	drawingFailingMorphs _ WeakIdentitySet new.
! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/25/2020 12:53:08'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft to: r bottomRight width: w color: `Color yellow`.
	self line: r topRight to: r bottomLeft width: w color: `Color yellow`.
	world notNil ifTrue: [					"Only if we are drawing the world"
		currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/25/2020 12:34:48'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

