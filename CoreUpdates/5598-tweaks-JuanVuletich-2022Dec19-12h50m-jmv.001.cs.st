'From Cuis 6.0 [latest update: #5597] on 19 December 2022 at 12:55:32 pm'!

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 08:44:33'!
initializeWithTranslation: aPoint
	"Set up these only on initialization."

	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].
	currentMorph _ nil.! !

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 08:43:19'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	cti _ 1.
	currentTransformation _ transformations at: cti.
	currentMorph _ drawingMorphStack at: cti.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 10:39:52'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"The main canvas should only be drawn by the UI process.
	If some other process has been set as the UI process, don't continue, as breakage is likely."
	world ifNotNil: [
		world canvas == self ifTrue: [
			UISupervisor isUIProcessRunning ifFalse: [^self]]].
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self canvasToUse drawCurrentAsOutline ifTrue: [
			self outOfMorph.
			^ self]].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

