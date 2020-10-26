'From Cuis 5.0 [latest update: #4431] on 26 October 2020 at 4:42:31 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/26/2020 16:18:31'!
currentMorphDrawingFails
	currentMorph drawingFails! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/26/2020 16:19:30'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifError: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		self setCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/26/2020 16:34:26'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder

	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		self fullUpdateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph.
	].
	trySubmorphs ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]
	].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/26/2020 16:28:23'!
fullUpdateCurrentBounds
	| currentMorphBounds isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph drawOn: self ].
	currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
	currentMorph displayBounds: currentMorphBounds.
	isKnownFailing ifFalse: [
		currentMorph submorphsDo: [ :m |
			self fullUpdateBounds: m ]]! !

