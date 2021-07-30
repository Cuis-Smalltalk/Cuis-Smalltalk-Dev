'From Cuis 5.0 [latest update: #4716] on 30 July 2021 at 3:38:36 pm'!

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/30/2021 12:11:02'!
fullRedrawNotNeeded
	"Clear redraw flags for receiver and all submorphs (but only if set!!)"

	self isSubmorphRedrawNeeded ifTrue: [
		self submorphsDo: [ :m |
			m fullRedrawNotNeeded ]].

	"Equivalent to
	self needsRedraw: false.
	self submorphNeedsRedraw: false.
	"
	id _ id bitAnd: `3 bitInvert`! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/30/2021 11:38:26'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph isOrthoRectangularMorph
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]].
			(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not]) ifTrue: [
				currentMorph submorphsDo: [ :m |
					self fullUpdateProtrudingBounds: m ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/30/2021 11:35:01'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self canvasToUse fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/30/2021 12:22:21'!
updateCurrentBounds
	"RUpdate display bounds, for currentMorph only."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph isOrthoRectangularMorph
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/30/2021 12:22:33'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder

	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ].
	trySubmorphs ifTrue: [
		(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not])
			ifTrue: [
				currentMorph submorphNeedsRedraw: false.
				currentMorph submorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]]
			ifFalse: [ currentMorph fullRedrawNotNeeded ]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/30/2021 11:40:17'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph displayBounds ifNil: [
					self fullUpdateProtrudingBounds: morph ]].
			self outOfMorph ]]! !

!methodRemoval: MorphicCanvas #fullUpdateCurrentBounds stamp: 'jmv 7/30/2021 15:37:25'!
MorphicCanvas removeSelector: #fullUpdateCurrentBounds!
!methodRemoval: MorphicCanvas #fullUpdateBounds: stamp: 'jmv 7/30/2021 15:37:22'!
MorphicCanvas removeSelector: #fullUpdateBounds:!
