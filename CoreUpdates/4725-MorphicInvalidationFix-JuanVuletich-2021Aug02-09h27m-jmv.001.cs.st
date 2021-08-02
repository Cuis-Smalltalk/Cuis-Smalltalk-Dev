'From Cuis 5.0 [latest update: #4724] on 2 August 2021 at 9:28:20 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/2/2021 09:25:35'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder

	| invalidateOwns |
	invalidateOwns _ false.
	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		invalidateOwns _ true ].
	trySubmorphs ifTrue: [
		(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not])
			ifTrue: [
				currentMorph submorphNeedsRedraw: false.
				currentMorph submorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]]
			ifFalse: [
				currentMorph fullRedrawNotNeeded.
				invalidateOwns _ true ]].
	invalidateOwns ifTrue: [
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ]! !

