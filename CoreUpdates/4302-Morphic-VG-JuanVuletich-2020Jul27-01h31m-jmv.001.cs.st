'From Cuis 5.0 [latest update: #4298] on 27 July 2020 at 1:32:21 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 00:28:46'!
fullUpdateBounds: aMorph
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self canvasToUse fullUpdateCurrentBounds.
		self outOfMorph
		]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 00:28:33'!
fullUpdateCurrentBounds
	| currentMorphBounds |
	currentMorph drawOn: self.
	currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
	currentMorph displayBounds: currentMorphBounds.
	currentMorph submorphsDo: [ :m |
		self fullUpdateBounds: m ]! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 01:31:46'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder
	| currentMorphBounds |
	addCurrentBounds ifTrue: [
		currentMorph setProperty: #needsRedraw toValue: false.
		currentMorph drawOn: self.
		currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
		currentMorph displayBounds: currentMorphBounds.
		aDamageRecorder recordInvalidRect: currentMorphBounds.
		currentMorph submorphsDo: [ :m | self fullUpdateBounds: m ].
	].
	trySubmorphs ifTrue: [
		currentMorph setProperty: #submorphNeedsRedraw toValue: false.
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]
	].! !

