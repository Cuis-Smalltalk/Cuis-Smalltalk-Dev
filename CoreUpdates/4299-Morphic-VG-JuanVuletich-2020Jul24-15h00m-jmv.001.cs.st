'From Cuis 5.0 [latest update: #4295] on 24 July 2020 at 3:03:15 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/24/2020 15:00:40'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder
	| currentMorphBounds |
	addCurrentBounds ifTrue: [
		currentMorph setProperty: #needsRedraw toValue: false.
		currentMorph drawOn: self.
		currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
		currentMorph displayBounds: currentMorphBounds.
		aDamageRecorder recordInvalidRect: currentMorphBounds.
	].
	trySubmorphs ifTrue: [
		currentMorph setProperty: #submorphNeedsRedraw toValue: false.
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]
	].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/24/2020 15:03:08'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addCurrentBounds trySubmorphs |
	aMorph visible ifTrue: [
		addCurrentBounds _ aMorph valueOfProperty: #needsRedraw ifAbsent: [ false ].
		trySubmorphs _ aMorph valueOfProperty: #submorphNeedsRedraw ifAbsent: [ false ].
		(trySubmorphs or: [addCurrentBounds]) ifTrue: [
			self into: aMorph.
			self canvasToUse
				fullAddCurrentRect: addCurrentBounds
				submorphs: trySubmorphs
				to: aDamageRecorder.
			self outOfMorph
		]]! !

