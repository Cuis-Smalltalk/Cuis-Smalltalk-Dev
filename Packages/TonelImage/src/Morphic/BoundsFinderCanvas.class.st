Class {
	#name : 'BoundsFinderCanvas',
	#superclass : 'AbstractVectorCanvas',
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'morphic' }
BoundsFinderCanvas >> clipByCurrentMorphStart [

]

{ #category : 'morphic' }
BoundsFinderCanvas >> clipByCurrentMorphStop [

]

{ #category : 'morphic' }
BoundsFinderCanvas >> displayBoundsForHand: aHand [
	"See #drawHand."

	^self drawsHandLarge
		ifTrue: [aHand morphPosition asIntegerPoint-`9@5` extent: `45@48`]
		ifFalse: [aHand morphPosition asIntegerPoint-`4@3` extent: `21@24`].
]

{ #category : 'morphic' }
BoundsFinderCanvas >> drawCurrentAndSubmorphs [
	| current |
	currentMorph visible ifFalse: [ ^self ].

	currentMorph drawOn: self.
	self displayBoundsSetOnCurrentMorph.

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')
	Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	currentMorph hasSubmorphs ifTrue: [
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	(current postDrawOn: self) ifTrue: [
		self displayBoundsUpdateOnCurrentMorph ].

	current isHighlighted ifTrue: [
		self drawHighlight: current ].
]

{ #category : 'morphic' }
BoundsFinderCanvas >> fullAddCurrentMorphTo: aDamageRecorder [

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r | | wb |
			wb := world localBounds.
			(wb intersects: r) ifTrue: [
				aDamageRecorder
					recordInvalidRect: (r intersect: wb)
					for: currentMorph ]]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].
]

{ #category : 'morphic' }
BoundsFinderCanvas >> fullAddRedrawRect: aMorph to: aDamageRecorder [
	
	aMorph visible ifTrue: [
		(aMorph isRedrawNeeded or: [aMorph isSubmorphRedrawNeeded]) ifTrue: [
			self into: aMorph.
			self fullAddCurrentMorphTo: aDamageRecorder.
			self outOfMorph
		]]
]

{ #category : 'morphic' }
BoundsFinderCanvas >> fullUpdateCurrentProtrudingBounds [
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	currentMorph knowsOwnLocalBounds
		ifTrue: [self displayBoundsSetOnCurrentMorph]
		ifFalse: [
			currentMorph isKnownFailing
				ifTrue: [
					self drawCurrentAsError ]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph drawOn: self.
					self displayBoundsSetOnCurrentMorph.
					(currentMorph postDrawOn: self) ifTrue: [
						self displayBoundsUpdateOnCurrentMorph ].
					(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifTrue: [
						currentMorph submorphsDo: [ :m |
							self fullUpdateProtrudingBounds: m ]]]].
]

{ #category : 'morphic' }
BoundsFinderCanvas >> fullUpdateProtrudingBounds: aMorph [
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].
]

{ #category : 'drawing - MorphicCanvas protocol' }
BoundsFinderCanvas >> image: srcForm at: aPoint [

	"Compute bounds"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black`.
]

{ #category : 'drawing - MorphicCanvas protocol' }
BoundsFinderCanvas >> imageUnrotated: srcForm at: aPoint [

	"Compute bounds"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black`.
]

{ #category : 'initialization' }
BoundsFinderCanvas >> initialize [
	engine := VectorEngineBoundsFinder new.
	engine canvas: self.
	clippingMorphDisplayBounds := nil.
]

{ #category : 'initialization' }
BoundsFinderCanvas >> initializeWithTranslation: aPoint [

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.
]

{ #category : 'testing' }
BoundsFinderCanvas >> isBoundsFinderCanvas [
	^true
]

{ #category : 'private-framework' }
BoundsFinderCanvas >> privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor [

]

{ #category : 'initialization' }
BoundsFinderCanvas >> setForm: aForm subPixelAntiAliasing: aBooleanOrNil [
	"nil means use default kind of anti aliasing"

	engine := VectorEngineBoundsFinder new.
	clippingMorphDisplayBounds := nil.
]

{ #category : 'morphic' }
BoundsFinderCanvas >> updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder [

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.
]

{ #category : 'morphic' }
BoundsFinderCanvas >> updateCurrentBounds [
	"Update display bounds, for currentMorph only."

	currentMorph knowsOwnLocalBounds
		ifTrue: [self displayBoundsSetOnCurrentMorph]
		ifFalse: [
			currentMorph isKnownFailing
				ifTrue: [
					self drawCurrentAsError ]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph drawOn: self.
					self displayBoundsSetOnCurrentMorph.
					(currentMorph postDrawOn: self) ifTrue: [
						self displayBoundsUpdateOnCurrentMorph ]].]
]

{ #category : 'morphic' }
BoundsFinderCanvas >> updateHandsDisplayBounds: aWorldMorph [
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."

	| hand |
	hand := aWorldMorph activeHand.
	hand hasSubmorphs ifTrue: [
		self into: hand.
		hand submorphsDo: [ :morph |
			morph basicDisplayBounds ifNil: [
				self fullUpdateProtrudingBounds: morph ]].
		self outOfMorph ].
]
