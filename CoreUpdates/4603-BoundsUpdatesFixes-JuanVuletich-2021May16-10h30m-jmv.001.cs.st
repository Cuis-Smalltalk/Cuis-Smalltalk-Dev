'From Cuis 5.0 [latest update: #4601] on 16 May 2021 at 10:56:12 am'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 5/15/2021 22:25:32'!
boundingRectOfCurrentMorphFromLocalBounds
	"In targetForm coordinates.
	Clipped to owner if appropriate."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 5/16/2021 10:54:52'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				privateDisplayBounds _ self isOrthoRectangularMorph 
					ifTrue: [
						"In these cases, #drawOn: might not be able to find bounds (InnerTextMorph, for example),
						but it is OK to compute them from #morphLocalBounds. This is not true for non-orthoRectangular morphs!!"
						aCanvas boundingRectOfCurrentMorphFromLocalBounds]
					ifFalse: [aCanvas boundingRectOfCurrentMorphAfterDraw]]]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/16/2021 10:54:57'!
fullUpdateCurrentBounds

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph isOrthoRectangularMorph ifFalse: [
				"In these cases, #drawOn: is required to find bounds.
				#displayBoundsSetFrom: will use the bounds found by us in these case.
				See #displayBoundsSetFrom:"
				currentMorph drawOn: self.
				currentMorph postDrawOn: self ].
			currentMorph displayBoundsSetFrom: self.
			currentMorph submorphsDo: [ :m |
				self fullUpdateBounds: m ]]! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 5/16/2021 10:41:36'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	BitBltCanvas doesn't keep track of Morph bounds as it draws them. But it is limited to drawing bounds where
	#morphLocalBounds is known. Use it, then."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!methodRemoval: BitBltCanvas #boundingRectOfCurrentMorph stamp: 'jmv 5/16/2021 10:32:43'!
BitBltCanvas removeSelector: #boundingRectOfCurrentMorph!
