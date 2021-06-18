'From Cuis 5.0 [latest update: #4639] on 18 June 2021 at 3:42:08 pm'!

!Form methodsFor: 'displaying-generic' stamp: 'jmv 6/18/2021 15:21:59'!
displayAt: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display at: aDisplayPoint.! !


!Morph methodsFor: 'geometry' stamp: 'jmv 6/18/2021 14:48:15'!
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
					ifFalse: [
						"In this cases, we use the bounds bound by #drawOn. This method must therefore be called after calling
						#drawOn: and #postDrawOn: . See senders."
						aCanvas boundingRectOfCurrentMorphAfterDraw]]]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/18/2021 14:49:16'!
fullUpdateBounds: aMorph
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self canvasToUse fullUpdateCurrentBounds.
		self outOfMorph ].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/18/2021 14:49:49'!
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
				self fullUpdateBounds: m ]].! !

