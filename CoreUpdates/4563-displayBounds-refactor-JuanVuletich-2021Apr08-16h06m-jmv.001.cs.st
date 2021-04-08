'From Cuis 5.0 [latest update: #4562] on 8 April 2021 at 4:07:46 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 15:14:43'!
adjustDisplayBoundsBy: delta
	"Private for framework use.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	privateDisplayBounds ifNotNil: [
		privateDisplayBounds _ privateDisplayBounds translatedBy: delta ].
	self allMorphsDo: [ :m | m adjustDisplayBoundsBy: delta ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 15:54:04'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw ]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 15:27:31'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c == aCanvas ifTrue: [
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw ]]].! !


!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 15:15:10'!
displayBounds
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us."

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 4/8/2021 15:09:29'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld _ aMorph morphPositionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		formerPositionInWorld ifNotNil: [
			d _ aMorph morphPositionInWorld - formerPositionInWorld.
			d isZero ifFalse: [
				aMorph adjustDisplayBoundsBy: d ]]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		lastPosition _ self morphPosition ].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2021 15:32:07'!
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
	currentMorph displayBoundsSetFrom: self.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2021 15:32:21'!
drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		self
			reverseRectangleBorder: currentMorph morphLocalBounds
			borderWidth: 2.
		currentMorph displayBoundsSetFrom: self ].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/8/2021 15:45:16'!
fullUpdateCurrentBounds
	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph drawOn: self ].
	currentMorph displayBoundsSetFrom: self.
	isKnownFailing ifFalse: [
		currentMorph submorphsDo: [ :m |
			self fullUpdateBounds: m ]]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/8/2021 15:56:00'!
updateCurrentDisplayBounds
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	currentMorph drawOn: self.
	currentMorph displayBoundsSetFrom: self.
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m | self updateDisplayBounds: m ]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2021 15:29:15'!
drawCurrentAndSubmorphs
	
	self isCurrentMorphVisible ifTrue: [

		currentMorph drawOn: self.
		currentMorph displayBoundsSetFrom: self.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		currentMorph postDrawOn: self.
		currentMorph displayBoundsUpdateFrom: self.

		currentMorph isHighlighted ifTrue: [
			self drawCurrentMorphHighlight ].
		world notNil ifTrue: [
			currentMorph halo ifNotNil: [
				Preferences halosShowCoordinateSystem ifTrue: [
					self drawCoordinateSystem: currentMorph morphLocalBounds ]]].

		currentMorph displayBoundsUpdateFrom: self.
	].! !

!methodRemoval: Morph #displayBounds: stamp: 'jmv 4/8/2021 16:07:33'!
Morph removeSelector: #displayBounds:!
