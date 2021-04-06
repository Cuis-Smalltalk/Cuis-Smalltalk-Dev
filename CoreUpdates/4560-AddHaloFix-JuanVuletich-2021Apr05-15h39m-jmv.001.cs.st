'From Cuis 5.0 [latest update: #4558] on 5 April 2021 at 3:50:39 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/5/2021 14:52:12'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self morphBounds: aDisplayRectangle.
	haloBox _ self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/5/2021 14:40:52'!
handlesBox
	"handlesBox is in local coordinates.
	We assume we are direct submorph of the world, without any scaling or rotation."

	| minSide hs c e box |
	hs _ Preferences haloHandleSize.
	minSide _ 4 * hs.
	e _ extent + (hs*2) max: minSide@minSide.
	c _ extent // 2 + self morphPosition.
	box _ Rectangle center: c extent: e.
	self world ifNotNil: [ :w | box _ box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	"Make it local"
	^box translatedBy: self morphPosition negated.
! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2021 14:50:49'!
addHalo: aMorphicEventOrNil

	| hand position |
	aMorphicEventOrNil
		ifNil: [
			hand _ self world activeHand.
			hand ifNil: [ hand _ self world firstHand ]. 
			position _ hand lastMouseEvent eventPosition ]
		ifNotNil: [
			hand _ aMorphicEventOrNil hand.
			position _ aMorphicEventOrNil eventPosition ].

	HaloMorph new popUpFor: self handPosition: position hand: hand! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 4/5/2021 15:50:34'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	target isInWorld ifTrue: [							"If not in world, not even bother"
		target displayBoundsForHalo
			ifNotNil: [ :r | self addHandles: r]								"If target has already been drawn and has bounds, go ahead"
			ifNil: [
				self whenUIinSafeState: [							"Otherwise, see if in next cycle target gets drawn and its bounds known"
					target displayBoundsForHalo
						ifNotNil: [ :r | self addHandles: r]]]
	]! !

!methodRemoval: HaloMorph #basicBox stamp: 'jmv 4/5/2021 15:47:16'!
HaloMorph removeSelector: #basicBox!
