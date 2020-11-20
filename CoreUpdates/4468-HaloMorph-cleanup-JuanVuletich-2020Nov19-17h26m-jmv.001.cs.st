'From Cuis 5.0 [latest update: #4466] on 19 November 2020 at 5:27:20 pm'!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox scaleOffset '
	classVariableNames: 'Icons '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 17:26:35'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	target displayBoundsForHalo ifNotNil: [ :r |
		self morphBounds: r.  "update my size"
		haloBox _ self basicBox.
		target addHandlesTo: self box: haloBox.
		self addNameString: (target printStringLimitedTo: 40).
		self redrawNeeded ].! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 17:26:45'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 17:26:50'!
startScale: evt with: scaleHandle
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset _ target scale / (evt eventPosition - target referencePosition) rho.

! !

!methodRemoval: HaloMorph #initialize stamp: 'jmv 11/19/2020 17:26:40'!
HaloMorph removeSelector: #initialize!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset scaleOffset haloBox'
	classVariableNames: 'Icons'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
