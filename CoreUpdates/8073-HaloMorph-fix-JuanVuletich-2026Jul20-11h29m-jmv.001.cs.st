'From Cuis7.9 [latest update: #8072] on 20 July 2026 at 11:36:12 am'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 20/Jul/2026, 2:31:12 pm (UTC)'!
doDrag: evt with: dragHandle
	| thePoint |
	haloDelta ifNil: [ ^self ].
	evt hand obtainHalo: self.
	thePoint := evt eventPosition - positionOffset.
	target positionInWorld: thePoint.
	self positionInWorld: thePoint + haloDelta.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 20/Jul/2026, 2:29:48 pm (UTC)'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians prevLocation deltaRadians |
	angleOffset ifNil: [ ^self ].
	evt hand obtainHalo: self.
	radians := (evt eventPosition - target referencePosition) theta + angleOffset.
	radians := radians detentBy: 0.05 atMultiplesOf: Float pi / 4 snap: false.
	rotHandle color: (radians = 0.0
		ifTrue: [`Color lightBlue`]
		ifFalse: [`Color blue`]).
	rotHandle submorphsDo: [ :m |
		m color: rotHandle color makeForegroundColor].
	prevLocation := target location.
	deltaRadians := radians-prevLocation radians.
	target yAxisPointsUp ifTrue: [ deltaRadians := deltaRadians negated ].
	target location: (prevLocation composedWith: (
		AffineTransformation withRadians: deltaRadians around: target rotationCenter)).
	rotHandle positionInWorld: evt eventPosition - (rotHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 20/Jul/2026, 2:30:11 pm (UTC)'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
	scaleOffset ifNil: [ ^self ].
	evt hand obtainHalo: self.
	scale := (evt eventPosition - target positionInWorld) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	target scale: scale.
	scaleHandle positionInWorld: evt eventPosition - (scaleHandle morphExtent // 2).
	self redrawNeeded.! !

