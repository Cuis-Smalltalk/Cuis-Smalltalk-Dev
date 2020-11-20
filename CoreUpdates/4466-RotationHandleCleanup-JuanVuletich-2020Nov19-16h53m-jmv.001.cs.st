'From Cuis 5.0 [latest update: #4464] on 19 November 2020 at 4:57:02 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/18/2020 16:41:51'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians |
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta.
	radians _ radians - angleOffset theta.
	"degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false."
	radians = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.
	target rotation: radians.
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/18/2020 16:45:01'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees.! !

