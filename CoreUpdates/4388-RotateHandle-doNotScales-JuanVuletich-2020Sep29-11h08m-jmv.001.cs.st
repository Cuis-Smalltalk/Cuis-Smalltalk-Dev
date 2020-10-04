'From Cuis 5.0 [latest update: #4388] on 29 September 2020 at 11:08:32 am'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/29/2020 11:08:03'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| radians scale |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta.
	radians _ radians - angleOffset theta.
	"degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false."
	radians = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	scale _ (evt eventPosition - target referencePosition) rho * scaleOffset.
	self removeAllHandlesBut: rotHandle.
	target rotation: radians scale: "scale" 1.									"Please use another handle for scale!!"
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !

