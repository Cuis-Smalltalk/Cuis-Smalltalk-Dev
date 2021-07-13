'From Cuis 5.0 [latest update: #4661] on 13 July 2021 at 11:16:45 am'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/13/2021 11:16:25'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
"MUST fix resize / rescale / rotation of embedded morphs!!!!!!"
	evt hand obtainHalo: self.
	scale _ (evt eventPosition - target morphPositionInWorld) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	target scale: scale.
	scaleHandle morphPositionInWorld: evt eventPosition - (scaleHandle morphExtent // 2).
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/13/2021 11:16:31'!
startScale: evt with: scaleHandle
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset _ target scale / (evt eventPosition - target morphPositionInWorld) rho.! !

