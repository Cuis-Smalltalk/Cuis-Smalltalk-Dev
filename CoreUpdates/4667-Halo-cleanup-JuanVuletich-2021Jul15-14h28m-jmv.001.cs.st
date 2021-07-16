'From Cuis 5.0 [latest update: #4666] on 15 July 2021 at 2:30:43 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/15/2021 14:30:32'!
doResize: evt with: resizeHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
	evt hand obtainHalo: self.
	newExtent _ (evt eventPosition - target morphPositionInWorld) - positionOffset.
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	resizeHandle morphPositionInWorld: evt eventPosition - (resizeHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/15/2021 14:28:28'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
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

