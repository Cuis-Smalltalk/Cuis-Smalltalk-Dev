'From Cuis 5.0 [latest update: #4733] on 4 August 2021 at 7:06:13 pm'!

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:46:45'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifFalse: [
		location _ location withTranslation: aPoint.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:53:36'!
morphPosition: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self morphExtent: (self internalizeDistance: newExtent).! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/4/2021 19:05:09'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	| b |
	b _ aDisplayRectangle outsetBy: 30@30.
	self morphPosition: b topLeft extent: b extent.
	haloBox _ self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	self redrawNeeded.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:53:31'!
morphPosition: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self morphExtent: (self internalizeDistance: newExtent).! !

!methodRemoval: WidgetMorph #morphBounds: stamp: 'jmv 8/4/2021 19:06:02'!
WidgetMorph removeSelector: #morphBounds:!
!methodRemoval: KernelMorph #morphBounds: stamp: 'jmv 8/4/2021 19:06:03'!
KernelMorph removeSelector: #morphBounds:!
