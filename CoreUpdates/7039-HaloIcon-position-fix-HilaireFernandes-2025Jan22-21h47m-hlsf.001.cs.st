'From Cuis7.3 [latest update: #7036] on 30 January 2025 at 12:04:32 pm'!

!HaloMorph methodsFor: 'private' stamp: 'hlsf 1/30/2025 12:03:55'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint := self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse := Color colorFrom: handleSpec color.
	handle := HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e := (Preferences at: #haloHandleSize) asPoint.
	handle position: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form := self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form := form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 2 ].
				icon := (ImageMorph newWith: form)
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `1@1`.
				handle morphExtent: icon morphExtent+2 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

