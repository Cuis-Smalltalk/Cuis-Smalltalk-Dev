"
This morph provides a halo of handles for its target morph. Dragging, duplicating, rotating, and resizing to be done by mousing down on the appropriate handle. There are also handles for help and for a menu of infrequently used operations.
"
Class {
	#name : 'HaloMorph',
	#superclass : 'BoxMorph',
	#instVars : [
		'target',
		'positionOffset',
		'angleOffset',
		'scaleOffset',
		'haloBox',
		'haloDelta'
	],
	#classVars : [
		'Icons'
	],
	#category : 'Morphic-Halos',
	#package : 'Morphic',
	#tag : 'Halos'
}

{ #category : 'accessing - icons' }
HaloMorph class >> haloCollapseIcon [

	^ Theme current haloCollapseIcon
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloColorIcon [

	^ self icons
		at: #haloColorIcon
		ifAbsentPut: [ Theme current haloColorIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloDebugIcon [

	^ self icons
		at: #haloDebugIcon
		ifAbsentPut: [ Theme current haloDebugIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloDismissIcon [

	^ self icons
		at: #haloDismissIcon
		ifAbsentPut: [ Theme current haloDismissIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloDragIcon [

	^ self icons
		at: #haloDragIcon
		ifAbsentPut: [ Theme current haloDragIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloDuplicateIcon [

	^ self icons
		at: #haloDuplicateIcon
		ifAbsentPut: [ Theme current haloDuplicateIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloFontEmphasisIcon [

	^ self icons
		at: #haloFontEmphasisIcon
		ifAbsentPut: [ Theme current haloFontEmphasisIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloFontSizeIcon [

	^ self icons
		at: #haloFontSizeIcon
		ifAbsentPut: [ Theme current haloFontSizeIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloGrabIcon [

	^ self icons
		at: #haloGrabIcon
		ifAbsentPut: [ Theme current haloGrabIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloHelpIcon [

	^ self icons
		at: #haloHelpIcon
		ifAbsentPut: [ Theme current haloHelpIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloMenuIcon [

	^ self icons
		at: #haloMenuIcon
		ifAbsentPut: [ Theme current haloMenuIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloResizeIcon [

	^ self icons
		at: #haloResizeIcon
		ifAbsentPut: [ Theme current haloResizeIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloRotateIcon [

	^ self icons
		at: #haloRotateIcon
		ifAbsentPut: [ Theme current haloRotateIcon ]
]

{ #category : 'accessing - icons' }
HaloMorph class >> haloScaleIcon [

	^ self icons
		at: #haloScaleIcon
		ifAbsentPut: [ Theme current haloScaleIcon ]
]

{ #category : 'cached state access' }
HaloMorph class >> icons [
	Icons ifNil: [
		Icons := Dictionary new ].
	^ Icons
]

{ #category : 'new-morph participation' }
HaloMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'cached state access' }
HaloMorph class >> releaseClassCachedState [

	Icons := nil
]

{ #category : 'handles' }
HaloMorph >> addCollapseHandle: handleSpec [
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	target owner
		ifNil: [ ^self ]	"nil happens, amazingly"
		ifNotNil: [ :to |
			(to isWorldMorph or: [ to is: #HandMorph ])
				ifFalse: [ ^self ]].
		
	(self addHandle: handleSpec)
		mouseDownSelector: #mouseDownInCollapseHandle:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeCollapse:with:
]

{ #category : 'handles' }
HaloMorph >> addDebugHandle: handleSpec [

	(Preferences at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]

]

{ #category : 'handles' }
HaloMorph >> addDismissHandle: handleSpec [

	(self addHandle: handleSpec)
		mouseDownSelector: #setDismissColor:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeDismiss:with:
]

{ #category : 'handles' }
HaloMorph >> addDragHandle: haloSpec [

	(self addHandle: haloSpec)
		mouseDownSelector: #startDrag:with:;
		mouseMoveSelector: #doDrag:with:
]

{ #category : 'handles' }
HaloMorph >> addDupHandle: haloSpec [
	(self addHandle: haloSpec) mouseDownSelector:#doDup:with:
]

{ #category : 'handles' }
HaloMorph >> addExploreHandle: handleSpec [

	(Preferences at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doExplore:with: ]

]

{ #category : 'handles' }
HaloMorph >> addGrabHandle: haloSpec [

	(self addHandle: haloSpec)
		mouseDownSelector: #doGrab:with:
]

{ #category : 'private' }
HaloMorph >> addHandle: handleSpec [
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
					": Non default size, scale that bugger!"
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
	^ handle
]

{ #category : 'private' }
HaloMorph >> addHandles [

	self removeAllMorphs.  "remove old handles, if any"
	target isInWorld ifTrue: [							"If not in world, not even bother"
		target displayBoundsForHalo
			ifNotNil: [ :r | self addHandles: r]								"If target has already been drawn and has bounds, go ahead"
			ifNil: [
				self whenUIinSafeState: [							"Otherwise, see if in next cycle target gets drawn and its bounds known"
					target displayBoundsForHalo
						ifNotNil: [ :r | self addHandles: r]]]
	]
]

{ #category : 'private' }
HaloMorph >> addHandles: aDisplayRectangle [
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self position: aDisplayRectangle topLeft extent: aDisplayRectangle extent.
	haloBox := self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: target shortPrintString.
	self redrawNeeded.
]

{ #category : 'handles' }
HaloMorph >> addHelpHandle: haloSpec [
	(self addHandle: haloSpec)
		mouseDownSelector: #mouseDownOnHelpHandle:;
		mouseMoveSelector: #deleteBalloon
]

{ #category : 'handles' }
HaloMorph >> addMenuHandle: haloSpec [

	(self addHandle: haloSpec) mouseDownSelector: #doMenu:with:
]

{ #category : 'private' }
HaloMorph >> addNameString: aString  [
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground := ColoredBoxMorph new
		color: ((target is: #SystemWindow)
			ifTrue: [target windowColor]
			ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph := LabelMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition := haloBox bottom + (Preferences at: #haloHandleSize).
	namePosition := haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph
]

{ #category : 'handles' }
HaloMorph >> addRecolorHandle: haloSpec [
	"Add a recolor handle to the receiver, if appropriate"

	(self addHandle: haloSpec) mouseUpSelector: #doRecolor:with:
]

{ #category : 'handles' }
HaloMorph >> addResizeHandle: haloSpec [

	target okayToResizeEasily ifTrue: [
		^(self addHandle: haloSpec)
			mouseDownSelector: #startResize:with:;
			mouseMoveSelector: #doResize:with: ].

	target okayToScaleEasily ifTrue: [
		^(self addHandle: haloSpec)
			mouseDownSelector: #startScale:with:;
			mouseMoveSelector: #doScale:with: ].
]

{ #category : 'handles' }
HaloMorph >> addRotateHandle: haloSpec [

	(self addHandle: haloSpec)
		mouseDownSelector: #startRot:with:;
		mouseMoveSelector: #doRot:with:
]

{ #category : 'handles' }
HaloMorph >> addScaleHandle: haloSpec [

	(self addHandle: haloSpec)
		mouseDownSelector: #startScale:with:;
		mouseMoveSelector: #doScale:with:
]

{ #category : 'initialization' }
HaloMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.6
		g: 0.8
		b: 1.0`
]

{ #category : 'accessing' }
HaloMorph >> defaultLayerNumber [
	^ 20
]

{ #category : 'initialization' }
HaloMorph >> delete [

	super delete.
	target ifNotNil: [ target redrawNeeded ].
]

{ #category : 'forward to target' }
HaloMorph >> deleteBalloon [
	target deleteBalloon
]

{ #category : 'private' }
HaloMorph >> doDebug: evt with: menuHandle [
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.
	evt shiftPressed ifTrue: [
		^ target inspect].

	menu := target buildDebugMenu: evt hand.
	menu addTitle: target shortPrintString.
	menu popUpInWorld: self world
]

{ #category : 'private' }
HaloMorph >> doDrag: evt with: dragHandle [
	| thePoint |
	evt hand obtainHalo: self.
	thePoint := evt eventPosition - positionOffset.
	target positionInWorld: thePoint.
	self positionInWorld: thePoint + haloDelta.

]

{ #category : 'private' }
HaloMorph >> doDup: evt with: dupHandle  [
	"Ask hand to duplicate my target."

	| hand positionInHandCoordinates |
	target okayToDuplicate ifFalse: [^ self].
	hand := evt hand.
	positionInHandCoordinates := target positionInWorld - hand positionInWorld.
	"Duplicate has no meaningful position, as it is not in the world. Grab position from original!"
	target := target duplicateMorph: evt.
	self addNameString: target shortPrintString.	
	hand
		obtainHalo: self;
		grabMorph: target delta: positionInHandCoordinates
]

{ #category : 'private' }
HaloMorph >> doExplore: evt with: aHandle [

	evt hand obtainHalo: self.	
	^ target explore
]

{ #category : 'private' }
HaloMorph >> doGrab: evt with: grabHandle [
	"Ask hand to grab my target."

	evt hand
		obtainHalo: self;
		grabMorph: target
]

{ #category : 'private' }
HaloMorph >> doMenu: evt with: menuHandle [
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	evt hand obtainHalo: self.	
	menu := target buildHandleMenu: evt hand.
	target addTitleForHaloMenu: menu.
	menu popUpInWorld: self world.

]

{ #category : 'private' }
HaloMorph >> doRecolor: event with: aHandle [
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: event]
		ifTrue: [
			target changeColor]
]

{ #category : 'private' }
HaloMorph >> doResize: evt with: resizeHandle [
	"Called while the mouse is down in the grow handle"

	| newExtent |
	evt hand obtainHalo: self.
	newExtent := (evt eventPosition - target positionInWorld) - positionOffset.
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target extentInWorld: newExtent.
	resizeHandle positionInWorld: evt eventPosition - (resizeHandle morphExtent // 2).	
	self redrawNeeded.
]

{ #category : 'private' }
HaloMorph >> doRot: evt with: rotHandle [
	"Update the rotation of my target if it is rotatable."

	| radians prevLocation deltaRadians |
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
	self redrawNeeded.
]

{ #category : 'private' }
HaloMorph >> doScale: evt with: scaleHandle [
	"Update the scale of my target if it is scalable."

	| scale |
	evt hand obtainHalo: self.
	scale := (evt eventPosition - target positionInWorld) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	target scale: scale.
	scaleHandle positionInWorld: evt eventPosition - (scaleHandle morphExtent // 2).
	self redrawNeeded.
]

{ #category : 'drawing' }
HaloMorph >> drawCoordinateSystemOn: aCanvas [

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx font strokeWidth tickLength stepXDecimals stepYDecimals |
	haloTargetTx := MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx := haloTargetTx composedWith: o location ].
	haloTargetTx := haloTargetTx composedWith: target location.

	target knowsOwnLocalBounds
		ifTrue: [ | r |
			r := target localBounds.
			x0 := r left.
			x1 := r right.
			y0 := r top.
			y1 := r bottom ]
		ifFalse: [
			x0 := x1 := y0 := y1 := 0.
			target displayFullBounds corners collect: [ :pt | | p |
				p := haloTargetTx inverseTransform: pt.
				x0 := x0 min: p x.
				x1 := x1 max: p x.
				y0 := y0 min: p y.
				y1 := y1 max: p y.]].

	font := FontFamily defaultFamilyPointSize: ((FontFamily defaultPointSize * 1.5 / haloTargetTx scale) rounded max: 0.1).
	stepX := (font pointSize * 9) round4perMagnitudeOrder asFloat.
	stepXDecimals := stepX log rounded negated + 1.
	stepY := (font pointSize * 5) round4perMagnitudeOrder asFloat.
	stepYDecimals := stepY log rounded negated + 1.
	strokeWidth := 3/ haloTargetTx scale.
	tickLength := 5 / haloTargetTx scale.

	prevTx := aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c := `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: strokeWidth color: c.
	aCanvas line: 0@y0 to: 0@y1 width: strokeWidth color: c.

	(x0 truncateTo: stepX) to: x1 by: stepX do: [ :x |
		aCanvas line: x @ tickLength negated to: x @ tickLength width: strokeWidth color: c.
		aCanvas drawString: (x printStringFractionDigits: stepXDecimals) atWaistCenter: x @ (tickLength*4) negated font: font color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - (tickLength*3) @ 0 font: font color: c.

	(y0 truncateTo: stepY) to: y1 by: stepY do: [ :y |
		aCanvas line: tickLength negated @ y to: tickLength @ y width: strokeWidth color: c.
		aCanvas drawString: (y printStringFractionDigits: stepYDecimals), ' ' atWaistRight: tickLength negated @ y font: font color: c ].
	  aCanvas drawString: 'y' atWaist: tickLength @ (y1 - (tickLength*4)) font: font color: c.

	aCanvas geometryTransformation: prevTx.
]

{ #category : 'drawing' }
HaloMorph >> drawOn: aCanvas  [

	target ifNil: [^super drawOn: aCanvas].
	
	"For debugging halos' morph bounds visually."
	
	"Fill the bounds of halo's morph. To activate:
	Preferences at: #halosMorphBoundsFillColor put: (Color blue alpha: 0.2)
	"
	(Preferences at: #halosMorphBoundsFillColor) ifNotNil: [:fillColor |
		aCanvas
			fillRectangle: self localBounds
			color: fillColor].
	
	"Draw a frame over halos morph bounds. To activate:
	Preferences at: #halosMorphBoundsFrameColor put: Color blue.
	"
	(Preferences at: #halosMorphBoundsFrameColor) ifNotNil: [:frameColor |
		aCanvas frameRectangle: self localBounds 
			borderWidth: 1 
			color: frameColor].
		
	target haloShowsCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas privateFrameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			target displayBounds ifNotNil: [ :b |
				b = fb ifFalse: [
					aCanvas privateFrameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
				self drawCoordinateSystemOn: aCanvas ]]].
]

{ #category : 'private' }
HaloMorph >> endInteraction [
	"Clean up after a user interaction with the a halo control"

	(target isInWorld not or: [owner isNil]) ifTrue: [^self].
	self isInWorld 
		ifTrue: [
			"make sure handles show in front"
			self comeToFront.
			self addHandles]
]

{ #category : 'private' }
HaloMorph >> handlesBox [
	"handlesBox is in local coordinates.
	We assume we are direct submorph of the world, without any scaling or rotation."

	| minSide hs c e box |
	hs := Preferences at: #haloHandleSize.
	minSide := 4 * hs.
	e := extent + (hs*2) max: minSide@minSide.
	c := extent // 2 + self morphPosition.
	box := Rectangle center: c extent: e.
	self world ifNotNil: [ :w | box := box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	"Make it local"
	^box translatedBy: self morphPosition negated.

]

{ #category : 'event handling testing' }
HaloMorph >> handlesMouseDown: aMouseButtonEvent [

	^aMouseButtonEvent mouseButton3Pressed
]

{ #category : 'testing' }
HaloMorph >> is: aSymbol [
	^ aSymbol == #HaloMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
HaloMorph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false
]

{ #category : 'geometry testing' }
HaloMorph >> knowsOwnLocalBounds [
	"HaloMorph is a bit special because of the drawing of the coordinate systems."

	^false
]

{ #category : 'private' }
HaloMorph >> maybeCollapse: event with: aHandle  [
	"Ask hand to collapse my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifTrue: [
			target collapse ].
	self delete.
]

{ #category : 'private' }
HaloMorph >> maybeDismiss: event with: aHandle [
	"Ask hand to dismiss my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle includesPixel: event eventPosition)
		ifFalse: [
			self delete.
			target addHalo: event]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]
]

{ #category : 'events' }
HaloMorph >> mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition [
	"Transfer the halo to the next likely recipient"
	target ifNil: [
		self delete.
		^self].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset := aMouseButtonEvent eventPosition - target positionInWorld.
	"wait for click to transfer halo"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil
]

{ #category : 'private' }
HaloMorph >> mouseDownInCollapseHandle: evt with: collapseHandle [
	"The mouse went down in the collapse handle; collapse the morph"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setDismissColor: evt with: collapseHandle
]

{ #category : 'forward to target' }
HaloMorph >> mouseDownOnHelpHandle: anEvent [
	target mouseDownOnHelpHandle: anEvent
]

{ #category : 'event handling' }
HaloMorph >> popUpFor: aMorph handPosition: handPosition hand: hand [

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset := handPosition - aMorph positionInWorld.
]

{ #category : 'handles' }
HaloMorph >> positionIn: aBox horizontalPlacement: horiz verticalPlacement: vert [
	| xCoord yCoord |

	horiz == #left
		ifTrue:	[xCoord := aBox left].
	horiz == #leftCenter
		ifTrue:	[xCoord := aBox left + (aBox width // 4)].
	horiz == #center
		ifTrue:	[xCoord := (aBox left + aBox right) // 2].
	horiz == #rightCenter
		ifTrue:	[xCoord := aBox left + ((3 * aBox width) // 4)].
	horiz == #right
		ifTrue:	[xCoord := aBox right].

	vert == #top
		ifTrue:	[yCoord := aBox top].
	vert == #topCenter
		ifTrue:	[yCoord := aBox top + (aBox height // 4)].
	vert == #center
		ifTrue:	[yCoord := (aBox top + aBox bottom) // 2].
	vert == #bottomCenter
		ifTrue:	[yCoord := aBox top + ((3 * aBox height) // 4)].
	vert == #bottom
		ifTrue:	[yCoord := aBox bottom].

	^ xCoord asInteger @ yCoord asInteger
]

{ #category : 'events-processing' }
HaloMorph >> rejectsEvent: aMorphicEvent [
	"Return true to reject the given event. Submorphs might still handle it."

	"Only mouse button down events are handled by the halo itself"
	(aMorphicEvent isMouse and: [ aMorphicEvent isMouseDown ])
	ifTrue: [
		^ false ].
	^true
]

{ #category : 'private' }
HaloMorph >> removeAllHandlesBut: h [
	"Remove all handles except h."
	submorphs copy do:
		[:m | m == h ifFalse: [m delete]].

]

{ #category : 'geometry testing' }
HaloMorph >> requiresVectorCanvas [
	"Prefer VectorGraphics halos and handled for morphs drawn with VectorCanvas."

	target ifNotNil: [
		^target requiresVectorCanvas ].
	^false
]

{ #category : 'private' }
HaloMorph >> setDismissColor: event with: aHandle [
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	event hand obtainHalo: self.
	colorToUse :=  (aHandle includesPixel: event eventPosition)
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	aHandle color: colorToUse
]

{ #category : 'dropping/grabbing' }
HaloMorph >> startDrag: evt with: dragHandle [
	"Drag my target without removing it from its owner."

	| p |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	p := target positionInWorld.
	positionOffset := dragHandle referencePosition - p.
	haloDelta := self positionInWorld - p.

]

{ #category : 'private' }
HaloMorph >> startResize: evt with: resizeHandle [
	"Initialize resizing of my target."

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: resizeHandle.  "remove all other handles"
	botRt := target positionInWorld + target extentInWorld.
	positionOffset := (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]
]

{ #category : 'private' }
HaloMorph >> startRot: evt with: rotHandle [
	"Initialize rotation of my target if it is rotatable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset := target rotation - (evt eventPosition - target referencePosition) theta.
]

{ #category : 'private' }
HaloMorph >> startScale: evt with: scaleHandle [
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset := target scale / (evt eventPosition - target positionInWorld) rho.
]

{ #category : 'event handling' }
HaloMorph >> staysUpWhenMouseIsDownIn: aMorph [
	^ ((aMorph == target) or: [aMorph hasOwner: self])
]

{ #category : 'stepping' }
HaloMorph >> step [
	(target isNil or: [target isInWorld not]) ifTrue: [self delete]
]

{ #category : 'stepping' }
HaloMorph >> stepTime [
	^ 100
]

{ #category : 'geometry testing' }
HaloMorph >> submorphsMightProtrude [
	"Handles and label are usually outside our bounds."

	^true
]

{ #category : 'accessing' }
HaloMorph >> target [

	^ target

]

{ #category : 'accessing' }
HaloMorph >> target: aMorph [

	target := aMorph.
	target ifNotNil: [self addHandles].

]

{ #category : 'event handling' }
HaloMorph >> transferHalo: event localPosition: localEventPosition [
	"Transfer the halo to the next likely recipient"
	target ifNil: [
		self delete.
		^ self ].
	target transferHalo: event from: target.
]

{ #category : 'testing' }
HaloMorph >> wantsHalo [
	^false
]

{ #category : 'stepping' }
HaloMorph >> wantsSteps [
	^ true
]
