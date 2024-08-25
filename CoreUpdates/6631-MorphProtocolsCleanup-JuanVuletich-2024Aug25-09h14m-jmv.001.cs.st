'From Cuis7.1 [latest update: #6630] on 25 August 2024 at 9:15:43 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:58:42'!
localBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^`0@0 corner: 75@70`.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:56:32'!
positionInWorld
	"Answer is in world coordinates."

	^ self externalizeToWorld: `0@0`! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:55:00'!
positionInWorld: newPositionInWorld
	"Change the position of this morph. Argument is in world coordinates.
	Ignored by morphs that are not movable."! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:55:07'!
positionInWorld: newPositionInWorld
	"Change the position of this morph.
	Argument is in world coordinates."

	| newPositionInOwner |
	newPositionInOwner := owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	location := location withTranslation: newPositionInOwner.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:59:06'!
localBounds

	^`0@0` extent: extent.! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:59:22'!
localBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^self localBounds.! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:54:16'!
widthInOwner: newWidth

	self morphExtentInOwner: newWidth @ self morphExtentInOwner y! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:56:36'!
positionInWorld
	"Answer is in world coordinates."

	^ `0@0`! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/25/2024 08:55:34'!
rejectDropMorphEvent: dropEvent
	"The receiver has been rejected, and must be put back somewhere.
	 If the original owner and position are known, use them, 
	else Just keep it in the hand"

	((dropEvent formerOwner isNil) or: [ dropEvent formerPosition isNil ])
		ifTrue: [
			dropEvent hand grabMorph: self.
		]
		ifFalse: [
			dropEvent formerOwner addMorph: self.
				   self positionInWorld: dropEvent formerPosition.
		]
		! !


!PlacedMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:56:41'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner := owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition := self positionInWorld ].
	owner := aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location := location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent := owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]].
	self fixYAxisDirection.! !


!BoxedMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 08:59:15'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:59:18'!
fullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	Note: Answers an integer rectangle"

	"Rethoric question. If it is ever true, call super."
	"(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifFalse: ["
	^ (self externalizeBoundingRectOf: self localBounds) encompassingIntegerRectangle! !

!BoxedMorph methodsFor: 'geometry services' stamp: 'jmv 8/25/2024 08:59:11'!
coversLocalPoint: aLocalPoint
	"Answer true as long as aLocalPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it."

	"If not visible, won't cover any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self localBounds containsPoint: aLocalPoint! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 08:59:26'!
drawOn: aCanvas

	Theme current roundWindowCorners
		ifTrue: [
			aCanvas
				roundRect: self localBounds
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ]! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 08:59:30'!
drawOn: aCanvas

	| r |
	r := self localBounds.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		drawTextComposition: textComposition
		at: `4@4`
		extent: extent - 8
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false).! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 08:59:36'!
drawOn: aCanvas
	aCanvas
		fillRectangle: self localBounds
		color: color.
	Transcript displayTextOn: aCanvas in: self localBounds.
	Transcript bounds: (self externalizeBoundsToWorld: self localBounds)
				encompassingIntegerRectangle.
	self updateWorkspace! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 08:59:40'!
drawCoordinateSystemOn: aCanvas

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

	font := FontFamily defaultFamilyPointSize: FontFamily defaultPointSize * 1.5 / haloTargetTx scale.
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

	aCanvas geometryTransformation: prevTx.! !

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:57'!
drawOn: aCanvas 

	| b |
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
			b := target displayBounds.
			b = fb ifFalse: [
				aCanvas privateFrameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
			self drawCoordinateSystemOn: aCanvas ]].! !

!HaloMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/25/2024 08:57:07'!
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	| p |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	p := target positionInWorld.
	positionOffset := dragHandle referencePosition - p.
	haloDelta := self positionInWorld - p.
! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 8/25/2024 08:56:59'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
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
		dblClkSel: nil! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 8/25/2024 08:57:02'!
popUpFor: aMorph handPosition: handPosition hand: hand

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset := handPosition - aMorph positionInWorld.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:55:42'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint := evt eventPosition - positionOffset.
	target positionInWorld: thePoint.
	self positionInWorld: thePoint + haloDelta.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:56:47'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	| hand positionInHandCoordinates |
	target okayToDuplicate ifFalse: [^ self].
	hand := evt hand.
	positionInHandCoordinates := target positionInWorld - hand positionInWorld.
	"Duplicate has no meaningful position, as it is not in the world. Grab position from original!!"
	target := target duplicateMorph: evt.
	self addNameString: target shortPrintString.	
	hand
		obtainHalo: self;
		grabMorph: target delta: positionInHandCoordinates! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:56:51'!
doResize: evt with: resizeHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
	evt hand obtainHalo: self.
	newExtent := (evt eventPosition - target positionInWorld) - positionOffset.
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	resizeHandle positionInWorld: evt eventPosition - (resizeHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:55:54'!
doRot: evt with: rotHandle
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
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:56:55'!
doScale: evt with: scaleHandle
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
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:57:10'!
startResize: evt with: resizeHandle
	"Initialize resizing of my target."

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: resizeHandle.  "remove all other handles"
	botRt := target positionInWorld + target morphExtentInWorld.
	positionOffset := (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:57:14'!
startScale: evt with: scaleHandle
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset := target scale / (evt eventPosition - target positionInWorld) rho.! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/25/2024 08:58:00'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h positionInWorld - self positionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner := aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld := aMorph positionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		d := formerPositionInWorld ifNotNil: [aMorph positionInWorld - formerPositionInWorld].
		(d isNil or: [d isZero not]) ifTrue: [
			aMorph adjustDisplayBoundsBy: d ]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition := self morphPosition ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/25/2024 08:57:28'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed := aMorph.
	aMorph owner ifNotNil: [ :o | grabbed := o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed := grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds := nil.
		grabbed isInWorld ifTrue: [
			bounds := (grabbed displayBounds ifNil: [grabbed fullBoundsInOwner ])
				translatedBy: grabbed positionInWorld negated ].
		bounds ifNil: [ bounds := grabbed fullBoundsInOwner ].
		positionInHandCoordinates := (bounds center + bounds bottomRight //2) negated + 
			grabbed morphPosition.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates := (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		positionInWorld - self positionInWorld.

	tx := GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx := o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx := o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !

!HandMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 08:57:39'!
rememberGrabMorphDataFor: aMorph

	grabMorphData
		at: aMorph 
		put: { aMorph owner. aMorph positionInWorld }! !


!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 8/25/2024 08:59:48'!
invalidateDisplayRect: damageRect for: aMorph
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

	self recordDamagedRect: (damageRect intersect: self localBounds ) for: aMorph! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 08:59:58'!
viewBox

	^ self localBounds! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/25/2024 08:59:55'!
mainLoop

	| morphicCanvas morphicDamageRecorder delay |
	self morphPosition: `0@0` extent: Display extent.

        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	(Display deferUpdates: true)
		ifNotNil: [
			"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
			morphicCanvas := Display getMainCanvas.
			"But then, they need to flush updates periodically."
			DisplayScreen installScreenUpdater ]
		ifNil: [
			"Mac (at least intel) doesn't support deferUpdates:, so Morphic Canvas target is this aux Form."
			morphicCanvas := (Form extent: extent depth: Display depth) getMainCanvas ].

	morphicCanvas ownerProcess: Processor activeProcess.
	morphicCanvas world: self.
	morphicDamageRecorder := DamageRecorder new.
	"Set this as the main canvas"
	canvas := morphicCanvas.
	damageRecorder := morphicDamageRecorder.
	damageRecorder recordInvalidRect: self localBounds for: nil.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	delay := Delay forMilliseconds: 50.
	[
		self displayWorldOn: morphicCanvas.
		[
			"Set the main canvas on every iteration.
			This is needed if UI processes are interrupted, resumed, etc."
			canvas := morphicCanvas.
			damageRecorder := morphicDamageRecorder.
			self doOneCycleOn: morphicCanvas delay: delay.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				context  selector = #drawOn: ])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/25/2024 08:56:02'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect := Rectangle center: aPoint extent: aMorph morphExtent.
	delta := trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph positionInWorld: (trialRect origin + delta) rounded.! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:02'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self localBounds
		color: color! !


!WindowEdgeAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:12'!
drawOn: aCanvas

	| c |
	(owner is: #SystemWindow) ifFalse: [
		^super drawOn: aCanvas ].

	"Use almost transparent, so effect on antialiasing for drawing exactly on top of Window is kept to a minimum."
	c := owner windowFrameColor alpha: 0.1.
	selector caseOf: {
		[ #windowTopLeft: ] -> [
			aCanvas roundTopLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowTopRight: ] -> [
			aCanvas roundTopRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomLeft: ] -> [
			aCanvas roundBottomLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomRight: ] -> [
			aCanvas roundBottomRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."	
		aCanvas roundEdge: self localBounds border: owner borderWidth color: c ].! !

!WindowEdgeAdjustingMorph methodsFor: 'geometry services' stamp: 'jmv 8/25/2024 09:00:06'!
coversLocalPoint: aLocalPoint
	"We don't completely cover our bounds. Account for that."

	| sensitiveBorder |
	sensitiveBorder := owner borderWidth.
	((self localBounds outsetBy: sensitiveBorder) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/25/2024 09:00:16'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self localBounds insetBy: borderWidth! !


!StringRequestMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:26'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self localBounds color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self localBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 8/25/2024 08:57:44'!
initialize
	super initialize.
	viewBox := LayoutMorph newRow color: self defaultColor.
	self
		addMorph: (clock := UpdatingLabelMorph initializedInstance)
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ].
	clock
		setProperty: #handlesMouseDown: toValue: true;
		setProperty: #'mouseButton1Down:localPosition:' 
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self positionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu morphExtentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:32'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self localBounds color: color radius: Theme current roundedWindowRadius * 5//6 ]
		ifFalse: [
			aCanvas fillRectangle: self localBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:39'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds := self drawBoundsForRow: row.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	c := (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:43'!
drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds := self drawBoundsForRow: highlightedRow.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: ((Theme current listHighlightFocused: owner hasKeyboardFocus) alpha: 0.3)! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:46'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds := self drawBoundsForRow: selectedRow.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'anchors' stamp: 'jmv 8/25/2024 08:57:51'!
anchorMorph: aMorph at: aPoint
	| relPt |
	aMorph owner == self ifTrue: [ self removeMorph: aMorph ].
	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Texts' ].
	self addMorphFront: aMorph.
	relPt := aPoint - self positionInWorld.
	editor insertMorph: aMorph at: relPt.
	self fit.! !

!InnerTextMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/25/2024 08:57:48'!
addMorphFrontFromWorldPosition: aMorph
	"Overridden for more specific re-layout and positioning"
	| positionInWorld |
	positionInWorld := aMorph positionInWorld.
	^self anchorMorph: aMorph at: positionInWorld! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:00:50'!
drawOn: aCanvas 
	| stringColor leftEdge magnifiedIcon |

	stringColor := color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self localBounds color: Theme current menuHighlight].
	leftEdge := 0.

	self hasMarker ifTrue: [
		leftEdge := leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		magnifiedIcon := self magnifiedIcon.
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge := magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - self class subMenuMarker width @ (extent y - self class subMenuMarker height // 2) ]! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:02'!
drawOn: aCanvas

	| x colorToUse centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self localBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].

	isSelected ifTrue: [
		aCanvas
			fillRectangle: self localBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	x := indentLevel * self indentWidth.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2) height: self indentWidth]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) height: self indentWidth ]].
	x := self indentWidth * 3 // 2 + x.

	icon isNil ifFalse: [
		centeringOffset := ((extent y - icon height) / 2.0) roundedHAFZ.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x := self indentWidth // 2 + icon width + x. ].

	colorToUse := complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:06'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol := self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c := color.
	self mouseIsOver | self isPressed ifTrue: [
		c := mouseOverColor ifNil: [ c lighter ]].
	c := self enableColorWith: c.
	aCanvas
		fillRectangle: self localBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: (borderColor ifNil: [ c ]).

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:09'!
roundButtonRect
	^self localBounds insetBy: 3! !


!DraggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:12'!
roundButtonRect
	"Use full bounds for drawing.
	See #computeSlider and #expandSlider"
	^self localBounds! !


!ScrollBar methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:15'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self localBounds
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:18'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners."

	aCanvas
		fillRectangle: (self localBounds insetBy: 1.5)
		color: color
		borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self borderColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas
		fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight)
		color: windowFrameColor.! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:21'!
drawRoundedFrameOn: aCanvas color: windowFrameColor

	aCanvas
		drawRoundedFrame: self localBounds
		border: borderWidth
		color: self borderColor
		topColor: windowFrameColor
		insideColor: color
		labelHeight: self labelHeight.! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 08:58:04'!
browseHierarchyAndReplaceMe
	"Create and schedule a new hierarchy browser on the currently selected class or meta.
	Resize and position it over me and dismiss me."

	model hierarchyBrowser ifNotNil: [ :newBrowser | | newBrowserWindow |
		newBrowserWindow := HierarchyBrowserWindow
			openNoSysCat: newBrowser
			label: newBrowser labelString.
		newBrowser assureSelectionsShow.
		newBrowserWindow
			positionInWorld: self positionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 08:58:08'!
browseMethodFullAndReplaceMe
	"Create and schedule a full Browser and then select the current class and message.
	Resize and position it over me and dismiss me."

	model selectedClassOrMetaClass ifNotNil: [ :myClass | | newBrowserWindow |
		newBrowserWindow := BrowserWindow
			fullOnClass: myClass 
			selector: model selectedMessageName.
		newBrowserWindow
			positionInWorld: self positionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 08:58:11'!
methodInheritanceAndReplaceMe
	"Create and schedule a method browser on the inheritance of implementors.
	Resize and position it over me and dismiss me."

	| newBrowser |
	(newBrowser := self methodInheritance) == self ifFalse: [
		newBrowser
			positionInWorld: self positionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]
	! !


!TextParagraphMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:27'!
drawOn: aCanvas
	| b |
	b := self localBounds insetBy: borderWidth * 2.
	aCanvas
		fillRectangle: self localBounds color: color.
	aCanvas
		drawTextComposition: textComposition
		at: b topLeft
		extent: b extent
		color: Theme current text
		selectionColor: `Color red`
		avoidOverhang: true.
	aCanvas
		frameRectangle: self localBounds
		color: borderColor
		borderWidth: borderWidth
		borderStyleSymbol: nil! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:01:30'!
drawContainingRectangle: aCanvas
	
	aCanvas frameAndFillRectangle: self localBounds fillColor: self color borderWidth: borderWidth borderColor: borderColor.
! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 8/25/2024 08:54:23'!
fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth := aNumber ]
		ifNil: [ fixedWidth := aNumber.
			morph widthInOwner: aNumber ].
	proportionalWidth := nil.
	self updateContainingLayout.! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/25/2024 08:58:48'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r := currentMorph localBoundsForError.
	w := r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft+1 to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight + (w negated@1) to: r bottomLeft + (1@ w negated) width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourClearOnCurrentMorph.! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/25/2024 09:01:34'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		^ (currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle ].
	^nil.! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 8/25/2024 09:01:37'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	| boundingRect |
	currentMorph knowsOwnLocalBounds ifTrue: [
		boundingRect :=  (currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle.
		"Include an extra pixel to cover possible anti aliasing."
		currentMorph requiresVectorCanvas ifTrue: [
			boundingRect := boundingRect outsetBy: 1 ].
		^boundingRect ].
	^nil.! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 8/25/2024 08:58:52'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r := currentMorph localBoundsForError.
	w := r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	r := r insetBy: w.
	self line: r topLeft to: r bottomRight width: w color: `Color yellow`.
	self line: r topRight to: r bottomLeft width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourSetOnCurrentMorph.! !


!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 8/25/2024 08:58:17'!
openCompletionMenu

	| theEditor |

	theEditor := textMorph editor.
	position := theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1) = Character space
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: (cursorIndex >  theEditor text size ifTrue: [ cursorIndex-1 ] ifFalse: [ cursorIndex ])].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph :=  self morphClass
				completer: self
				position: characterBlock bottomLeft + textMorph positionInWorld ].
! !

