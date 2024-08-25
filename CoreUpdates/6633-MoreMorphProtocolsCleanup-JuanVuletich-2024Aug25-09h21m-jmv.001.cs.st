'From Cuis7.1 [latest update: #6630] on 25 August 2024 at 9:23:06 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:03:33'!
extentInOwner

	self morphExtent ifNotNil: [ :e |
		^ self externalizeDistance: e ].
	^self fullBoundsInOwner extent! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:04:16'!
extentInOwner: newExtent

	self morphExtent: (self internalizeDistance: newExtent).! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:05:50'!
extentInWorld
	"World coordinates"

	^(self externalizeDistanceToWorld: extent) ceiling! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:06:11'!
extentInWorld: newExtent
	"world coordinates"

	self morphExtent: (self internalizeDistanceFromWorld: newExtent).! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:05:11'!
heightInOwner: newHeight

	self extentInOwner: self extentInOwner x @ newHeight! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:07:06'!
position: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self extentInOwner: newExtent.! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:04:16'!
fitInto: aRectangle
	"Change the position and extent of this morph. Arguments are owner's coordinates.
	See inheritance: For general, non resizeable morphs, adjust position and scale."

	self morphPosition: aRectangle origin.
	self extentInOwner: aRectangle extent.! !

!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:04:16'!
widthInOwner: newWidth

	self extentInOwner: newWidth @ self extentInOwner y! !

!BoxedMorph methodsFor: 'window like behavior' stamp: 'jmv 8/25/2024 09:07:06'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self displayBounds.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self position: possibleBounds topLeft extent: possibleBounds extent! !

!BoxedMorph methodsFor: 'window like behavior' stamp: 'jmv 8/25/2024 09:07:06'!
resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self displayBounds].
	self position: boundingBox origin extent: boundingBox extent! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 09:07:06'!
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
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 09:07:06'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self position: aDisplayRectangle topLeft extent: aDisplayRectangle extent.
	haloBox := self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: target shortPrintString.
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 09:06:11'!
doResize: evt with: resizeHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
	evt hand obtainHalo: self.
	newExtent := (evt eventPosition - target positionInWorld) - positionOffset.
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target extentInWorld: newExtent.
	resizeHandle positionInWorld: evt eventPosition - (resizeHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 09:05:50'!
startResize: evt with: resizeHandle
	"Initialize resizing of my target."

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: resizeHandle.  "remove all other handles"
	botRt := target positionInWorld + target extentInWorld.
	positionOffset := (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/25/2024 09:07:06'!
mainLoop

	| morphicCanvas morphicDamageRecorder delay |
	self position: `0@0` extent: Display extent.

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


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 8/25/2024 09:07:06'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w := self new.
	w color: (Color r: 0.167 g: 0.344 b: 0.629).
	w borderWidth: 0.
	w position: `0@0` extent: Display extent.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 8/25/2024 09:05:50'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preferences at: #cheapWindowReframe) or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner extentInWorld ]].! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/25/2024 09:03:33'!
adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs before and after'.
			^self 
	].
	l := self submorphs at: i +1.
	ls := l layoutSpec.
	lCurrentWidth := l extentInOwner x max: 1.	"avoid division by zero"
	r := self submorphs at: i - 1.
	rs := r layoutSpec.
	rCurrentWidth := r extentInOwner x max: 1.	"avoid division by zero"
	delta := localPoint x - aLayoutAdjustMorph referencePositionInOwner x.
	delta := delta max: l minimumShrinkWidth - lCurrentWidth.
	delta := delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth := rCurrentWidth - delta.
	lNewWidth := lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion := lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion := 1.0 - leftNewProportion.
			toDistribute := ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/25/2024 09:03:33'!
adjustVerticallyBy: aLayoutAdjustMorph at: localPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs above and below'.
			^self 
	].	t := self submorphs at: i +1.
	ts := t layoutSpec.
	tCurrentHeight := t extentInOwner y max: 1.	"avoid division by zero"
	b := self submorphs at: i - 1.
	bs := b layoutSpec.
	bCurrentHeight := b extentInOwner y max: 1.	"avoid division by zero"
	delta := localPoint y - aLayoutAdjustMorph referencePositionInOwner y.
	delta := delta max: t minimumShrinkHeight - tCurrentHeight.
	delta := delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight := tCurrentHeight + delta.
	bNewHeight := bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion := tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion := 1.0 - topNewProportion.
			toDistribute := ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example1
"
	self example1
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BoxedMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example101VG
"
	self example101VG
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (Sample09Clock new name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 100); 
	addMorph: (Sample01Star new name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Sample03Smiley new name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (Sample03Smiley new name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color lightRed;
	addMorph: (Sample02Bezier new name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Sample02Bezier new  name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (Sample02Bezier new  name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example11
"
	self example11
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane := (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BoxedMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane position: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example1b
"
Based on #example1, but using some ImageMorph instead of RectangleLikeMorph, so extent is not adjustable
	self example1b
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (ImageMorph new name: #B);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (ImageMorph new name: #G);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ImageMorph new name: #J);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example2
	"
	self example2
	"
	| pane row |
	pane := (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rightOrBottom);
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane position: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example20
"
	self example20
"
| column |

column := (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 30).
column position: 150@130 extent: 400@300.
column openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane := (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BoxedMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane position: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/25/2024 09:07:06'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane := LayoutMorph newRow separation: 5. "1"
pane addMorph: (LabelMorph contents: '1').

rect1 := BoxedMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := BoxedMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	position: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane := LayoutMorph newRow separation: 5. "2"
pane addMorph: (LabelMorph contents: '2').

rect1 := BoxedMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := BoxedMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	position: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane := LayoutMorph newRow separation: 5. "3"
pane addMorph: (LabelMorph contents: '3').

rect1 := BoxedMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := BoxedMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	position: 520 @ 50 extent: 180 @ 100;
	openInWorld! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 8/25/2024 09:07:06'!
updatePositionAndExtent
	| w newExtent |
	layoutSpec notNil ifTrue: [
		^self ].
	w := ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 18.
	w := w min: Display extent x.
	newExtent := w > extent x
		ifTrue: [ w@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world |
			self position: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]! !


!TaskbarMorph methodsFor: 'events' stamp: 'jmv 8/25/2024 09:07:06'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	self world ifNotNil: [ :w |
		y := w morphExtent y - self defaultHeight.
		e := (self internalizeDistance: w morphExtent x @ self defaultHeight) asIntegerPoint.
		self position: 0@y extent: e ].! !

!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 8/25/2024 09:05:50'!
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
				point := 0 @ (taskbarEdge - menu extentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]! !


!MenuMorph methodsFor: 'private' stamp: 'jmv 8/25/2024 09:07:06'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w := submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w := w + 1.
	p := `5 @ 5`.
	submorphs do: [ :m |
		h := m morphHeight.
		m position: p extent: w@h.
		p := p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:07:06'!
adjustExtent
	"And reposition submorphs"
	| w h y |
	"make all items wide, so selection indicator is wide too"
	w := self desiredWidth.
	y := 0.
	self submorphsDo: [ :m |
		h := m morphHeight.
		m position: 0@y extent: w@h.
		y := y + h ].
	self morphExtent: w@y! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:03:33'!
hTotalScrollRange
	"Return the width extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller extentInOwner x! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:03:33'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level.
	Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt := false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller extentInOwner ifFalse: [
				theScrollerExtent := scroller extentInOwner.
				doIt := true ]].
		scrollerLocation := scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale := scrollerLocation scale.
			doIt := true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians := scrollerLocation radians.
			doIt := true ].
		doIt ifTrue: [ self setScrollDeltas ]].! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:07:06'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars = #hide ifTrue: [^self].
	t := ScrollBar scrollbarThickness.
	(hideScrollBars = #hideVertical)
	ifFalse: [
		scrollBar
			position: extent x - t - borderWidth @ borderWidth
			extent: t @ self vScrollBarHeight.
	].
	hScrollBar
		position: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/25/2024 09:03:33'!
vTotalScrollRange
	"Return the height extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller extentInOwner y! !


!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 8/25/2024 09:07:06'!
sliderGrabbedAt: handPositionRelativeToSlider

	| e p |
	grabPosition := handPositionRelativeToSlider.
	self isHorizontal
		ifTrue: [
			e := slider morphWidth @ self buttonExtent.
			p := slider morphPosition x @ borderWidth ]
		ifFalse: [
			e := self buttonExtent @ slider morphHeight.
			p := borderWidth @ slider morphPosition y ].
	sliderShadow
		position: p extent: e;
		show! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/25/2024 09:05:50'!
makeMeFullyVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-extent)]) ifTrue: [
		^ self "OK -- visible"].

	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self extentInWorld world: self world) topLeft! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/25/2024 09:07:06'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos halfThickness |
	thickness := self borderWidth.
	halfThickness := thickness * 0.5.
	cornerExtent := thickness * 5.
	ww := extent x.
	wh := extent y.
	w := ww - cornerExtent - cornerExtent.
	h := wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) position: cornerExtent@halfThickness extent: w@0.
	(adjusters at: #bottomAdjuster) position: cornerExtent@(wh-halfThickness) extent: w@0.
	(adjusters at: #leftAdjuster) position: halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #rightAdjuster) position: ww-halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #topLeftAdjuster) position: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) position: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) position: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) position: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos := borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			position: pos
			extent: extent - pos - borderWidth ].

	self layoutNeeded: false.! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 09:06:11'!
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
			extentInWorld: self extentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 09:06:11'!
browseMethodFullAndReplaceMe
	"Create and schedule a full Browser and then select the current class and message.
	Resize and position it over me and dismiss me."

	model selectedClassOrMetaClass ifNotNil: [ :myClass | | newBrowserWindow |
		newBrowserWindow := BrowserWindow
			fullOnClass: myClass 
			selector: model selectedMessageName.
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/25/2024 09:06:11'!
methodInheritanceAndReplaceMe
	"Create and schedule a method browser on the inheritance of implementors.
	Resize and position it over me and dismiss me."

	| newBrowser |
	(newBrowser := self methodInheritance) == self ifFalse: [
		newBrowser
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ]
	! !


!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'jmv 8/25/2024 09:07:06'!
updateOutlineMorph
	| rectangle |
	rectangle := self selectionRectangle: Display extent.
	outlineMorph
		position: rectangle origin extent: rectangle extent;
		show! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/25/2024 09:07:06'!
resetMenu
	| width newExtent |

	self calculateItemsPerPage.

	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.

	width := self calculateWidth.
	newExtent := width + 4 @ (self itemsPerPage * self itemHeight + 2).

	self position: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !


!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 8/25/2024 09:07:06'!
showSelectorDocumentation

	| selectorDocumentationLocation selectorDocumentationExtent |

	selectorDocumentationLocation := self selectorDefaultDocumentationLocation.
	selectorDocumentationExtent := self selectorDocumentationExtent.
	selectorDocumentationLocation := self adjust: selectorDocumentationLocation ifOutOfScreenWith: selectorDocumentationExtent xOffset: extent x yOffset: self itemHeight negated.

	self setDefaultColors.

	self selectorDocumentation
		model: (TextModel withText: self selectorDocumentationText);
		position: selectorDocumentationLocation extent: selectorDocumentationExtent;
		wrapFlag: false;
		show.

	! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 8/25/2024 09:03:33'!
fixedHeight
	^proportionalHeight isNil ifTrue: [fixedHeight ifNil: [morph extentInOwner y]] ifFalse: [ 0 ]! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 8/25/2024 09:05:11'!
fixedOrMorphHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight := aNumber ]
		ifNil: [ fixedHeight := aNumber.
			  morph heightInOwner: aNumber 
		].
	proportionalHeight := nil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 8/25/2024 09:03:33'!
fixedWidth
	^proportionalWidth isNil ifTrue: [fixedWidth ifNil: [morph extentInOwner x]] ifFalse: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 8/25/2024 09:03:33'!
fixedOrMinimumLayoutHeight
	^fixedHeight ifNil: [ morph extentInOwner y ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 8/25/2024 09:03:33'!
fixedOrMinimumLayoutWidth
	^fixedWidth ifNil: [ morph extentInOwner x ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 8/25/2024 09:03:33'!
minimumSpecExtent
	"If fixedHeight is not nil, use it.  
	 If fixdHeight and propostionlHeight are nil, use morphHeight"

	| meo h w |
	meo := morph extentInOwner.
	w := fixedWidth ifNil: [ proportionalWidth ifNotNil: [ 0 ] ifNil: [  meo x ] ].
	h := fixedHeight ifNil: [ proportionalHeight ifNotNil: [ 0 ] ifNil: [ meo y ] ].
	^w@h! !

