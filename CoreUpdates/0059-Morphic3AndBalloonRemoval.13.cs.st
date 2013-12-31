'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 26 August 2008 at 1:17:48 pm'!!BitBlt class methodsFor: 'examples' stamp: 'jmv 8/26/2008 12:47'!exampleOne	"This tests BitBlt by displaying the result of all sixteen combination rules that BitBlt is capable of using. (Please see the comment in BitBlt for the meaning of the combination rules). This only works at Display depth of 1. (Rule 15 does not work?)"	| path displayDepth |"jmv- this nice example needs some replacement for the Path (I removed class Path)""	displayDepth _ Display depth.	Display newDepth: 1.	path _ Path new.	0 to: 3 do: [:i | 0 to: 3 do: [:j | path add: j * 100 @ (i * 75)]].	Display fillWhite.	path _ path translateBy: 60 @ 40.	1 to: 16 do: [:index | BitBlt			exampleAt: (path at: index)			rule: index - 1			fillColor: nil].	[Sensor anyButtonPressed] whileFalse: [].	Display newDepth: displayDepth."	"BitBlt exampleOne"! !!Canvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/26/2008 11:44'!fillRectangle: aRectangle color: aColor borderStyle: aBorderStyle	"Fill the given rectangle."	self fillRectangle: (aRectangle insetBy: aBorderStyle width) color: aColor.	aBorderStyle frameRectangle: aRectangle on: self! !!Form methodsFor: 'displaying' stamp: 'jmv 8/26/2008 10:43'!displayInterpolatedIn: aRectangle on: aForm	"Display the receiver on aForm, using interpolation if necessary.		Form fromUser displayInterpolatedOn: Display.	"	self extent = aRectangle extent ifTrue:[^self displayOn: aForm at: aRectangle origin].		"We've got no bilinear interpolation. Use WarpBlt instead"	(WarpBlt current toForm: aForm)		sourceForm: self destRect: aRectangle;		combinationRule: 3;		cellSize: 2;		warpBits! !!Form methodsFor: 'displaying' stamp: 'jmv 8/26/2008 10:44'!displayInterpolatedOn: aForm	"Display the receiver on aForm, using interpolation if necessary.		Form fromUser displayInterpolatedOn: Display.	"	self extent = aForm extent ifTrue:[^self displayOn: aForm].		"We've got no bilinear interpolation. Use WarpBlt instead"	(WarpBlt current toForm: aForm)		sourceForm: self destRect: aForm boundingBox;		combinationRule: 3;		cellSize: 2;		warpBits! !!Form methodsFor: 'displaying' stamp: 'jmv 8/26/2008 10:45'!displayResourceFormOn: aForm	"a special display method for blowing up resource thumbnails"	self extent = aForm extent ifTrue:[^self displayOn: aForm].	"We've got no bilinear interpolation. Use WarpBlt instead"	(WarpBlt current toForm: aForm)		sourceForm: self destRect: aForm boundingBox;		combinationRule: 3;		cellSize: 2;		warpBits! !!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/26/2008 10:50'!fillRectangle: aRectangle fillStyle: aFillStyle	"Fill the given rectangle."	| pattern |	self shadowColor ifNotNil:		[^self fillRectangle: aRectangle color: aFillStyle asColor].	(aFillStyle isKindOf: InfiniteForm) ifTrue: [		^self infiniteFillRectangle: aRectangle fillStyle: aFillStyle	].	(aFillStyle isSolidFill) 		ifTrue:[^self fillRectangle: aRectangle color: aFillStyle asColor].	"We have a very special case for filling with infinite forms"	(aFillStyle isBitmapFill and:[aFillStyle origin = (0@0)]) ifTrue:[		pattern _ aFillStyle form.		(aFillStyle direction = (pattern width @ 0) 			and:[aFillStyle normal = (0@pattern height)]) ifTrue:[				"Can use an InfiniteForm"				^self fillRectangle: aRectangle color: (InfiniteForm with: pattern)].	].	self fillRectangle: aRectangle color: aFillStyle asColor! !!LightWidget methodsFor: 'defaults' stamp: 'jmv 8/26/2008 10:18'!borderStyleFor: aColor	^OldBorderStyle raised width: 3; color: aColor! !!LightWidget methodsFor: 'defaults' stamp: 'jmv 8/26/2008 10:18'!borderStyleWith: aColor	^OldBorderStyle raised width: 3; color: aColor! !!LightWidget methodsFor: 'events-processing' stamp: 'jmv 8/26/2008 13:02'!handleMouseDown: anEvent	"System level event handling."	anEvent wasHandled ifTrue:[^self]. "not interested"	anEvent hand removePendingBalloonFor: self.	anEvent wasHandled: true.	(anEvent controlKeyPressed			and: [Preferences cmdGesturesEnabled])		ifTrue: [^ self invokeMetaMenu: anEvent].	"Make me modal during mouse transitions"	anEvent hand newMouseFocus: self event: anEvent.	anEvent blueButtonChanged ifTrue:[^self blueButtonDown: anEvent].	self mouseDown: anEvent.	anEvent hand removeHaloFromClick: anEvent on: self.	(self handlesMouseStillDown: anEvent) ifTrue:[		self startStepping: #handleMouseStillDown: 			at: Time millisecondClockValue + self mouseStillDownThreshold			arguments: {anEvent copy resetHandlerFields}			stepTime: self mouseStillDownStepRate ].! !!LightWidget methodsFor: 'events-processing' stamp: 'jmv 8/26/2008 13:03'!handleMouseLeave: anEvent	"System level event handling."	anEvent hand removePendingBalloonFor: self.	anEvent isDraggingEvent ifTrue:[		(self handlesMouseOverDragging: anEvent) ifTrue:[			anEvent wasHandled: true.			self mouseLeaveDragging: anEvent].		^self].	(self handlesMouseOver: anEvent) ifTrue:[		anEvent wasHandled: true.		self mouseLeave: anEvent.	].! !!IndeterminatePogressBarLW methodsFor: 'drawing' stamp: 'jmv 8/26/2008 11:34'!drawOn: aCanvas	| bw bc pos fillBounds |	bw _ self borderWidth.	bc _ self borderColor.			aCanvas fillRectangle: bounds color: self backgroundColor.	pos _ bounds origin  + (xOffset@0).	fillBounds _ pos  extent:  (bounds extent  x/ 4)@(bounds extent y).	aCanvas fillRectangle: fillBounds color: self fillColor.				aCanvas frameRectangle: bounds width: bw color: bc		! !!OldBorderStyle class methodsFor: 'instance creation' stamp: 'jmv 8/26/2008 10:20'!borderStyleChoices	"Answer the superset of all supported borderStyle symbols"	^ #(simple inset raised)! !!OldMorph methodsFor: '*geniestubs-stubs' stamp: 'jmv 8/26/2008 13:03'!handleMouseDown: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue:[^self]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.

	(anEvent controlKeyPressed
			and: [Preferences cmdGesturesEnabled])
		ifTrue: [^ self invokeMetaMenu: anEvent].

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.
	anEvent blueButtonChanged ifTrue:[^self blueButtonDown: anEvent].

	self mouseDown: anEvent.
	anEvent hand removeHaloFromClick: anEvent on: self.

	(self handlesMouseStillDown: anEvent) ifTrue:[
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {anEvent copy resetHandlerFields}
			stepTime: self mouseStillDownStepRate ].
! !!OldMorph methodsFor: 'events-processing' stamp: 'jmv 8/26/2008 13:03'!handleMouseLeave: anEvent	"System level event handling."	anEvent hand removePendingBalloonFor: self.	anEvent isDraggingEvent ifTrue:[		(self handlesMouseOverDragging: anEvent) ifTrue:[			anEvent wasHandled: true.			self mouseLeaveDragging: anEvent].		^self].	(self handlesMouseOver: anEvent) ifTrue:[		anEvent wasHandled: true.		self mouseLeave: anEvent.	].! !!OldMorph methodsFor: 'menus' stamp: 'jmv 8/26/2008 11:28'!addFillStyleMenuItems: aMenu hand: aHand 
	"Add the items for changing the current fill style of the Morph"
	aMenu add: 'change color...' translated action: #changeColor! !!OldEllipseMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2008 11:31'!drawOn: aCanvas 	aCanvas isShadowDrawing		ifTrue: [^ aCanvas fillOval: bounds color:  color borderWidth: 0 borderColor: nil].	aCanvas fillOval: bounds color: color borderWidth: borderWidth borderColor: borderColor.! !!OldHaloMorph methodsFor: 'events' stamp: 'jmv 8/26/2008 13:08'!popUpFor: aMorph event: evt 	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."	| hand anEvent |	self flag: #workAround.	"We should really have some event/hand here..."	anEvent := evt isNil 				ifTrue: 					[hand := aMorph world activeHand.					hand ifNil: [hand := aMorph world primaryHand]. 					hand lastEvent transformedBy: (aMorph transformedFrom: nil)]				ifFalse: 					[hand := evt hand.					evt].	self target: aMorph.	hand halo: self.	hand world addMorphFront: self.	positionOffset := anEvent position 				- (aMorph point: aMorph position in: owner).	self startStepping! !!OldHaloMorph methodsFor: 'meta-actions' stamp: 'jmv 8/26/2008 13:07'!blueButtonDown: event	"Transfer the halo to the next likely recipient"	target ifNil:[^self delete].	event hand obtainHalo: self.	positionOffset _ event position - (target point: target position in: owner).	"wait for drags or transfer"	event hand 		waitForClicksOrDrag: self 		event: event		selectors: { nil. nil. nil. #dragTarget:. }		threshold: 5.! !!OldHaloMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/26/2008 13:07'!delete	"Delete the halo.  Tell the target that it no longer has the halo; accept any pending edits to the name; and then either actually delete myself"	self acceptNameEdit.	super delete! !!OldHaloMorph methodsFor: 'private' stamp: 'jmv 8/26/2008 13:07'!addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse |
	aPoint := self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	handle := OldEllipseMorph 
				newBounds: (Rectangle center: aPoint extent: HandleSize asPoint)
				color: (colorToUse := Color colorFrom: handleSpec color).
	self addMorph: handle.
	(iconName := handleSpec iconSymbol) ifNotNil: 
			[| form |
			form := ScriptingSystem formAtKey: iconName.
			form ifNotNil: 
					[handle addMorphCentered: ((OldImageMorph new)
								image: form;
								color: colorToUse makeForegroundColor;
								lock)]].
	handle 
		on: #mouseUp
		send: #endInteraction
		to: self.
	handle 
		on: eventName
		send: selector
		to: recipient.
	handle 
		setBalloonText: (target balloonHelpTextForHandle: handle) translated.
	^handle! !!OldHaloMorph methodsFor: 'private' stamp: 'jmv 8/26/2008 13:11'!endInteraction
	"Clean up after a user interaction with the a halo control"

	(target isInWorld not or: [owner isNil]) ifTrue: [^self].
	self isInWorld 
		ifTrue: [
			"make sure handles show in front, even if flex shell added"
			self comeToFront.
			self addHandles]! !!OldMenuItemMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2008 11:39'!drawOn: aCanvas 	| stringColor stringBounds leftEdge |	isSelected & isEnabled		ifTrue: [			aCanvas fillRectangle: self bounds color: (				Display depth <= 2					ifTrue: [ Color gray ]					ifFalse: [ Color lightBlue ])].	stringColor := color.	leftEdge := 0.	self hasIcon		ifTrue: [| iconForm | 			iconForm := isEnabled ifTrue:[self icon] ifFalse:[self icon asGrayScale].			aCanvas paintImage: iconForm at: self left @ (self top + (self height - iconForm height // 2)).			leftEdge := iconForm width + 2].	self hasMarker		ifTrue: [ leftEdge := leftEdge + self submorphBounds width + 8 ].	stringBounds := bounds left: bounds left + leftEdge.	aCanvas		drawString: contents		in: stringBounds		font: self fontToUse		color: stringColor.	subMenu		ifNotNil: [aCanvas paintImage: SubMenuMarker at: self right - 8 @ (self top + self bottom - SubMenuMarker height // 2)]! !!OldMenuItemMorph methodsFor: 'private' stamp: 'jmv 8/26/2008 11:42'!onImage	"Return the form to be used for indicating an '<off>' marker"	| form |	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.	(form getCanvas)		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 			borderWidth: 1 borderColor: Color black;		fillRectangle: (form boundingBox insetBy: 2) color: Color black.	^form! !!OldPolygonMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2008 10:59'!drawOn: aCanvas 	"Display the receiver, a spline curve, approximated by straight line segments."	| lineColor bevel topLeftColor bottomRightColor bigClipRect brush p1i p2i |	vertices size < 1 ifTrue: [self error: 'a polygon must have at least one point'].	closed & color isTransparent not ifTrue:		[self filledForm colors: (Array with: Color transparent with: color).		aCanvas image: self filledForm at: bounds topLeft-1].	lineColor _ borderColor.  bevel _ false.	"Border colors for bevelled effects depend on CW ordering of vertices"	borderColor == #raised ifTrue: [topLeftColor _ color lighter.						bottomRightColor _ color darker.  bevel _ true].	borderColor == #inset ifTrue: [topLeftColor _ owner colorForInsets darker.						bottomRightColor _ owner colorForInsets lighter.  bevel _ true].	bigClipRect _ aCanvas clipRect expandBy: self borderWidth+1//2.	brush _ nil.	self lineSegmentsDo:		[:p1 :p2 | p1i _ p1 asIntegerPoint.  p2i _ p2 asIntegerPoint.		(closed or: ["bigClipRect intersects: (p1i rect: p2i) optimized:"					((p1i min: p2i) max: bigClipRect origin) <=					((p1i max: p2i) min: bigClipRect corner)]) ifTrue:			[bevel ifTrue: [((p1i quadrantOf: p2i) > 2)						ifTrue: [lineColor _ topLeftColor]						ifFalse: [lineColor _ bottomRightColor]].			(borderWidth > 3 and: [borderColor isColor])			ifTrue: [brush == nil ifTrue:						[brush _ (ColorForm dotOfSize: borderWidth)								colors: (Array with: Color transparent with: borderColor)].					aCanvas line: p1i to: p2i brushForm: brush]			ifFalse: [aCanvas line: p1i to: p2i							width: borderWidth color: lineColor]]].	self arrowForms ifNotNil:		[self arrowForms do:			[:f | f colors: (Array with: Color transparent with: borderColor).			aCanvas image: f at: f offset]]! !!OldPolygonMorph methodsFor: 'private' stamp: 'jmv 8/26/2008 11:01'!filledForm	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."	| bb origin |	closed ifFalse: [^ filledForm _ nil].	filledForm ifNotNil: [^ filledForm].	filledForm _ ColorForm extent: bounds extent+2.	"Draw the border..."	bb _ (BitBlt current toForm: filledForm) sourceForm: nil; fillColor: Color black;			combinationRule: Form over; width: 1; height: 1.	origin _ bounds topLeft asIntegerPoint-1.	self lineSegmentsDo: [:p1 :p2 | bb drawFrom: p1 asIntegerPoint-origin										to: p2 asIntegerPoint-origin].	"Fill it in..."	filledForm convexShapeFill: Color black.	(borderColor isColor and: [borderColor isTranslucentColor]) ifTrue:		["If border is stored as a form, then erase any overlap now."		filledForm copy: self borderForm boundingBox from: self borderForm			to: 1@1 rule: Form erase].	^ filledForm! !!OldFillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/26/2008 11:35'!createAcceptButton
	"create the [accept] button"

	| result frame |
	result := (OldSimpleButtonMorph new)
				target: self;
				color: Color lightGreen.
	result borderColor: (Preferences menuAppearance3d 
				ifTrue: [#raised]
				ifFalse: [result color twiceDarker]).
	result
		label: 'Accept(s)' translated;
		actionSelector: #accept.
	result setNameTo: 'accept'.
	frame := OldLayoutFrame new.
	frame
		rightFraction: 0.5;
		rightOffset: -10;
		bottomFraction: 1.0;
		bottomOffset: -2.
	result layoutFrame: frame.
	self addMorph: result.
	^result! !!OldFillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/26/2008 11:35'!createCancelButton
	"create the [cancel] button"

	| result frame |
	result := (OldSimpleButtonMorph new)
				target: self;
				color: Color lightRed.
	result borderColor: (Preferences menuAppearance3d 
				ifTrue: [#raised]
				ifFalse: [result color twiceDarker]).
	result
		label: 'Cancel(l)' translated;
		actionSelector: #cancel.
	result setNameTo: 'cancel'.
	frame := OldLayoutFrame new.
	frame
		leftFraction: 0.5;
		leftOffset: 10;
		bottomFraction: 1.0;
		bottomOffset: -2.
	result layoutFrame: frame.
	self addMorph: result.
	^result! !!OldFillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/26/2008 11:35'!updateColor	"update the recevier's fillStyle"	| textPaneBorderColor |	textPane isNil		ifTrue: [^ self].	textPaneBorderColor := self borderColor == #raised				ifTrue: [#inset]				ifFalse: [self borderColor].	textPane borderColor: textPaneBorderColor! !!OldMenuMorph methodsFor: 'control' stamp: 'jmv 8/26/2008 11:37'!popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem 	"Present this menu at the given point under control of the given  	hand."	| delta tryToPlace selectedOffset |	hand world startSteppingSubmorphsOf: self.	popUpOwner := sourceItem.	self fullBounds.		"ensure layout is current"	selectedOffset := (selectedItem				ifNil: [self items first]) position - self position.	tryToPlace := [:where :mustFit | 			self position: where - selectedOffset.			delta := self fullBoundsInWorld amountToTranslateWithin: sourceItem worldBounds.			(delta x = 0					or: [mustFit])				ifTrue: [delta = (0 @ 0)						ifFalse: [self position: self position + delta].					sourceItem owner owner addMorphFront: self.					^ self]].	tryToPlace value: rightOrLeftPoint first value: false;		 value: rightOrLeftPoint last - (self width @ 0) value: false;		 value: rightOrLeftPoint first value: true! !!OldMenuMorph methodsFor: 'control' stamp: 'jmv 8/26/2008 11:37'!popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given  
	hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	MenuIcons decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: OldUpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	"precompute width"
	self 
		positionAt: aPoint
		relativeTo: (selectedItem ifNil: [self items first])
		inWorld: aWorld.
	aWorld addMorphFront: self.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard or: [evt isMouse and: [evt anyButtonPressed not]]) 
		ifTrue: 
			["Select first item if button not down"

			self moveSelectionDown: 1 event: evt].
	self changed! !!OldScrollBar class methodsFor: 'coloring morphs' stamp: 'jmv 8/26/2008 12:30'!updateScrollBarButtonsAspect: aCollection color: aColor 	"update aCollection of morphs with aColor"			aCollection		do: [:each | each color: aColor]! !!OldScrollBar class methodsFor: 'images' stamp: 'jmv 8/26/2008 12:30'!createArrowOfDirection: aSymbolDirection size: finalSizeInteger color: aColor 
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #top, #bottom. #left or #right  
	 
	Try with:  
	(ScrollBar createArrowOfDirection: #top size: 32 color: Color  
	lightGreen) asMorph openInHand.  
	"

	| resizeFactor outerBox arrow resizedForm |
	resizeFactor := 4.
	outerBox := OldRectangleMorph new.
	outerBox
		extent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: aColor.
	""
	arrow := self createArrowOfDirection: aSymbolDirection in: outerBox bounds.
	arrow color: aColor muchDarker.
	outerBox addMorphCentered: arrow.
	""
	resizedForm := outerBox imageForm 
				magnify: outerBox imageForm boundingBox
				by: 1 / resizeFactor
				smoothing: 4.
	""
	^(resizedForm replaceColor: aColor withColor: Color transparent) 
		trimBordersOfColor: Color transparent! !!OldScrollBar class methodsFor: 'images' stamp: 'jmv 8/26/2008 12:30'!createBoxOfSize: finalSizeInteger color: aColor 
	"PRIVATE - create a box with finalSizeInteger and aColor  
	 
	Try with:  
	(ScrollBar createBoxOfSize: 32 color: Color lightGreen) asMorph  
	openInHand.  
	"

	| resizeFactor outerBox innerBox resizedForm |
	resizeFactor := 4.
	outerBox := OldRectangleMorph new.
	outerBox
		extent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: aColor.
	""
	innerBox := self createBoxIn: outerBox bounds.
	innerBox color: aColor muchDarker.
	outerBox addMorphCentered: innerBox.
	""
	resizedForm := outerBox imageForm 
				magnify: outerBox imageForm boundingBox
				by: 1 / resizeFactor
				smoothing: 4.
	""
	^(resizedForm replaceColor: aColor withColor: Color transparent) 
		trimBordersOfColor: Color transparent! !!OldSketchMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2008 10:46'!generateRotatedForm	"Compute my rotatedForm and offsetWhenRotated."	| scalePt smoothPix pair |	scalePoint ifNil: [scalePoint := 1 @ 1].	scalePt := scalePoint x abs @ scalePoint y abs.	rotationStyle == #none ifTrue: [scalePt := 1 @ 1].	smoothPix := (scalePt x < 1.0 or: [scalePt y < 1.0]) 		ifTrue: [2]		ifFalse: [1].	rotationStyle = #leftRight 		ifTrue: 			[self heading asSmallAngleDegrees < 0.0 				ifTrue: [scalePt := scalePt x negated @ scalePt y]].	rotationStyle = #upDown 		ifTrue: 			[self heading asSmallAngleDegrees abs > 90.0 				ifTrue: [scalePt := scalePt x @ scalePt y negated]].	rotatedForm := scalePt = (1 @ 1) 				ifTrue: [originalForm]				ifFalse: 					[					pair := WarpBlt current 								rotate: originalForm								degrees: 0								center: originalForm boundingBox center								scaleBy: scalePt								smoothing: smoothPix.					pair first]! !!OldSystemWindow methodsFor: 'panes' stamp: 'jmv 8/26/2008 12:26'!updateBox: anIconMorph color: aColor 	| fill |	anIconMorph isNil		ifTrue: [^ self].	anIconMorph		extent: self boxExtent.	fill _ aColor alphaMixed: 0.5 with: Color white.	anIconMorph fillStyle: fill.	anIconMorph borderWidth: ((Preferences alternativeWindowLook					and: [Preferences alternativeWindowBoxesLook])				ifTrue: [1]				ifFalse: [0]);		borderColor: aColor darker! !!OldTheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/26/2008 11:30'!appearanceMenu
	"Build the appearance menu for the world."	
	^self fillIn: (self menu: 'appearance...') from: {
		{'system fonts...' . { self . #standardFontDo} . 'Choose the standard fonts to use for code, lists, menus, window titles, etc.'}.
		nil.
		{#menuColorString . { Preferences . #toggleMenuColorPolicy} . 'Governs whether menu colors should be derived from the desktop color.'}.
		nil.
		{'full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
	}! !!PluggableCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/26/2008 10:51'!fillRectangle: aRectangle fillStyle: aFillStyle	| pattern |	self shadowColor ifNotNil: [^self fillRectangle: aRectangle color: self shadowColor].	(aFillStyle isKindOf: InfiniteForm) ifTrue: [		^self infiniteFillRectangle: aRectangle fillStyle: aFillStyle	].	aFillStyle isSolidFill ifTrue:[ ^self fillRectangle: aRectangle color: aFillStyle asColor].	"We have a very special case for filling with infinite forms"	(aFillStyle isBitmapFill and:[aFillStyle origin = (0@0)]) ifTrue:[		pattern _ aFillStyle form.		(aFillStyle direction = (pattern width @ 0) 			and:[aFillStyle normal = (0@pattern height)]) ifTrue:[				"Can use an InfiniteForm"				^self fillRectangle: aRectangle color: (InfiniteForm with: pattern)].	].	self fillRectangle: aRectangle color: aFillStyle asColor! !!Preferences class methodsFor: 'misc' stamp: 'jmv 8/26/2008 13:16'!defaultValueTableForCurrentRelease
	"Answer a table defining default values for all the preferences in the release.  Returns a list of (pref-symbol, boolean-symbol) pairs"

	^  #(
		(abbreviatedBrowserButtons false)
		(alternativeBrowseIt false)
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)
		(annotationPanes false)
		(automaticFlapLayout true)
		(automaticPlatformSettings true)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(canRecordWhilePlaying false)
		(caseSensitiveFinds false)
		(changeSetVersionNumbers true)
		(checkForSlips true)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(confirmFirstUseOfStyle true)
		(conversionMethodsAtFileOut false)
		(cpuWatcherEnabled false)
		(debugHaloHandle true)
		(debugPrintSpaceLog false)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(dismissAllOnOptionClose false)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(higherPerformance false)
		(honorDesktopCmdKeys true)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl false)  
		(modalColorPickers true)
		(mouseOverForKeyboardFocus false)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(projectsSentToDisk false)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(showBoundsInHalo false)
		(simpleMenus false)
		(smartUpdating true)
		(soundQuickStart false)
		(soundStopWhenDone false)
		(soundsEnabled true)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(twentyFourHourFileStamps true)
		(uniqueNamesInHalos false)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))


"
Preferences defaultValueTableForCurrentRelease do:
	[:pair | (Preferences preferenceAt: pair first ifAbsent: [nil]) ifNotNilDo:
			[:pref | pref defaultValue: (pair last == #true)]].
Preferences chooseInitialSettings.
"! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 13:01'!brightSqueak
	"The classic bright Squeak look.  Windows have saturated colors and relatively low contrast; scroll-bars are of the flop-out variety and are on the left.  Many power-user features are enabled."

	self setPreferencesFrom:
	#(
		(alternativeScrollbarLook false)
		(alternativeWindowLook false)
		(annotationPanes true)
		(automaticFlapLayout true)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(debugHaloHandle true)
		(debugPrintSpaceLog false)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars false)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl true)  
		(mouseOverForKeyboardFocus true)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(simpleMenus false)
		(smartUpdating true)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 12:30'!juans

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)		(alternativeWindowBoxesLook true)		(alwaysHideHScrollbar false)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar true)
		(annotationPanes true)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips true)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl true)  
		(mouseOverForKeyboardFocus false)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow true)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating true)		(subPixelRenderFonts true)
		(thoroughSenders true)	)! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 13:01'!magdeburg	"Alternative window & scroll-bar looks, no desktop command keys, no keyboard menu control, no annotation panes..."	self setPreferencesFrom: #(		(alternativeScrollbarLook true)		(alternativeWindowLook true)		(annotationPanes false)		(canRecordWhilePlaying false)		(conversionMethodsAtFileOut true)		(honorDesktopCmdKeys false)		(menuKeyboardControl false)  		(scrollBarsWithoutMenuButton true))! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 13:16'!paloAlto
	"Similar to the brightSqueak theme, but with a number of idiosyncratic personal settings.   Note that mouseOverForKeyboardFocus & caseSensitiveFinds are both true"


	self setPreferencesFrom:
	#(
		(abbreviatedBrowserButtons false)
		(accessOnlineModuleRepositories noOpinion)
		(alternativeBrowseIt noOpinion)
		(alternativeScrollbarLook false)
		(alternativeWindowLook false)
		(annotationPanes true)
		(automaticFlapLayout true)
		(automaticPlatformSettings noOpinion)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(canRecordWhilePlaying noOpinion)
		(caseSensitiveFinds true)
		(changeSetVersionNumbers true)
		(checkForSlips true)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(confirmFirstUseOfStyle true)
		(conservativeModuleDeActivation noOpinion)
		(conversionMethodsAtFileOut true)
		(cpuWatcherEnabled noOpinion)
		(debugHaloHandle true)
		(debugPrintSpaceLog true)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(dismissAllOnOptionClose true)
		(duplicateControlAndAltKeys false)
		(extraDebuggerButtons true)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(higherPerformance noOpinion)
		(honorDesktopCmdKeys true)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars false)
		(lenientScopeForGlobals noOpinion)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl true)  
		(modalColorPickers true)
		(modularClassDefinitions noOpinion)
		(mouseOverForKeyboardFocus true)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(projectsSentToDisk noOpinion)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(showBoundsInHalo false)
		(simpleMenus false)
		(smartUpdating true)
		(soundQuickStart noOpinion)
		(soundsEnabled true)
		(soundStopWhenDone noOpinion)
		(strongModules noOpinion)
		(swapControlAndAltKeys noOpinion)
		(swapMouseButtons  noOpinion)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(twentyFourHourFileStamps false)
		(uniqueNamesInHalos false)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 12:31'!slowMachine

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook false)		(alternativeWindowBoxesLook false)		(alwaysHideHScrollbar true)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar false)
		(annotationPanes false)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl false)  
		(mouseOverForKeyboardFocus true)
		(noviceMode false)
		(optionalButtons false)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating false)		(subPixelRenderFonts false)
		(thoroughSenders false)	)! !!Preferences class methodsFor: 'themes' stamp: 'jmv 8/26/2008 13:01'!westwood
	"Settings generally similar to those found in a standard browser-plug-in-based Squeak image"

	self setPreferencesFrom: #(
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)
		(honorDesktopCmdKeys false)
		(menuKeyboardControl false)
		(propertySheetFromHalo true)		)! !Preferences class removeSelector: #areaFillsAreTolerant!Preferences class removeSelector: #areaFillsAreVeryTolerant!Preferences class removeSelector: #gradientMenu!Preferences class removeSelector: #gradientScrollBars!Preferences class removeSelector: #haloTransitions!Preferences class removeSelector: #magicHalos!PluggableCanvas removeSelector: #balloonFillOval:fillStyle:borderWidth:borderColor:!PluggableCanvas removeSelector: #balloonFillRectangle:fillStyle:!PluggableCanvas removeSelector: #drawPolygon:color:borderWidth:borderColor:!PluggableCanvas removeSelector: #fillOval:fillStyle:borderWidth:borderColor:!PluggableCanvas removeSelector: #render:!OldTheWorldMenu removeSelector: #setGradientColor!OldSketchMorph removeSelector: #addToggleItemsToHaloMenu:!OldSketchMorph removeSelector: #toggleInterpolation!OldSketchMorph removeSelector: #useInterpolation!OldSketchMorph removeSelector: #useInterpolation:!OldSketchMorph removeSelector: #useInterpolationString!OldSimpleButtonMorph removeSelector: #recolor:!OldScrollBar class removeSelector: #updateScrollBarButtonAspect:color:!OldMenuMorph removeSelector: #updateColor!OldFillInTheBlankMorph removeSelector: #updateColor:color:intensity:!OldAlignmentMorph removeSelector: #canHaveFillStyles!OldRectangleMorph removeSelector: #canHaveFillStyles!OldPolygonMorph removeSelector: #canHaveFillStyles!OldPolygonMorph removeSelector: #drawArrowOn:at:from:!OldPolygonMorph removeSelector: #drawArrowsOn:!OldPasteUpMorph removeSelector: #canHaveFillStyles!OldPasteUpMorph removeSelector: #gradientFillColor:!OldPasteUpMorph removeSelector: #setGradientColor:!OldMenuItemMorph removeSelector: #selectionFillStyle!OldHandMorph removeSelector: #removePendingHaloFor:!OldHandMorph removeSelector: #spawnMagicHaloFor:!OldHandMorph removeSelector: #triggerHaloFor:after:!OldHaloMorph removeSelector: #drawSubmorphsOn:!OldHaloMorph removeSelector: #fadeIn!OldHaloMorph removeSelector: #fadeInInitially!OldHaloMorph removeSelector: #fadeOut!OldHaloMorph removeSelector: #fadeOutFinally!OldHaloMorph removeSelector: #handleEntered!OldHaloMorph removeSelector: #handleLeft!OldHaloMorph removeSelector: #isMagicHalo!OldHaloMorph removeSelector: #isMagicHalo:!OldHaloMorph removeSelector: #magicAlpha!OldHaloMorph removeSelector: #magicAlpha:!OldHaloMorph removeSelector: #popUpMagicallyFor:hand:!OldEllipseMorph removeSelector: #canHaveFillStyles!OldMorph removeSelector: #addMagicHaloFor:!OldMorph removeSelector: #canHaveFillStyles!OldMorph removeSelector: #fillWithRamp:oriented:!OldMorph removeSelector: #useBitmapFill!OldMorph removeSelector: #useGradientFill!OldMorph removeSelector: #useSolidFill!OldBorderStyle class removeSelector: #complexAltFramed!OldBorderStyle class removeSelector: #complexAltInset!OldBorderStyle class removeSelector: #complexAltRaised!OldBorderStyle class removeSelector: #complexFramed!OldBorderStyle class removeSelector: #complexInset!OldBorderStyle class removeSelector: #complexRaised!IndeterminatePogressBarLW removeSelector: #fillStyle!InfiniteForm removeSelector: #addFillStyleMenuItems:hand:from:!FormCanvas removeSelector: #asBalloonCanvas!FormCanvas removeSelector: #asMorphicCanvas!FormCanvas removeSelector: #balloonFillOval:fillStyle:borderWidth:borderColor:!FormCanvas removeSelector: #balloonFillRectangle:fillStyle:!FormCanvas removeSelector: #drawPolygon:color:borderWidth:borderColor:!FormCanvas removeSelector: #drawPolygon:fillStyle:borderWidth:borderColor:!FormCanvas removeSelector: #fillOval:fillStyle:borderWidth:borderColor:!FormCanvas removeSelector: #render:!Form removeSelector: #floodFill:at:!Form removeSelector: #floodFill:at:tolerance:!FileList2 class removeSelector: #update:in:fileTypeRow:morphUp:!ColorMappingCanvas removeSelector: #drawPolygon:color:borderWidth:borderColor:!Color removeSelector: #addFillStyleMenuItems:hand:from:!Canvas removeSelector: #asAlphaBlendingCanvas:!Canvas removeSelector: #drawPolygon:color:borderWidth:borderColor:!Canvas removeSelector: #drawPolygon:fillStyle:!Canvas removeSelector: #drawPolygon:fillStyle:borderWidth:borderColor:!Canvas removeSelector: #fillOval:fillStyle:!Canvas removeSelector: #fillOval:fillStyle:borderWidth:borderColor:!Canvas removeSelector: #render:!Smalltalk removeClassNamed: #AlphaBlendingCanvas!Smalltalk removeClassNamed: #Arc!Smalltalk removeClassNamed: #BalloonBezierSimulation!Smalltalk removeClassNamed: #BalloonBuffer!Smalltalk removeClassNamed: #BalloonCanvas!Smalltalk removeClassNamed: #BalloonEdgeData!Smalltalk removeClassNamed: #BalloonEngine!Smalltalk removeClassNamed: #BalloonEngineBase!Smalltalk removeClassNamed: #BalloonEngineConstants!Smalltalk removeClassNamed: #BalloonEnginePlugin!Smalltalk removeClassNamed: #BalloonEngineSimulation!Smalltalk removeClassNamed: #BalloonFillData!Smalltalk removeClassNamed: #BalloonLineSimulation!Smalltalk removeClassNamed: #BalloonSolidFillSimulation!Smalltalk removeClassNamed: #BalloonState!Smalltalk removeClassNamed: #Bezier2Segment!Smalltalk removeClassNamed: #Bezier3Segment!Smalltalk removeClassNamed: #BitmapFillStyle!Smalltalk removeClassNamed: #CartesianCoordinateSystem!Smalltalk removeClassNamed: #Circle!Smalltalk removeClassNamed: #CompositeMorph!Smalltalk removeClassNamed: #CompressedBoundaryShape!Smalltalk removeClassNamed: #ContinuousImage!Smalltalk removeClassNamed: #CoordinateSystem!Smalltalk removeClassNamed: #CurveFitter!Smalltalk removeClassNamed: #EarthMapMorph!Smalltalk removeClassNamed: #FillStyle!Smalltalk removeClassNamed: #FlexImage!Smalltalk removeClassNamed: #FlexImageBuilder!Smalltalk removeClassNamed: #FlexImageTest!Smalltalk removeClassNamed: #GeneralInterpolatedFunction!Smalltalk removeClassNamed: #GeneralInterpolatedFunctionBuilder!Smalltalk removeClassNamed: #GradientFillStyle!Smalltalk removeClassNamed: #ImageMorph!Smalltalk removeClassNamed: #InterpolatedFunction!Smalltalk removeClassNamed: #InterpolatedFunctionBuilder!Smalltalk removeClassNamed: #InterpolatedFunctionTest!Smalltalk removeClassNamed: #Line!Smalltalk removeClassNamed: #LineSegment!Smalltalk removeClassNamed: #LinearFit!Smalltalk removeClassNamed: #Location!Smalltalk removeClassNamed: #LogXCoordinateSystem!Smalltalk removeClassNamed: #MirroringInterpolatedFunction!Smalltalk removeClassNamed: #MollweideCoordinateSystem!Smalltalk removeClassNamed: #Morph!Smalltalk removeClassNamed: #Morphic3Plugin!Smalltalk removeClassNamed: #MorphicCanvas!Smalltalk removeClassNamed: #OldComplexBorder!Smalltalk removeClassNamed: #OrientedFillStyle!Smalltalk removeClassNamed: #OwnSpaceMorph!Smalltalk removeClassNamed: #Path!Smalltalk removeClassNamed: #RestrictedInterpolatedFunction!Smalltalk removeClassNamed: #ScrollingMorph!Smalltalk removeClassNamed: #SinusoidalCoordinateSystem!Smalltalk removeClassNamed: #SolidFillStyle!Smalltalk removeClassNamed: #SpaceViewCoordinateSystem!Smalltalk removeClassNamed: #Spline!Smalltalk removeClassNamed: #SplineInterpolator!Smalltalk removeClassNamed: #SplineInterpolator1Offset!