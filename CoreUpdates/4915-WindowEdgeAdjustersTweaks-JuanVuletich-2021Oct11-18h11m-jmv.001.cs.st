'From Cuis 5.0 [latest update: #4908] on 11 October 2021 at 7:04:32 pm'!

!WindowEdgeAdjustingMorph methodsFor: 'geometry' stamp: 'jmv 10/11/2021 18:36:56'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `0@0`! !


!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 10/11/2021 18:11:33'!
roundEdge: aRectangle border: borderWidth color: borderColor
	"NOP here"! !


!Morph methodsFor: 'geometry services' stamp: 'jmv 10/11/2021 18:18:53'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the WidgetMorph hierarchy should answer #true to wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	(aMorph displayBoundsIntersects: privateDisplayBounds) ifFalse: [
		^false ].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/11/2021 18:16:43'!
on: aRectangle ifCovered: partiallyOrTotallyCoveredBlock uncoveredPartsDo: uncoveredPartBlock else: notCoveredAtAllBlock
	"Find rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included.
	
	If aRectangle is not covered at all, or for whatever reason we don't know, evaluate notCoveredAtAllBlock.
	Othewise evaluate partiallyOrTotallyCoveredBlock once, and also evaluate uncoveredPartBlock for each part in aRectangle we don't cover."

	^ notCoveredAtAllBlock value! !

!Morph methodsFor: 'updating' stamp: 'jmv 10/11/2021 18:19:02'!
invalidateBounds
	"Report that the area occupied by this morph and submorphs should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self displayFullBounds ifNotNil: [ :r |
		"outsetBy: 1 due to anti aliasing in VectorCanvas"
		self invalidateDisplayRect: r for: nil ].
! !

!Morph methodsFor: 'updating' stamp: 'jmv 10/11/2021 18:19:08'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way.
	Notes:
		Area used before any change is reported here.
		Area used after any changes is reported when displayBounds are updated.
			See updateBoundsIn:addDamageTo: and senders.
	"

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self basicDisplayBounds ifNotNil: [ :b |
		"outsetBy: 1 due to anti aliasing in VectorCanvas"
		self invalidateDisplayRect: b for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/11/2021 18:19:14'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds |
	handBounds _ self displayBounds.
	fullBounds _ handBounds copy.
	self submorphsDo: [ :m |
		fullBounds updateMerging: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ (fullBounds translatedBy: self morphPosition-lastPos) updateMerging: handBounds ].
	^fullBounds encompassingIntegerRectangle! !


!WindowEdgeAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 10/11/2021 19:02:52'!
drawOn: aCanvas

	| c |
	(owner is: #SystemWindow) ifFalse: [
		^super drawOn: aCanvas ].

	"Use almost transparent, so effect on antialiasing for drawing exactly on top of Window is kept to a minimum."
	c _ owner windowFrameColor alpha: 0.1.
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
		aCanvas roundEdge: self morphLocalBounds border: owner borderWidth color: c ].! !

!WindowEdgeAdjustingMorph methodsFor: 'geometry services' stamp: 'jmv 10/11/2021 18:46:51'!
coversLocalPoint: aLocalPoint
	"We don't completely cover our bounds. Account for that."

	| sensitiveBorder |
	sensitiveBorder _ owner borderWidth.
	((self morphLocalBounds outsetBy: sensitiveBorder) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 10/11/2021 18:35:41'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: (self morphLocalBounds insetBy: 1.5) color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: windowFrameColor! !

!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 10/11/2021 18:20:52'!
on: aRectangle ifCovered: partiallyOrTotallyCoveredBlock uncoveredPartsDo: uncoveredPartBlock else: notCoveredAtAllBlock
	"Find rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included.
	
	If aRectangle is not covered at all, or for whatever reason we don't know, evaluate notCoveredAtAllBlock.
	Othewise evaluate partiallyOrTotallyCoveredBlock once, and also evaluate uncoveredPartBlock for each part in aRectangle we don't cover."

	 | bounds r e r2 |
	self isOrAnyOwnerIsRotated ifTrue: [
		^ notCoveredAtAllBlock value ].

	color mightBeTranslucent ifTrue: [
		^ notCoveredAtAllBlock value ].

	"Due to possible anti aliasing, and possible conversion to integer, we can't be really sure
	about the 2 outer pixels at each edge."
	bounds _ self displayBounds insetBy: 2.
	bounds ifNil: [
		^ notCoveredAtAllBlock value ].

	(aRectangle intersects: bounds) ifFalse: [
		^ notCoveredAtAllBlock value ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	partiallyOrTotallyCoveredBlock value.
	aRectangle areasOutside: bounds do: [ :rect | uncoveredPartBlock value: rect ].

	Theme current roundWindowCorners ifTrue: [
		r _ Theme current roundedWindowRadius * 1.1.										"A bit more than actual radius because we use Bezier, not arc."
		e _ self externalizeDistanceToWorld: r@r.
		r2 _ aRectangle intersect: (bounds topLeft extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds topRight - (e x@0) extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomLeft - (0@ e y)extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomRight - e extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		].! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 10/11/2021 19:02:41'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos halfThickness |
	thickness _ self borderWidth.
	halfThickness _ thickness * 0.5.
	cornerExtent _ thickness * 5.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@halfThickness extent: w@0.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-halfThickness) extent: w@0.
	(adjusters at: #leftAdjuster) morphPosition: halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #topLeftAdjuster) morphPosition: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: extent - pos - borderWidth ].

	self layoutNeeded: false.! !


!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/9/2020 15:28:39'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	"
	(BitBltCanvas onForm: Display)
		fillRectangle: (10@10 extent: 300@200) 
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised
		baseColorForBorder: Color red.
	Display forceToScreen.
	"

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/11/2021 18:18:35'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance, with VectorGraphics package loaded."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	"Include an extra pixel to cover possible anti aliasing."
	boundingRect _ boundingRect outsetBy: 1.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!Theme methodsFor: 'other options' stamp: 'jmv 12/7/2010 14:32'!
roundWindowCorners
	^true! !

!methodRemoval: BitBltCanvas #roundEdge:color: stamp: 'jmv 10/11/2021 18:12:24'!
BitBltCanvas removeSelector: #roundEdge:color:!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' drawOn: drawingFails drawingFailsNot fullyCovers: hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayBoundsSetFrom: displayBoundsUpdateFrom: displayFullBounds extentChanged: externalize: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale)
('geometry services' collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: displayBoundsIntersects: fullOwnsOrCoversPixel: isCloserThan:toPixel: on:ifCovered:uncoveredPartsDo:else: ownsOrCoversPixel: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!WindowEdgeAdjustingMorph reorganize!
('initialization' initializeBottom initializeBottomLeft initializeBottomRight initializeLeft initializeRight initializeTop initializeTopLeft initializeTopRight)
('adjusting' adjustOwnerAt:millisecondSinceLast: basicAdjustOwnerAt:)
('accessing' cursor)
('drawing' drawOn:)
('geometry' minimumExtent)
('geometry services' coversLocalPoint:)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' on:ifCovered:uncoveredPartsDo:else:)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor fullScreen makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('resize/collapse' allowedArea expandBoxHit resize resize: resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing')
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!

