'From Cuis 5.0 [latest update: #4749] on 13 August 2021 at 11:45:30 am'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:09:34'!
isOrAnyOwnerIsRotated
	"True if we or any owner is rotated."

	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:26:48'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: to explore affected pixels to deduce actual morph bounds. See senders."

	^false! !


!MovableMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:17:24'!
isOrAnyOwnerIsRotated
	"True if we or any owner is rotated.
	A zoomed widget will answer false, but a rotated one will answer true (even if only
		some owner is rotated).
	Note: unrotated SystemWindow answer true, but they implements
		#addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^ location doesNotRotate not or: [ owner notNil and: [ owner isOrAnyOwnerIsRotated ]].! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/13/2021 11:44:24'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use. NOP here. See super implementation.
	See also #knowsOwnLocalBounds and #wantsContour."! !

!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:26:52'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: to explore affected pixels to deduce actual morph bounds. See senders."

	^true! !


!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:26:58'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: to explore affected pixels to deduce actual morph bounds. See senders."

	^true! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/13/2021 11:14:36'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."

	aCollection add: aRectangle.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/13/2021 11:41:56'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				self knowsOwnLocalBounds
					ifTrue: [
						"A good optimization in many cases.
						But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
						for example) this is essential."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds]
					ifFalse: [
						"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self wantsContour
							ifTrue: [
								 (aCanvas
									contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
									into: [ :contourArray :contourY0 :contourY1 |
										self setProperty: #contour toValue: contourArray.
										self setProperty: #contourY0 toValue: contourY0.
										self setProperty: #contourY1 toValue: contourY1.
								])]
							ifFalse: [
								self setProperty: #contour toValue: nil.
								self setProperty: #contourY0 toValue: nil.
								self setProperty: #contourY1 toValue: nil ]]]]].! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:35:01'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If False, we can be drawn by BitBltCanvas."

	^true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:37:25'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ true! !


!KernelMorph methodsFor: 'geometry services' stamp: 'jmv 8/13/2021 10:59:28'!
morphContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in a submorph in front of us, as long as it is inside our shape."

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:31:43'!
requiresVectorCanvas
	"Kernel morphs can run with any kind of Canvas"

	^false! !

!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:37:43'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ false! !

!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:43:48'!
wantsContour
	"Kernel morphs don't need contour.
	See also #knowsOwnLocalBounds and senders."

	^false! !


!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:34:48'!
requiresVectorCanvas
	"Prefer VectorGraphics halos and handled for morphs drawn with VectorCanvas."

	target ifNotNil: [
		^target requiresVectorCanvas ].
	^false! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/13/2021 11:44:27'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use. NOP here. See super implementation.
	See also #knowsOwnLocalBounds and #wantsContour."! !

!WidgetMorph methodsFor: 'geometry services' stamp: 'jmv 8/13/2021 10:59:33'!
morphContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in a submorph in front of us, as long as it is inside our shape."

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:34:13'!
requiresVectorCanvas
	"Widgets can usually run with any kind of Canvas, but not if zoomed or rotated.
	Note: Subclasses that use VectorGraphics for their drawing should answer true."

	^ location isPureTranslation not or: [ owner notNil and: [ owner requiresVectorCanvas ]].! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 10:37:56'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ false! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 11:43:56'!
wantsContour
	"Widget morphs don't need contour.
	See also #knowsOwnLocalBounds and senders."

	^false! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 8/13/2021 11:13:41'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		Preferences cheapWindowReframe or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtent ]].! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/13/2021 11:13:25'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."

	 | bounds radius |
	self isOrAnyOwnerIsRotated ifTrue: [
		aCollection add: aRectangle.
		^self ].

	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self displayBounds.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report left and right columns as uncovered areas"
	radius _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: radius@0) do: [ :rect |  aCollection add: rect ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/13/2021 11:27:25'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph knowsOwnLocalBounds
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]].
			(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not]) ifTrue: [
				currentMorph submorphsDo: [ :m |
					self fullUpdateProtrudingBounds: m ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/13/2021 11:27:37'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph knowsOwnLocalBounds
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 8/13/2021 11:01:03'!
morph: aMorph isAtPoint: aPoint

	aMorph basicDisplayBounds ifNotNil: [ :r |
		(r containsPoint: aPoint) ifFalse: [
			^false ]].
	"Give morphs with a non-rectangular shape (corner WindowEdgeAdjustingMorphs)
	a chance to have a say."
	^ aMorph morphContainsPoint:
		(aMorph internalizeFromWorld: aPoint)! !

!methodRemoval: PluggableButtonMorph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:44'!
PluggableButtonMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: WindowEdgeAdjustingMorph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:45'!
WindowEdgeAdjustingMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: WidgetMorph #displayBoundsSetFrom: stamp: 'jmv 8/13/2021 11:42:04'!
WidgetMorph removeSelector: #displayBoundsSetFrom:!
!methodRemoval: WidgetMorph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:40'!
WidgetMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: HaloMorph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:44'!
HaloMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: HaloMorph #morphContainsPoint: stamp: 'jmv 8/13/2021 11:33:05'!
HaloMorph removeSelector: #morphContainsPoint:!
!methodRemoval: KernelMorph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:44'!
KernelMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: Morph #isOrthoRectangularMorph stamp: 'jmv 8/13/2021 11:32:44'!
Morph removeSelector: #isOrthoRectangularMorph!

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
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayBoundsOrBogus displayBoundsSetFrom: displayBoundsUpdateFrom: displayFullBounds extentChanged: externalize: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale)
('geometry services' collides: containsGlobalPoint: contourIntersects:top:bottom: displayBoundsIntersects: fullContainsGlobalPoint: isCloserThan:toPoint:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
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


!KernelMorph reorganize!
('accessing' color color:)
('initialization' defaultColor initialize)
('drawing' drawOn:)
('geometry' displayBoundsUpdateFrom: morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphWidth morphWidth: privateExtent: rotationCenter)
('geometry services' morphContainsPoint:)
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
!


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry services' containsGlobalPoint:)
('geometry testing' requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
!


!WidgetMorph reorganize!
('accessing' borderColor borderColor: borderWidth borderWidth: color color:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor initialize noBorder)
('drawing' drawOn:)
('geometry' displayBoundsUpdateFrom: ensureMinimimExtent extentBorder morphBottomLeft morphBottomRight morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphTopLeft morphTopRight morphWidth morphWidth: privateExtent: rotationCenter)
('geometry services' morphContainsPoint:)
('geometry testing' hasVariableExtent knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('layout' minItemWidth)
('layout-properties' layoutSpec:)
('testing' is:)
!


!WindowEdgeAdjustingMorph reorganize!
('initialization' initializeBottom initializeBottomLeft initializeBottomRight initializeLeft initializeRight initializeTop initializeTopLeft initializeTopRight)
('adjusting' adjustOwnerAt:millisecondSinceLast: basicAdjustOwnerAt:)
('accessing' cursor)
('drawing' drawOn:)
('geometry services' morphContainsPoint:)
!

