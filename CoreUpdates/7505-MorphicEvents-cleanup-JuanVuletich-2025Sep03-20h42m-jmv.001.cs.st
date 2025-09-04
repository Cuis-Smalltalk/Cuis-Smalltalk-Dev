'From Cuis7.5 [latest update: #7502] on 3 September 2025 at 9:01:44 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/3/2025 20:59:50'!
noticeMouseOver: aMorph event: aMouseOverEvent
	"Remember that the mouse is currently over some morph"
	leftMorphs ifNil: [ ^self ].		"Might happen if you halt during layout."
	(leftMorphs includes: aMorph) 
		ifTrue: [ leftMorphs remove: aMorph ]
		ifFalse: [ enteredMorphs nextPut: aMorph ].
	overMorphs nextPut: aMorph.! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/3/2025 20:48:41'!
acceptDroppingMorph: aMorph event: aDropEvent
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:event: message. This default implementation just adds the given morph to the receiver."
	
	self addMorph: aMorph! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/3/2025 20:48:20'!
justDroppedInto: newOwnerMorph event: aDropEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	newOwnerMorph activateWindow! !

!Morph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:49:14'!
keyUp: aKeyboardEvent
	"Handle a key up event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler | handler value: aKeyboardEvent ]! !

!Morph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:09'!
mouseEnter: aMouseEnterEvent
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseEnter: 
		ifPresentDo: [ :handler | handler value: aMouseEnterEvent ]! !

!Morph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:47:01'!
mouseHover: aMouseOverEvent localPosition: localEventPosition
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled.
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #mouseHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseOverEvent
				value: localEventPosition ].! !

!Morph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:55:43'!
mouseLeave: aMouseLeaveEvent
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	(Preferences at: #focusFollowsMouse)
		ifTrue: [aMouseLeaveEvent hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:44:29'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseOverEvent) not and: [self fullIncludesPixel: aMouseOverEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseOverEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:45:14'!
processMouseScroll: aMouseScrollEvent localPosition: localEventPosition
	((self handlesMouseScroll: aMouseScrollEvent) and: [ aMouseScrollEvent wasHandled not ]) ifTrue: [
		self
			mouseScroll: aMouseScrollEvent
			localPosition: localEventPosition.
		aMouseScrollEvent wasHandled: true ].! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 9/3/2025 20:45:45'!
mouseScroll: aMouseScrollEvent localPosition: localEventPosition
	aMouseScrollEvent direction
		caseOf: {
			[ #up ] 		-> 	[ angle := angle + 0.02. self redrawNeeded ].
			[ #down ] 	-> 	[ angle := (angle - 0.02) max: 0. self redrawNeeded ].
			[ #left ] 	-> 	[ depthLimit := depthLimit - 1 max: 1. self redrawNeeded].
			[ #right ] 	-> 	[ depthLimit := depthLimit + 1 min: 15. self redrawNeeded]
		}! !


!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:16'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ aMouseEnterEvent hand newKeyboardFocus: self ]! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:20'!
mouseEnter: aMouseEnterEvent
	"The mouse entered the receiver"
	owner ifNil: [ ^self ].
	owner selectItem: self! !


!IndentingListItemMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:25'!
mouseEnter: aMouseEnterEvent
	isHighlighted := true.
	self redrawNeeded.
	^super mouseEnter: aMouseEnterEvent! !

!IndentingListItemMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:55:49'!
mouseLeave: aMouseLeaveEvent
	isHighlighted := false.
	self redrawNeeded.
	^super mouseLeave: aMouseLeaveEvent! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:31'!
mouseEnter: aMouseEnterEvent
	"The mouse entered the receiver"
	mouseIsOver := true.
	self redrawNeeded.
	^super mouseEnter: aMouseEnterEvent! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:55:54'!
mouseLeave: aMouseLeaveEvent
	"The mouse has left the area of the receiver"
	mouseIsOver := false.
	self redrawNeeded.
	^super mouseLeave: aMouseLeaveEvent! !


!HoverableButtonMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:36'!
mouseEnter: aMouseEnterEvent
	
	mouseEnterSelector ifNotNil: [ model perform: mouseEnterSelector ].
	^super mouseEnter: aMouseEnterEvent! !

!HoverableButtonMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:56:18'!
mouseLeave: aMouseLeaveEvent
	
	mouseLeaveSelector ifNotNil: [
		model perform: mouseLeaveSelector ].
	^super mouseLeave: aMouseLeaveEvent! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 9/3/2025 20:45:52'!
mouseScroll: aMouseScrollEvent localPosition: localEventPosition
	aMouseScrollEvent direction
		caseOf: {
			[ #up ] 		-> 		[ scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 	-> 		[ hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[ hScrollBar scrollDown: 1 ] }! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:43'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ aMouseEnterEvent hand newKeyboardFocus: self ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:50'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ aMouseEnterEvent hand newKeyboardFocus: self ]! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:56:07'!
mouseLeave: aMouseLeaveEvent
	super mouseLeave: aMouseLeaveEvent.
	scroller highlightedRow: nil! !


!TextModelMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:54:57'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ aMouseEnterEvent hand newKeyboardFocus: scroller ]! !


!TileResizeMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:47:09'!
mouseHover: aMouseOverEvent localPosition: localEventPosition
	self selectResize: localEventPosition! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:55:02'!
mouseEnter: aMouseEnterEvent
	self activeHand newKeyboardFocus: completer textMorph.
	^ super mouseEnter: aMouseEnterEvent! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:55:09'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	self cursor activateCursor.
	hand := aMouseEnterEvent hand! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:56:12'!
mouseLeave: aMouseLeaveEvent
	super mouseLeave: aMouseLeaveEvent.
	hand ifNotNil: [
		hand := nil.
		Cursor defaultCursor activateCursor ].! !


!ResizeMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:57:23'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	self selectTo: localEventPosition! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 9/3/2025 20:59:31'!
noticeMouseOver: aMorph event: aMouseOverEvent
	mouseOverHandler ifNil: [ ^self ].
	mouseOverHandler noticeMouseOver: aMorph event: aMouseOverEvent.! !


!DraggingGuideMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/3/2025 20:48:26'!
justDroppedInto: newOwnerMorph event: aDropEvent 
	
	self delete.
	aDropEvent hand redrawNeeded.! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color defaultLayerNumber layerNumber location location: lock mainBoundsFinderCanvas mainCanvas morphId taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError showBitMask showContour)
('drawing' drawOn: drawingFails drawingFailsNot hide icon imageForm imageForm: imageForm:depth: imageFormVG:depth: isKnownFailing postDrawOn: show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: keyboardShortcut: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown: tabToFocus)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' focusKeyboardFor: handleMouseFocusEvent: invalidDrop: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange: keyboardFocused)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds ensureMinimumExtent extentInOwner externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: localBoundsForError minimumExtent morphExtent morphPosition morphPosition: positionInWorld positionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' bitMask collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: fullyCoveredRectangle includesPixel: isCloserThan:toPixel: ownsPixel:)
('geometry testing' clipsSubmorphs hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloRedrawMayBeNeeded haloShowsCoordinateSystem haloSpecifications mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo: wantsRecolorHandle)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphsIfNeeded maximumLayoutHeight minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf window withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allMorphsFrontToBackDo: allSubmorphsDo: allSubmorphsFrontToBackDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphVisibleAt: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addLabel:font:centeredAt:rotationDegrees: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront clearDependencyAndEvents comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('submorphs-menus' addItemTo:fromSpec:submenus: addLineTo:fromSpec:submenus: addSubmenuTo:fromSpec:submenus: addTitleTo:fromSpec: allMenuNshortcutsSpecs getMenu menuNshortcutsKey menuNshortcutsSpecsDo: menuNshortcutsSpecsFor:do:)
('testing' canDiscardEdits disregardUnacceptedEdits drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isPlacedMorph isProportionalHeight isProportionalWidth isReallyVisible resistsRemoval)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse embeddedInMorphicWindowLabeled: flash flashWith: showAndComeToFront unlockOneSubpart whenUIinSafeState:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMove:toIndex: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

