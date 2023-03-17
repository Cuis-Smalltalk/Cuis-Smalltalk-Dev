'From Cuis 6.0 [latest update: #5699] on 17 March 2023 at 6:26:16 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 10:24:08'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations.
	The answer should be the largest fully covered rectangle we can know cheaply."

	^nil! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 17:42:14'!
on: paramRectangle fullyCoveredRect: fullyCoveredRectBlock partiallyCoveredPartsDo: partiallyCoveredRectsBlock uncoveredRectsDo: uncoveredRectsBlock
	"Find out about aRectangle and us.
	- If paramRectangle contains a rectangle that is fully covered by us, evaluate fullyCoveredRectBlock with it.
	- For parts of paramRectangle we are certain we won't draw on, evaluate uncoveredRectsBlock
	- The rest of paramRectangle comprises areas which we may cover partially, or can't really be certain
		 Evaluate partiallyCoveredRectsBlock on them.
	Note: The found rectangles, passed to the various blocks, form a partition of paramRectangle.
		This means that all parts of paramRectangle are sent to some block. It also means that no part
		is sent to two different blocks.
	Note: The rectangles passed to argument blocks are new instances. It is OK to modify them as needed.
	Note: if fullyCoveredRectBlock is evaluated, it must be evaluated before any of the other blocks. Senders rely on this!!
	This implementation is correct, although simplistic.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations."

	"No intersection? Safe answer."
	(paramRectangle intersects: self displayBounds) ifFalse: [
		uncoveredRectsBlock value: paramRectangle copy.
		^self ].

	"We know no better"
	partiallyCoveredRectsBlock value: paramRectangle copy.! !


!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 10:57:18'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	This implementation is a good optimization for many common cases."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	b := self displayBounds.
	b isNil ifTrue: [
		^nil ].

	e := Theme current roundWindowCorners
		ifFalse: [
			"Due to possible anti aliasing, and possible conversion to integer, we can't
			really be sure 	about the 2 outer pixels at each edge."
			`2@2` ]
		ifTrue: [
			"Add 2 pixels in y direction for anti aliasing and rounding."
			(self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling+2 ].
	^ b insetBy: e.! !

!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 17:42:09'!
on: paramRectangle fullyCoveredRect: fullyCoveredRectBlock partiallyCoveredPartsDo: partiallyCoveredRectsBlock uncoveredRectsDo: uncoveredRectsBlock
	"Find out about aRectangle and us.
	- If paramRectangle contains a rectangle that is fully covered by us, evaluate fullyCoveredRectBlock with it.
	- For parts of paramRectangle we are certain we won't draw on, evaluate uncoveredRectsBlock
	- The rest of paramRectangle comprises areas which we may cover partially, or can't really be certain
		 Evaluate partiallyCoveredRectsBlock on them.
	Note: The found rectangles, passed to the various blocks, form a partition of paramRectangle.
		This means that all parts of paramRectangle are sent to some block. It also means that no part
		is sent to two different blocks.
	Note: The rectangles passed to argument blocks are new instances. It is OK to modify them as needed.
	Note: if fullyCoveredRectBlock is evaluated, it must be evaluated before any of the other blocks. Senders rely on this!!
	This implementation is a good optimization for many common cases."

	| displayBounds intersectionWithBounds fullyCovered intersectionWithFullyCovered |
	displayBounds := self displayBounds.	

	"No intersection? Safe answer."
	(paramRectangle intersects: displayBounds) ifFalse: [
		uncoveredRectsBlock value: paramRectangle copy.
		^self ].

	"So, we have some intersection. Use it."
	intersectionWithBounds := paramRectangle intersect: displayBounds.
	fullyCovered := self fullyCoveredRectangle.

	"No part is fully covered. It is just partially covered."
	(fullyCovered isNil or: [ (intersectionWithBounds intersects: fullyCovered) not ]) ifTrue: [
		partiallyCoveredRectsBlock value: intersectionWithBounds.
		paramRectangle areasOutside: intersectionWithBounds do: [ :rect |
			uncoveredRectsBlock value: rect ].
		^self ].

	"So we have some fully covered part."
	intersectionWithFullyCovered := intersectionWithBounds intersect: fullyCovered.
	fullyCoveredRectBlock value: intersectionWithFullyCovered.
	intersectionWithBounds areasOutside: intersectionWithFullyCovered do: [ :rect |
		partiallyCoveredRectsBlock value: rect ].
	paramRectangle areasOutside: intersectionWithBounds do: [ :rect |
		uncoveredRectsBlock value: rect ].! !


!Morph methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 10:31:25'!
fullyCovers: aRectangle
	"Answer whether our drawing completely covers aRectangle.
	Answer true only if we are certain."

	^ self fullyCoveredRectangle
		ifNotNil: [ :r | r containsRect: aRectangle ]
		ifNil: [false].! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky boundsFinderCanvas canvas color layerNumber location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError showBitMask showContour)
('drawing' drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: imageFormVG:depth: isKnownFailing postDrawOn: show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: invalidDrop: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange: keyboardFocused)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' bitMask collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: fullyCoveredRectangle fullyCovers: includesPixel: isCloserThan:toPixel: on:fullyCoveredRect:partiallyCoveredPartsDo:uncoveredRectsDo: on:ifCovered:uncoveredPartsDo:else: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo: wantsRecolorHandle)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addLabel:font:centeredAt:rotationDegrees: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' canDiscardEdits disregardUnacceptedEdits drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse embeddedInMorphicWindowLabeled: flash flashWith: showAndComeToFront unlockOneSubpart whenUIinSafeState:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

