'From Cuis 6.0 [latest update: #5091] on 6 March 2022 at 7:04:57 pm'!

!Morph methodsFor: '*ui-draganddrop' stamp: 'KenD 3/4/2022 12:56:00'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, 
	 which is being dropped by a hand in response to the given event. 
	Note that for a successful drop operation both parties need to agree. 
	The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	 Individual Morpks may override by setting the corresponding property
	 to an appropriate two argument closure."

	^self valueOfProperty: #wantsDroppedMorph:event:
		 ifPresentDo: [ :wantsMorphEvt | wantsMorphEvt value: aMorph value: evt ]
		 ifAbsent: [ true ]! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky boundsFinderCanvas canvas color layerNumber location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' drawOn: drawingFails drawingFailsNot fullyCovers: hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: includesPixel: isCloserThan:toPixel: on:ifCovered:uncoveredPartsDo:else: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
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
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

