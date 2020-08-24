'From Cuis 5.0 [latest update: #4360] on 22 August 2020 at 4:48:30 pm'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/22/2020 16:40:53'!
acceptDroppingMorph: aMorph event: evt
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:event: message. This default implementation just adds the given morph to the receiver."
	
	self addMorph: aMorph! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:35:34'!
morphExtent
	"In our own coordinates!!"

	^ extent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:36:56'!
morphHeight: aNumber

	self morphExtent: extent x@aNumber! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:37:29'!
morphWidth: aNumber

	self morphExtent: aNumber@extent y! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees: whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:fromSubmorph:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropFiles:localPosition: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' allocHeightForFactor: allocWidthForFactor: displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent minimumLayoutExtent morphAlign:with: morphBounds morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth rotateBy: rotation:scale: scaleBy:)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsDo: clippedSubmorph findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsDrawingOutsideReverseDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying: unclippedSubmorphsReverseDo:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

