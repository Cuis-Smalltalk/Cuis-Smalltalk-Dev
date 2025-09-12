'From Cuis7.5 [latest update: #7539] on 12 September 2025 at 1:09:36 pm'!

!Morph methodsFor: 'active model behavior' stamp: 'HAW 9/12/2025 13:08:37'!
actionMap

	^self valueOfProperty: #actionMap ifAbsent: nil! !

!Morph methodsFor: 'active model behavior' stamp: 'HAW 9/12/2025 13:08:41'!
postCopy

	super postCopy.
	self removeAllActions! !

!Morph methodsFor: 'active model behavior' stamp: 'HAW 9/12/2025 13:09:03'!
releaseActionMap

	self removeAllActions! !

!Morph methodsFor: 'active model behavior' stamp: 'HAW 9/12/2025 13:09:07'!
removeAllActions

	self removeProperty: #actionMap! !

!Morph methodsFor: 'active model behavior' stamp: 'HAW 9/12/2025 13:09:11'!
updateableActionMap

	^self valueOfProperty: #actionMap ifAbsentPut: [ IdentityDictionary new ] ! !


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
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsToBeDroppedInto:)
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
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allMorphsFrontToBackDo: allSubmorphsDo: allSubmorphsFrontToBackDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying: topMorphAt: topUnlockedMorphAt:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addLabel:font:centeredAt:rotationDegrees: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront clearDependencyAndEvents comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('submorphs-menus' addItemTo:fromSpec:submenus: addLineTo:fromSpec:submenus: addSubmenuTo:fromSpec:submenus: addTitleTo:fromSpec: allMenuNshortcutsSpecs getMenu menuNshortcutsKey menuNshortcutsSpecsDo: menuNshortcutsSpecsFor:do:)
('testing' canDiscardEdits disregardUnacceptedEdits drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isPlacedMorph isProportionalHeight isProportionalWidth isReallyUnlocked isReallyVisible resistsRemoval)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse embeddedInMorphicWindowLabeled: flash flashWith: showAndComeToFront unlockOneSubpart whenUIinSafeState:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMove:toIndex: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
('active model behavior' addAssignToInstanceVariablePropertyTo: addGenerateGetterTo: addWidgetNamePropertyTo: propertiesEditorMorph: addChildWidget: allowAddingChildWidgets allowsAddingChildWidget: allowsAddingChildWidgetPropertyName assertCanAddChildWidget: canAddChildWidget: disallowAddingChildWidgets notifyCannotAddChildWidget actionMap postCopy releaseActionMap removeAllActions updateableActionMap)
!

