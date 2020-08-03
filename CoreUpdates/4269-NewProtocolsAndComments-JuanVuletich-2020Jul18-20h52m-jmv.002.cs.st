'From Cuis 5.0 [latest update: #4262] on 18 July 2020 at 9:43:11 pm'!

!RectangleLikeMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such.!


!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:00'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If false, we occupy a Rectangle in local coordinates. Allows for many optimizations to be done.
	Note: If answer is false, but some owner requiresVectorCanvas, then we also do. Canvas takes care of this.
	Note: SystemWindow answers false, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:38:32'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their submorphs tree, will lie inside their bounds,
	either 'naturally' (by construction) or by being clipped by their owner (#clipsLastSubmorph). Allows for many optimizations to be done."

	^true! !


!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:08'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If false, we occupy a Rectangle in local coordinates. Allows for many optimizations to be done.
	Note: If answer is false, but some owner requiresVectorCanvas, then we also do. Canvas takes care of this.
	Note: SystemWindow answers false, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^location isPureTranslation not! !

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:38:44'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their submorphs tree, will lie inside their bounds,
	either 'naturally' (by construction) or by being clipped by their owner (#clipsLastSubmorph). Allows for many optimizations to be done."

	^false! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:37:13'!
clipsLastSubmorph
	"Answer true if we clip the shape of our last submorph to ours.
	Answer true only when clipping by the canvas is needed."

	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:45'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^false! !


!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:50'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^true! !


!PluggableScrollPane methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:37:41'!
clipsLastSubmorph
	"Answer true if we clip the shape of our last submorph to ours.
	Answer true only when clipping by the canvas is needed."

	^ true! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:32:52'!
isOrthoRectangularMorph
	^false! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees: whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:from: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing refreshWorld show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropFiles:localPosition: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' allocHeightForFactor: allocWidthForFactor: extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent minimumLayoutExtent morphAlign:with: morphBounds morphBounds: morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphFullBoundsInWorld morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth rotateBy: rotation:scale: scaleBy: worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' clipsLastSubmorph fullContainsPoint: isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
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
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOpaqueMorph isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: verticalScrollBar)
('access options' hideScrollBarsIndefinitely)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: focusIndicatorBottom focusIndicatorExtent focusIndicatorLeft focusIndicatorRectangle focusIndicatorRight focusIndicatorTop hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent privateExtent: scrollDeltaHeight scrollDeltaWidth scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableArea viewableAreaBottom viewableAreaLeft viewableAreaRight viewableAreaTop viewableAreaTopLeft viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' clipsLastSubmorph hIsScrollbarShowing vIsScrollbarShowing)
('initialization' initialize innerMorphClass scrollBarClass)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' is:)
!


!WindowEdgeAdjustingMorph reorganize!
('initialization' initializeBottom initializeBottomLeft initializeBottomRight initializeLeft initializeRight initializeTop initializeTopLeft initializeTopRight)
('adjusting' adjustOwnerAt:)
('accessing' cursor)
('drawing' drawOn:)
('testing' isOpaqueMorph)
('geometry testing' isOrthoRectangularMorph morphContainsPoint:)
!

