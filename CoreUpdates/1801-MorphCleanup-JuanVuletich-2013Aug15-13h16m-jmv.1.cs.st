'From Cuis 4.2 of 25 July 2013 [latest update: #1800] on 15 August 2013 at 1:33:48.100044 pm'!

!Morph methodsFor: 'events-alarms' stamp: 'jmv 8/15/2013 13:32'!
addAlarm: aSelector withArguments: args after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"

	self alarmScheduler ifNotNil: [ :scheduler |
		scheduler
			addAlarm: aSelector
			withArguments: args
			for: self
			at: Time localMillisecondClock + delayTime ]! !

!methodRemoval: HaloMorph #morphicLayerNumber!
HaloMorph removeSelector: #morphicLayerNumber!
!methodRemoval: FillInTheBlankMorph #morphicLayerNumber!
FillInTheBlankMorph removeSelector: #morphicLayerNumber!
!methodRemoval: PasteUpMorph #addMorphFront:!
PasteUpMorph removeSelector: #addMorphFront:!
!methodRemoval: MenuMorph #morphicLayerNumber!
MenuMorph removeSelector: #morphicLayerNumber!
!methodRemoval: Morph #addAlarm:at:!
Morph removeSelector: #addAlarm:at:!
!methodRemoval: Morph #addAlarm:with:at:!
Morph removeSelector: #addAlarm:with:at:!
!methodRemoval: Morph #addAlarm:with:with:after:!
Morph removeSelector: #addAlarm:with:with:after:!
!methodRemoval: Morph #addAlarm:with:with:at:!
Morph removeSelector: #addAlarm:with:with:at:!
!methodRemoval: Morph #addAlarm:withArguments:at:!
Morph removeSelector: #addAlarm:withArguments:at:!
!methodRemoval: Morph #addMorphInFrontOfLayer:!
Morph removeSelector: #addMorphInFrontOfLayer:!
!methodRemoval: Morph #addMorphInLayer:!
Morph removeSelector: #addMorphInLayer:!
!methodRemoval: Morph #allMorphs!
Morph removeSelector: #allMorphs!
!methodRemoval: Morph #allOwners!
Morph removeSelector: #allOwners!
!methodRemoval: Morph #altSpecialCursor0!
Morph removeSelector: #altSpecialCursor0!
!methodRemoval: Morph #altSpecialCursor1!
Morph removeSelector: #altSpecialCursor1!
!methodRemoval: Morph #altSpecialCursor2!
Morph removeSelector: #altSpecialCursor2!
!methodRemoval: Morph #altSpecialCursor3!
Morph removeSelector: #altSpecialCursor3!
!methodRemoval: Morph #altSpecialCursor3:!
Morph removeSelector: #altSpecialCursor3:!
!methodRemoval: Morph #findA:!
Morph removeSelector: #findA:!
!methodRemoval: Morph #morphicLayerNumber!
Morph removeSelector: #morphicLayerNumber!
!methodRemoval: Morph #morphicLayerNumberWithin:!
Morph removeSelector: #morphicLayerNumberWithin:!

!Morph reorganize!
('accessing' adoptWidgetsColor: balloonText beSticky color isLocked isSticky location lock lock: resistsRemoval sticky: toggleStickiness unlock unlockContents)
('accessing - extension' assureExtension hasExtension privateExtension: resetExtension)
('accessing - properties' hasProperty: removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits dragEvent:localPosition: rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: clipsSubmorphs drawErrorOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hide imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseLeave: mouseMove:localPosition: mouseStillDown: windowEvent:)
('event handling testing' allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseStillDown:localPosition: processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus keyboardFocusChange:)
('geometry' clippingRect externalize: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: internalize: internalizeDistance: internalizeFromWorld: layoutBounds minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphFullBoundsInWorld morphHeight morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphWidth worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' fullContainsPoint: isOrthoRectangularMorph morphContainsPoint:)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec:)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor collapse expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' nameForFindWindowFeature)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('property extension' extension)
('rotate scale and flex' rotationDegrees)
('stepping' wantsSteps)
('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: shouldGetStepsFrom: startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)
('structure' allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld owner ownerThatIsA: pasteUpMorph root veryLastLeave withAllOwnersDo: world)
('submorphs-accessing' allMorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOpaqueMorph isReallyVisible stepTime)
('updating' redrawNeeded update:)
('user interface' activateWindow activateWindowAndSendTopToBack:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

