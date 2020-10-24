'From Cuis 5.0 [latest update: #4414] on 21 October 2020 at 3:03:43 pm'!

!Morph commentStamp: 'jmv 10/21/2020 14:49:13' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (a WorldMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets 
drawn. To hide a Morph and its submorphs, send the #visible: message. 

Every morph has a local coordinate system to interpret positions.
Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.

Events are delivered to morphs in Z-order, i.e. if a morph occludes another the event is only delivered to the foremost (just like physical objects). Events received by a morph carry positions in the local coordinate system.

Morphs can be translated by an offset, rotated around their center, orbited (rotated around the owner center), and zoomed (i.e. scaled).

Every morph has an associated transformation that defines the inner space where the morph is drawn and where the submorphs live. These transformations don't change anything from the internal point of view of the morph.

Structure:
instance var 				Type 						Description 
owner 				Morph		 				My parent Morph, or nil for the top-level Morph, which is a
 				or nil						world, typically a PasteUpMorph.
submorphs 				Array 						My child Morphs. 
location 				GeometryTransformation 						Specifies position (and possibly, angle of rotation and scale change) inside owner
												See comment at GeometryTransformation!


!Morph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 15:01:06'!
orbitBy: radians
	"Change the scale of this morph. Arguments are an angle and a scale."
	location _ (AffineTransformation withRadians: radians) composedWith: location.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 14:56:59'!
rotation: radians
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

    self rotation: radians scale: location scale.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 14:56:28'!
rotation: radians scale: scale
	"Change the rotation and scale of this morph. Arguments are an angle and a scale."
	location _ location withRotation: radians scale: scale.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
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
('geometry' allocHeightForFactor: allocWidthForFactor: displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent minimumLayoutExtent morphAlign:with: morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth orbitBy: rotateBy: rotation: rotation:scale: rotationDegrees: scaleBy:)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
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

