'From Cuis 5.0 [latest update: #4475] on 24 November 2020 at 12:18:10 pm'!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset scaleOffset haloBox haloDelta '
	classVariableNames: 'Icons '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!Morph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 11:40:08'!
rotation
	^0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 11:39:08'!
rotationDegrees: degrees
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

	self rotation: degrees degreesToRadians! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 11:41:25'!
rotation
	^location radians! !


!Number methodsFor: 'truncation and round off' stamp: 'di 2/19/98 21:58'!
detentBy: detent atMultiplesOf: grid snap: snap
	"Map all values that are within detent/2 of any multiple of grid to that multiple.  Otherwise, if snap is true, return self, meaning that the values in the dead zone will never be returned.  If snap is false, then expand the range between dead zones so that it covers the range between multiples of the grid, and scale the value by that factor."
	| r1 r2 |
	r1 _ self roundTo: grid.  "Nearest multiple of grid"
	(self - r1) abs <= detent ifTrue: [^ r1].  "Snap to that multiple..."
	snap ifTrue: [^ self].  "...or return self"

	r2 _ self < r1  "Nearest end of dead zone"
		ifTrue: [r1 - (detent asFloat/2)]
		ifFalse: [r1 + (detent asFloat/2)].
	"Scale values between dead zones to fill range between multiples"
	^ r1 + ((self - r2) * grid asFloat / (grid - detent))
"
	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: true] 	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: false]
	(3.9 to: 4.1 by: 0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: true] 	(-3.9 to: -4.1 by: -0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: false]
"! !


!Morph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 10:33:48'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"

	self displayBounds ifNotNil: [ :r | r center ].
	^0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 11:40:19'!
rotationDegrees

	self rotation radiansToDegrees! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 11/24/2020 12:17:21'!
removeAllMorphs
	| oldMorphs |
	submorphs isEmpty ifTrue: [ ^self ].
	submorphs do: [ :m |
		m invalidateBounds.
		m privateOwner: nil ].
	oldMorphs _ submorphs.
	submorphs _ #().
	oldMorphs do: [ :m |
		self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.
! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 11/24/2020 11:31:31'!
removeMorph: aMorph
	"Remove the given morph from my submorphs"

	aMorph owner == self ifFalse: [^self].
	aMorph invalidateBounds.
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
	self someSubmorphPositionOrExtentChanged.
! !

!Morph methodsFor: 'updating' stamp: 'jmv 11/24/2020 12:16:24'!
invalidateBounds
	"Report that the area occupied by this morph and submorphs should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self displayFullBounds ifNotNil: [ :r | self invalidateDisplayRect: r for: nil ].
! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 10:33:58'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	self displayBounds ifNotNil: [ :r | r center ].
	^self morphPositionInWorld! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 11/24/2020 10:34:03'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	self displayBounds ifNotNil: [ :r | r center ].
	^self morphExtentInWorld // 2 + self morphPositionInWorld! !


!HaloMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 11/24/2020 11:17:47'!
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	| p |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	p _ target morphPositionInWorld.
	positionOffset _ dragHandle referencePosition - p.
	haloDelta _ self morphPositionInWorld - p.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/24/2020 11:31:40'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	target displayBoundsForHalo ifNotNil: [ :r |
		self morphBounds: r.  "update my size"
		haloBox _ self basicBox.
		target addHandlesTo: self box: haloBox.
		self addNameString: (target printStringLimitedTo: 40).
		self redrawNeeded ].
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/24/2020 11:17:51'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPositionInWorld: thePoint.
	self morphPositionInWorld: thePoint + haloDelta.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/24/2020 11:55:32'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians |
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta + angleOffset.
	radians _ radians detentBy: 0.05 atMultiplesOf: Float pi / 4 snap: false.
	radians = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	target rotation: radians.
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/24/2020 11:19:54'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
	evt hand obtainHalo: self.
	scale _ (evt eventPosition - target referencePosition) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	target scale: scale.
	scaleHandle morphPositionInWorld: evt eventPosition - (scaleHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/24/2020 11:44:08'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ target rotation - (evt eventPosition - target referencePosition) theta.! !

!methodRemoval: MovableMorph #rotationDegrees: stamp: 'jmv 11/24/2020 12:15:04'!
MovableMorph removeSelector: #rotationDegrees:!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
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
('geometry' displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentChanged: externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth referencePosition rotation rotation:scale: rotationDegrees rotationDegrees: scale)
('geometry testing' clipsSubmorphs fullContainsGlobalPoint: fullContainsPoint: hasOwnLocation hasVariableExtent isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

!classDefinition: #HaloMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset scaleOffset haloBox haloDelta'
	classVariableNames: 'Icons'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
