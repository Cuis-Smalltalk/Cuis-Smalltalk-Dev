'From Cuis 5.0 [latest update: #4453] on 15 November 2020 at 2:07:57 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 14:04:05'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	self layoutNeeded: true.! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:32:36'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	self buildMagnifiedBackgroundImage.! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:33:14'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	self setCanvas! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 14:05:15'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	wrapFlag ifTrue: [
		extent x = oldExtent x ifFalse: [
			self resetTextComposition.
			self editor recomputeSelection.	
			owner
				updateScrollBarsBounds;
				setScrollDeltas ]]! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:34:23'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	magnifiedIcon _ nil! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:34:59'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	"Now reset widget sizes"
	scroller adjustExtent.
	self updateScrollBarsBounds.
	self setScrollDeltas.
	self scrollSelectionIntoView ! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:36:06'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."
	| isH wasH |

	super extentChanged: oldExtent.
	wasH _ self isHorizontal.
	"Doesn't move!!"
	"upButton morphPosition: borderWidth@borderWidth."
	downButton morphPosition: extent - borderWidth - downButton morphExtent.
	isH _ self isHorizontal.
	isH = wasH ifFalse: [
		isH
			ifTrue: [
				upButton updateLeftButtonImage.
				downButton updateRightButtonImage ]
			ifFalse: [
				upButton updateUpButtonImage.
				downButton updateDownButtonImage ]].! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 10:59:42'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"

	| oldExtent |
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:00:00'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| someChange oldExtent |
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:15:33'!
ensureMinimimExtent

	| oldExtent |
	oldExtent _ extent.
	(self privateExtent: extent) ifTrue: [
		self extentChanged: oldExtent ].! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:00:16'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"

	| oldExtent |
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:00:31'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| someChange oldExtent |
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 11/15/2020 10:58:37'!
image: anImage 
	| newExtent oldExtent |
	image _ anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ]. 
	newExtent _ image extent.
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		extent _ newExtent.
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]].
	self redrawNeeded.! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 11/15/2020 11:30:31'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent |
	wrapFlag _ true.
	model _ aTextModel.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self model: aTextModel! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:16:56'!
privateExtent: aPoint
	| newExtent |

	"Resist changing the extent if no wordwrap. this should be checked."
	wrapFlag ifFalse: [ ^ false ].
	
	"Just update width. Height is set by ourselves. See #fit"
	newExtent _ aPoint x truncated @ extent y.

	^ super privateExtent: newExtent! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 11/15/2020 10:59:06'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				oldExtent _ extent.
				extent _ newExtent.
				self extentChanged: oldExtent.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !

!methodRemoval: ScrollBar #privateExtent: stamp: 'jmv 11/15/2020 14:07:33'!
ScrollBar removeSelector: #privateExtent:!
!methodRemoval: PluggableScrollPane #privateExtent: stamp: 'jmv 11/15/2020 14:07:33'!
PluggableScrollPane removeSelector: #privateExtent:!
!methodRemoval: PluggableButtonMorph #privateExtent: stamp: 'jmv 11/15/2020 14:07:33'!
PluggableButtonMorph removeSelector: #privateExtent:!
!methodRemoval: WorldMorph #privateExtent: stamp: 'jmv 11/15/2020 14:07:33'!
WorldMorph removeSelector: #privateExtent:!
!methodRemoval: PasteUpMorph #privateExtent: stamp: 'jmv 11/15/2020 14:07:33'!
PasteUpMorph removeSelector: #privateExtent:!

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
('geometry' displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentChanged: externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth referencePosition rotation:scale: scale)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
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


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR chooseEmphasisOrAlignment chooseFont confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeyStroke:)
('events-processing' processKeystroke:localPosition: processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
!

