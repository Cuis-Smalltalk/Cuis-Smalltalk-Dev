'From Cuis 6.0 [latest update: #5300] on 16 June 2022 at 4:31:56 pm'!

!Browser methodsFor: 'system category functions' stamp: 'len 6/12/2022 13:44:27'!
categorizeUnderCategory: systemCategoryName class: className
	systemCategoryName isNil ifTrue: [^ self].
	(Smalltalk at: className asSymbol ifAbsent: [^ self]) category: systemCategoryName! !


!HierarchicalListMorph methodsFor: 'accessing' stamp: 'len 6/16/2022 16:30:58'!
rejectDrops

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'len 6/12/2022 13:31:46'!
dragEvent: aMouseEvent localPosition: localEventPosition
	| draggedItem draggingGuide label |
	draggedItem _ ((self itemFromPoint: localEventPosition) ifNil: [ ^self ]) withoutListWrapper.
	label _ LabelMorph contents: draggedItem asString.
	draggingGuide _ DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: draggedItem.	
	aMouseEvent hand attachMorphBeside: draggingGuide! !

!HierarchicalListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 13:31:07'!
acceptDroppingMorph: aMorph event: dropEvent
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:event: message. This default implementation just adds the given morph to the receiver."
	| localPosition targetItem dropActionSelector args |
	localPosition _ self internalizeFromWorld: dropEvent eventPosition.
	targetItem _ ((self itemFromPoint: localPosition) ifNil: [ ^self ]) withoutListWrapper.
	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: targetItem].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument _ aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		^model perform: dropActionSelector with: targetItem with: dropSelectorArgument ].
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'! !

!HierarchicalListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 12:40:22'!
acceptDropsFrom: aMorph performing: dropActionSelector
	(dropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: dropActionSelector! !

!HierarchicalListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 13:26:38'!
allowItemDrag: aBoolean
	self setProperty: #allowsItemDrag toValue: aBoolean! !

!HierarchicalListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 12:33:40'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^(aMorph is: #DraggingGuideMorph)
		and: [ (aMorph valueOfProperty: #dragSource) = (self valueOfProperty: #acceptedDragSource) ]! !


!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 12:57:39'!
acceptDropsFrom: aMorph performing: dropActionSelector
	(dropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: dropActionSelector! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 13:25:07'!
allowItemDrag: aBoolean
	self setProperty: #allowsItemDrag toValue: aBoolean! !


!Browser methodsFor: 'message category functions' stamp: 'len 6/12/2022 12:56:06'!
categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aSelectorToCategorize
	aMessageCategoryListIndex isNil ifTrue: [^ self categorizeUnderNewCategorySelector: aSelectorToCategorize].
	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector _ self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]! !


!Morph methodsFor: 'event handling testing' stamp: 'len 6/12/2022 12:05:17'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ (self valueOfProperty: #allowsMorphDrop) = true! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'len 6/12/2022 13:23:33'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ])! !


!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'len 6/16/2022 16:28:43'!
rejectDrops

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 12:54:46'!
acceptDroppingMorph: aMorph event: dropEvent
	| localPosition row dropSelectorArgument dropActionSelector args |
	localPosition _ self internalizeFromWorld: dropEvent eventPosition.
	row _ self rowAtLocation: localPosition ifNone: [].
	dropSelectorArgument _ aMorph
		valueOfProperty: #dropSelectorArgument
		ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: dropSelectorArgument ].
	args = 2 ifTrue: [ ^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
	
		
! !

!PluggableListMorph methodsFor: 'events' stamp: 'len 6/12/2022 12:23:38'!
dragEvent: aMouseEvent localPosition: localEventPosition
	| row listItem label draggingGuide |
	scroller highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.
	
	label _ LabelMorph contents: listItem.
	draggingGuide _ DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: draggingGuide! !

!PluggableListMorph methodsFor: 'events' stamp: 'len 6/12/2022 13:24:13'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		scroller noSelection].
	row _ self 
		rowAtLocation: localEventPosition
		ifNone:  [^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view.
	Model update will be done on mouse button up, so this feedback will be visible before that."
	scroller highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 6/12/2022 13:22:11'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCategoryList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	classList allowItemDrag: true.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategory:class:.
		
	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'len 6/12/2022 13:22:16'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classList _ self buildMorphicClassList.
	classColumn _ self buildMorphicClassColumnWith: classList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (PreferenceNG at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!methodRemoval: PluggableListMorph #acceptDroppingMorph:outsideListWithEvent: stamp: 'len 6/16/2022 16:24:31'!
PluggableListMorph removeSelector: #acceptDroppingMorph:outsideListWithEvent:!
!methodRemoval: PluggableListMorph #allowsMorphDrop stamp: 'len 6/16/2022 16:26:25'!
PluggableListMorph removeSelector: #allowsMorphDrop!
!methodRemoval: PluggableListMorph #itemsAreDraggable stamp: 'len 6/16/2022 16:26:25'!
PluggableListMorph removeSelector: #itemsAreDraggable!
!methodRemoval: PluggableListMorph #acceptDroppingMorph:atRow:withEvent: stamp: 'len 6/16/2022 16:24:31'!
PluggableListMorph removeSelector: #acceptDroppingMorph:atRow:withEvent:!
!methodRemoval: PluggableListMorph #acceptDropsFrom:performing:whenOutsideList: stamp: 'len 6/16/2022 16:24:31'!
PluggableListMorph removeSelector: #acceptDropsFrom:performing:whenOutsideList:!
!methodRemoval: PluggableListMorph #makeItemsUndraggable stamp: 'len 6/16/2022 16:27:55'!
PluggableListMorph removeSelector: #makeItemsUndraggable!
!methodRemoval: PluggableListMorph #makeItemsDraggable stamp: 'len 6/16/2022 16:27:56'!
PluggableListMorph removeSelector: #makeItemsDraggable!

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
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
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

