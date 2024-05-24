'From Cuis7.1 [latest update: #6392] on 22 May 2024 at 12:15:29 pm'!
!classDefinition: 'SystemWindow class' category: #'Morphic-Widgets'!
SystemWindow class
	instanceVariableNames: 'menuNshortcutsProviders '!

!Morph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:59:03'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut."

	| menuNshortcutsSpecs selector shortcutCharacter target widgetKey |
	widgetKey := self menuNshortcutsKey ifNil: [ ^false ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
	menuNshortcutsSpecs ifEmpty: [ ^false ].
	menuNshortcutsSpecs valuesDo: [ :itemSpec |
		itemSpec size >= 5 ifTrue: [
			shortcutCharacter := itemSpec third.
			shortcutCharacter = aKeyboardEvent keyCharacter ifTrue: [
				target := itemSpec fourth
					ifNotNil: [ :targetGetter | self perform: targetGetter ]
					ifNil: [ self ].
				selector := itemSpec fifth.
				target perform: selector.
				^true ]
			]
		].
	^false! !

!Morph methodsFor: 'structure' stamp: 'jmv 5/21/2024 16:02:22'!
window
	"Return the first enclosing morph that is a kind of Window, or nil if none"
	^self owningWindow! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/16/2024 10:55:14'!
menuNshortcutsKey
	"
	Nil by default. Specific for each instance for PluggableLists. Maybe class name (and therefore specific for each class) for TextModelMorph or such (^self class name).
	"
	^nil! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/16/2024 10:50:40'!
menuNshortcutsSpec: aWidgetKey for: aWidget
	"menuSpec is the Spec for the menu and shortcut keys.
	It is an Array where each entry is
		#(itemNumber 'menu item label'    shortcut    receiver    selector     iconOrNil(optional)    'help text (optional)')
		- itemNumber is an integer or float number, and is only used for ordering of menu items
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiver is either #w (window) or #m (model)
		- selector is #aSymbol. No arguments.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	"
	"Several nested morphs may provide their own menu items and keyboard shortcuts for submorphs.
	A morph could also provide its own."

	| menuNshortcutsSpecs |

	menuNshortcutsSpecs := Dictionary new.
	self withAllOwnersReverseDo: [ :o |
		o ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: [ :menuItemSpec |
			(menuItemSpec size = 2 and: [ 		menuItemSpec second first = $X ])
				"#(12 		'XXX') means 'remove menu item numbered 12'"
				ifTrue: [ menuNshortcutsSpecs removeKey: menuItemSpec first ifAbsent: nil ]
				ifFalse: [ menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]]].
	^ menuNshortcutsSpecs! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/15/2024 15:13:03'!
ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs.
	See, for instance, SystemWindow."! !


!PluggableMorph methodsFor: 'access' stamp: 'jmv 5/16/2024 10:52:33'!
menuNshortcutsKey
	"See senders"
	^menuNshortcutsKey! !

!PluggableMorph methodsFor: 'access' stamp: 'jmv 5/15/2024 15:18:45'!
menuNshortcutsKey: aSymbol
	"See senders"
	menuNshortcutsKey := aSymbol.! !


!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 5/16/2024 10:44:18'!
ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs."

	(self perform: aWidgetKey) do: [ :eachSpec |
		aBlock value: eachSpec ].

	(self class registeredMenuAndShortcutsSpecProviders: aWidgetKey) ifNotNil: [ :providers |
		providers do: [ :eachMessageSend |
			(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]].! !


!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'jmv 5/15/2024 11:23:29'!
at: aWidgetKey registerMenuAndShortcutsSpecProvider: aMessageSend
	menuNshortcutsProviders ifNil: [ menuNshortcutsProviders := Dictionary new ].
	(menuNshortcutsProviders at: aWidgetKey ifAbsentPut: [ OrderedCollection new ])
		add: aMessageSend! !

!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'jmv 5/15/2024 12:22:29'!
registeredMenuAndShortcutsSpecProviders: aWidgetKey
	menuNshortcutsProviders ifNil: [ ^nil ].
	^menuNshortcutsProviders at: aWidgetKey ifAbsent: nil.! !


!Morph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:02:32'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event.
	Subclasses should call super, and check if the event was handled."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ aKeyboardEvent wasHandled: true ].

	aKeyboardEvent anyModifierKeyPressed ifTrue: [
		(self keyboardShortcut: aKeyboardEvent)
			ifTrue: [ ^ aKeyboardEvent wasHandled: true ]].! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/21/2024 16:04:10'!
getMenu
	"Answer the right-click (contextual) menu."

	| widgetKey menuNshortcutsSpecs itemSpec label menu menuItem shortcutCharacter target selector iconSymbol helpText |
	widgetKey := self menuNshortcutsKey ifNil: [ ^nil ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].

	menu := MenuMorph new.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size <= 2
			ifTrue: [ menu addLine ]
			ifFalse: [
				label := itemSpec second.
				shortcutCharacter := itemSpec third.
				target := itemSpec fourth
					ifNotNil: [ :targetGetter | self perform: targetGetter ]
					ifNil: [ self ].
				selector := itemSpec fifth.
				iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
				helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
				menuItem := label isSymbol
					ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
					ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
				iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
				helpText ifNotNil: [ menuItem setBalloonText: helpText ]
				]
		].
	^ menu! !


!HierarchicalListMorph methodsFor: 'menu' stamp: 'jmv 5/16/2024 11:00:00'!
getMenu
	"Answer the menu for this view"

	"Use the new mechanism if provided. See #menuNshortcutsKey"
	super getMenu ifNotNil: [ :menu | 
		^menu ].

	"Fallback to the old way. See #mainView:menuGetter:keystrokeAction:	"
	menuGetter ifNil: [^ nil].
	(menuGetter is: #MessageSend) ifTrue: [
		^menuGetter value ].
	menuGetter numArgs = 0 ifTrue: [
		^ mainView perform: menuGetter ].
	^ self error: 'The menuGetter has an unsupported number of arguments'! !


!PluggableListMorph methodsFor: 'menu' stamp: 'jmv 5/16/2024 11:00:29'!
getMenu
	"Answer the menu for this view"

	"Use the new mechanism if provided. See #menuNshortcutsKey"
	super getMenu ifNotNil: [ :menu | 
		^menu ].

	"Fallback to the old way. See #mainView:menuGetter:keystrokeAction:	"
	menuGetter ifNil: [^ nil].
	(menuGetter is: #MessageSend) ifTrue: [
		^menuGetter value ].
	menuGetter numArgs = 0 ifTrue: [
		^ mainView perform: menuGetter ].
	^ self error: 'The menuGetter has an unsupported number of arguments'! !

!methodRemoval: Morph #model stamp: 'jmv 5/22/2024 12:12:21'!
Morph removeSelector: #model!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color layerNumber location location: lock mainBoundsFinderCanvas mainCanvas morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError showBitMask showContour)
('drawing' drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: imageFormVG:depth: isKnownFailing postDrawOn: show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: keyboardShortcut: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: invalidDrop: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange: keyboardFocused)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' bitMask collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: fullyCoveredRectangle includesPixel: isCloserThan:toPixel: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloRedrawMayBeNeeded haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo: wantsRecolorHandle)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded maximumLayoutHeight minimumLayoutExtent: minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf window withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addLabel:font:centeredAt:rotationDegrees: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('submorphs-menus' getMenu menuNshortcutsKey menuNshortcutsSpec:for: ownMenuNshortcutsSpecs:for:do:)
('testing' canDiscardEdits disregardUnacceptedEdits drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isPlacedMorph isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse embeddedInMorphicWindowLabeled: flash flashWith: showAndComeToFront unlockOneSubpart whenUIinSafeState:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: horizontalScrollBar verticalScrollBar)
('access options' alwaysHideVerticalScrollbar alwaysShowVerticalScrollbar hideScrollBarsIndefinitely)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: extentChanged: focusIndicatorBottom focusIndicatorExtent focusIndicatorLeft focusIndicatorRectangle focusIndicatorRight focusIndicatorTop fontPreferenceChanged hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent scrollDeltaHeight scrollDeltaHeight: scrollDeltaWidth scrollDeltaWidth: scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableArea viewableAreaBottom viewableAreaLeft viewableAreaRight viewableAreaTop viewableAreaTopLeft viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' clipsSubmorphs hIsScrollbarShowing vIsScrollbarShowing)
('initialization' initialize innerMorphClass scroller:)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' drawsKeyboardFocusIndicator is:)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: isHighlighted makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' fullyCoveredRectangle)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit closeOtherWindows delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('moving' moveBottom moveBottomLeft moveBottomRight moveLeft moveRight moveTop moveTopLeft moveTopRight)
('resize/collapse' allowedArea expandBoxHit resize resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('submorphs-menus' ownMenuNshortcutsSpecs:for:do:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: forceSaveContents forceSaveContentsTo: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!

!classDefinition: 'SystemWindow class' category: #'Morphic-Widgets'!
SystemWindow class
	instanceVariableNames: 'menuNshortcutsProviders'!
