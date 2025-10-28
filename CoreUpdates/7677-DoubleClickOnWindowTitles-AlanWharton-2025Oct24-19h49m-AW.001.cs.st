'From Cuis7.5 [latest update: #7641] on 24 October 2025 at 7:51:20 pm'!

!SystemWindow methodsFor: 'events' stamp: 'AW 10/24/2025 19:50:18'!
doubleClick: aMouseEvent localPosition: currentPosition
	self fullScreen.! !

!SystemWindow methodsFor: 'events' stamp: 'AW 10/24/2025 19:49:35'!
handlesMouseDown: aMouseEvent
	^true! !

!SystemWindow methodsFor: 'events' stamp: 'AW 10/24/2025 19:49:58'!
mouseButton1Down: aMouseEvent localPosition: localEventPosition
	aMouseEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2:  self
		event:  aMouseEvent 
		clkSel:  nil
		clkNHalf:  nil
		dblClkSel:  #doubleClick:localPosition:
		dblClkNHalfSel:  nil
		tripleClkSel:  nil
		dragSel: #dragEvent:localPosition:
	! !


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: isHighlighted makeMeFullyVisible makeMeVisible visible:)
('events' doubleClick:localPosition: handlesMouseDown: mouseButton1Down:localPosition: wantsToBeDroppedInto:)
('geometry' extentChanged: fontPreferenceChanged interiorExtent interiorOrigin labelXPosition minimumExtent rescaleButtons)
('geometry services' fullyCoveredRectangle)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeAdjusters initializeLabelArea intoWorld: openInWorld windowEdgeAdjustingMorphClass)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beForm beRow layoutMorph useFormLayout useLayoutMorph: useLinearLayout validateLayoutMorphCanBeChanged)
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
('submorphs-menus' allRegisteredMenuAndShortcutsSpecProviders:do:widget: menuAndShortcutRegistryRoot: menuNshortcutsSpecsDo: menuNshortcutsSpecsFor:do: windowMenuSpec)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow)
('user interface' canDiscardEditsOf: forceSaveContents forceSaveContentsTo: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!

