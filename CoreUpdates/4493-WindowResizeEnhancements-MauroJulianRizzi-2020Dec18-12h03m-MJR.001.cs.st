'From Cuis 5.0 [latest update: #4384] on 18 December 2020 at 12:16:51 pm'!

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:43'!
allowedArea
	^(RealEstateAgent maximumUsableAreaInWorld: self world) insetBy: Theme current fullScreenDeskMargin! !


!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeBottom
	self resize: (self allowedArea top: self allowedArea height // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeBottomLeft
	self resize: (self allowedArea leftCenter corner: self allowedArea bottomCenter)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeBottomRight
	self resize: (self allowedArea center corner: self allowedArea corner)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeFull
	self resize: self allowedArea! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeLeft
	self resize: (self allowedArea right: self allowedArea width // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeRight
	self resize: (self allowedArea left: self allowedArea width // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeTop
	self resize: (self allowedArea bottom: self allowedArea height // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeTopLeft
	self resize: (self allowedArea origin corner: self allowedArea center)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MJR 12/18/2020 12:14:00'!
resizeTopRight
	self resize: (self allowedArea topCenter corner: self allowedArea rightCenter)! !


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' addPossiblyUncoveredAreasIn:to: drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld titleBarButtonsExtent)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor fullScreen makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor:)
('printing' printOn:)
('resize/collapse' allowedArea expandBoxHit resize resize: resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing')
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!

