'From Haver 6.0 [latest update: #5488] on 20 September 2022 at 1:41:58 pm'!

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:30:28'!
moveBottom
	"Move the window to the bottom edge of its owner/world/display."

	self morphPosition: self morphPosition x @ (self allowedArea bottom - self morphHeight)! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:36:04'!
moveBottomLeft
	"Move the window to the bottom left corner of its owner/world/display."

	self morphPosition: self allowedArea left @ (self allowedArea bottom - self morphHeight)! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:34:04'!
moveBottomRight
	"Move the window to the bottom right corner of its owner/world/display."

	self morphPosition: self allowedArea bottomRight - self morphExtent! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:26:53'!
moveLeft
	"Move the window to the left edge of its owner/world/display."

	self morphPosition: self allowedArea left @ self morphPosition y! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:29:23'!
moveRight
	"Move the window to the right edge of its owner/world/display."

	self morphPosition: (self allowedArea right - self morphWidth) @ self morphPosition y! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:26:40'!
moveTop
	"Move the window to the top edge of its owner/world/display."

	self morphPosition: self morphPosition x @ self allowedArea top! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:33:03'!
moveTopLeft
	"Move the window to the top left corner of its owner/world/display."

	self morphPosition: self allowedArea topLeft! !

!SystemWindow methodsFor: 'moving' stamp: 'KLG 9/20/2022 13:37:42'!
moveTopRight
	"Move the window to the tog right corner of its owner/world/display."

	self morphPosition: (self allowedArea right - self morphWidth) @ self allowedArea top! !


!WindowEdgeAdjustingMorph methodsFor: 'events' stamp: 'KLG 9/20/2022 13:21:32'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Some nice window expansions."
	
	self owner ifNotNil: [ :o |
		o perform: ((selector copyUpToLast: $:) 
			copyReplaceAll: 'window' 
			with: (aMouseButtonEvent shiftPressed ifFalse: [ 'resize' ] ifTrue: [ 'move' ])) asSymbol ]! !


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawLabelOn: drawOn: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' on:ifCovered:uncoveredPartsDo:else:)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('moving' moveBottom moveBottomLeft moveBottomRight moveLeft moveRight moveTop moveTopLeft moveTopRight)
('resize/collapse' allowedArea expandBoxHit resize resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing')
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!

