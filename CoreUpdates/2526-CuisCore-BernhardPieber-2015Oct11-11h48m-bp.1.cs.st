'From Cuis 4.2 of 25 July 2013 [latest update: #2525] on 11 October 2015 at 6:05:13.297779 pm'!

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:22'!
resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self morphBoundsInWorld].
	self morphPosition: boundingBox origin extent: boundingBox extent! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:45'!
resizeBottom
	self resize: (Display boundingBox top: Display height // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:40'!
resizeBottomLeft
	self resize: (Display boundingBox leftCenter corner: Display boundingBox bottomCenter)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:38'!
resizeBottomRight
	self resize: (Display boundingBox center corner: Display boundingBox corner)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:20'!
resizeFull
	self resize: Display boundingBox! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:44'!
resizeLeft
	self resize: (Display boundingBox right: Display width // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:44'!
resizeRight
	self resize: (Display boundingBox left: Display width // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:45'!
resizeTop
	self resize: (Display boundingBox bottom: Display height // 2)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:36'!
resizeTopLeft
	self resize: (Display boundingBox origin corner: Display boundingBox center)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/11/2015 17:39'!
resizeTopRight
	self resize: (Display boundingBox topCenter corner: Display boundingBox rightCenter)! !


!SystemWindow methodsFor: 'menu' stamp: 'bp 10/11/2015 17:32'!
buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' action: #relabel;
		add: 'window color...' action: #setWindowColor;
		addLine;
		add: 'send to back' action: #sendToBack;
		add: 'make next-to-topmost' action: #makeSecondTopmost;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ]) action: #toggleStickiness;
		addLine;
		add: 'close' action: #delete;
		add: 'collapse' action: #collapse;
		add: 'expand / contract' action: #expandBoxHit;
		addLine;
		add: 'resize full' action: #resizeFull;
		add: 'resize top' action: #resizeTop;
		add: 'resize left' action: #resizeLeft;
		add: 'resize bottom' action: #resizeBottom;
		add: 'resize right' action: #resizeRight;
		add: 'resize top left' action: #resizeTopLeft;
		add: 'resize top right' action: #resizeTopRight;
		add: 'resize bottom left' action: #resizeBottomLeft;
		add: 'resize bottom right' action: #resizeBottomRight.

	^ aMenu! !


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' addPossiblyUncoveredAreasIn:to: drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minPaneHeightForReframe minPaneWidthForReframe minimumExtent rescaleButtons)
('initialization' boxExtent createCloseBox createCollapseBox createExpandBox createMenuBox defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: buildWindowMenu changeColor fullScreen makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent openInWorld:)
('panes' setUpdatablePanesFrom: updatablePanes widgetsColor widgetsColor:)
('printing' printOn:)
('resize/collapse' expandBoxHit resize: resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing' submorphsDrawingOutsideReverseDo:)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is: isOpaqueMorph)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' okToChange)
!

