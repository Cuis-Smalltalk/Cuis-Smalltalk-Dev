'From Cuis 5.0 of 7 November 2016 [latest update: #3220] on 24 November 2017 at 12:18:25 pm'!

!SystemWindow methodsFor: 'accessing' stamp: 'jmv 11/24/2017 12:06:15'!
objectsForWorkspace
	^{}! !


!InspectorWindow methodsFor: 'accessing' stamp: 'jmv 11/24/2017 12:16:02'!
objectsForWorkspace
	| root sel |
	root _ model object.
	sel _ model selection.
	(root == sel or: [ model contentsIsString ]) ifTrue: [
		^{root} ].
	^{root. sel }! !


!ObjectExplorerWindow methodsFor: 'accessing' stamp: 'jmv 11/24/2017 12:17:16'!
objectsForWorkspace
	| root sel |
	root _ model rootObject.
	sel _ model object.
	(root == sel or: [ sel isNil ]) ifTrue: [
		^{root} ].
	^{root. sel }! !


!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/24/2017 12:08:35'!
wantsDroppedMorph: aMorph event: evt
	"Dropping a morph on my window means:
  	    display exteral name for aMorph in Workspace
	    add a binding from name to aMorph
	    and rejecting the drop."
	
	| textModelMorph addedBindings |
	textModelMorph := self layoutMorph submorphs at: 1.
	addedBindings _ false.
	aMorph objectsForWorkspace do: [ :object | | objectName |
		addedBindings _ true.
		objectName _ self nameForObject: object.
		(self model bindingOf: objectName) value: object.
		textModelMorph editor afterSelectionInsertAndSelect: String newLineString, objectName , '. ' ].
	addedBindings ifTrue: [
		"send aMorph back to original position"
		evt isNil ifFalse: [ evt wasHandled: true ]. 
		"Short circuit"
		aMorph rejectDropMorphEvent: evt ].
	
	^ false ! !


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
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor:)
('printing' printOn:)
('resize/collapse' expandBoxHit resize resize: resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing' submorphsDrawingOutsideReverseDo:)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is: isOpaqueMorph)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' okToChange)
('accessing' objectsForWorkspace)
!

