'From Cuis 5.0 of 7 November 2016 [latest update: #3207] on 10 November 2017 at 11:19:54 am'!
!classDefinition: #WorkspaceWindow category: #'Morphic-Tools'!
SystemWindow subclass: #WorkspaceWindow
	instanceVariableNames: 'acceptDroppedMorphs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 11/10/2017 11:14:34'!
objectForWorkspace
	^self! !


!InspectorWindow methodsFor: 'accessing' stamp: 'jmv 11/10/2017 11:15:23'!
objectForWorkspace
	^model object! !


!ObjectExplorerWindow methodsFor: 'accessing' stamp: 'jmv 11/10/2017 11:19:10'!
objectForWorkspace
	^model rootObject! !


!Morph methodsFor: 'accessing - properties' stamp: 'jmv 11/10/2017 11:16:44'!
nameForWorkspace
	"Answer a name suitable for a Workspace variable"
	| displayName object |
	object _ self objectForWorkspace.
	displayName := object name.
	^ displayName 
		ifNotNil: [ | name |
			name := displayName asIdentifier: false.
			(name size < 1)
			ifTrue: [ object class name asLowercase , object identityHash asString ]
			ifFalse: [ name at: 1 put:  (name at: 1) asLowercase. name ]
		] 
		ifNil: [ object class name asLowercase , object identityHash asString ]! !


!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 11/10/2017 11:10:18'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aCustomMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aCustomMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling! !

!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 11/10/2017 11:10:38'!
offerWindowMenu
	| aMenu |
	aMenu _ self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu popUpInWorld: self world! !

!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/10/2017 11:09:41'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 11/10/2017 11:17:29'!
wantsDroppedMorph: aMorph event: evt
	"Dropping a morph on my window means:
  	    display exteral name for aMorph in Workspace
	    add a binding from name to aMorph
	    and rejecting the drop."
	
	| objectName textModelMorph object |
	objectName := aMorph nameForWorkspace.
	object _ aMorph objectForWorkspace.
	textModelMorph := self layoutMorph submorphs at: 1.
	
	(self model  bindingOf: objectName) value: object.
	textModelMorph editor afterSelectionInsertAndSelect: objectName , ' '.
	"send aMorph back to original position"
	evt isNil ifFalse: [ evt wasHandled: true ]. 
	"Short circuit"
	aMorph rejectDropMorphEvent: evt.
	
	^ false ! !

!methodRemoval: WorkspaceWindow #allowsMorphDropWording!
WorkspaceWindow removeSelector: #allowsMorphDropWording!
!methodRemoval: WorkspaceWindow #initialize!
WorkspaceWindow removeSelector: #initialize!
!methodRemoval: WorkspaceWindow #toggleAcceptDroppedMorphs!
WorkspaceWindow removeSelector: #toggleAcceptDroppedMorphs!

!InspectorWindow reorganize!
('GUI building' buildMorphicWindow initialExtent)
('menu building' fieldListMenu)
('menu commands' addEntry browseClassRefs browseFullProtocol browseHierarchy browseMethodFull exploreSelection inspectBasic inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('keyboard shortcuts' inspectorKey:from:)
('accessing' objectForWorkspace)
!


!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('building menus' genericMenu)
('keyboard shortcuts' explorerKey:from:)
('menu commands' browseClassRefs browseFullProtocol browseHierarchy browseMethodFull exploreSelection inspectBasic inspectSelection monitor: openWeightExplorer stopMonitoring)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
('stepping' stepAt:)
('accessing' objectForWorkspace)
!

!classDefinition: #WorkspaceWindow category: #'Morphic-Tools'!
SystemWindow subclass: #WorkspaceWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!
