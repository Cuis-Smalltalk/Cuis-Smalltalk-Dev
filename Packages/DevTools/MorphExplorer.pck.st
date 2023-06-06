'From Cuis 6.0 [latest update: #5832] on 6 June 2023 at 10:47:27 am'!
'Description '!
!provides: 'MorphExplorer' 1 10!
SystemOrganization addCategory: 'MorphExplorer'!


!classDefinition: #MorphExplorer category: 'MorphExplorer'!
AbstractHierarchicalList subclass: #MorphExplorer
	instanceVariableNames: 'rootMorph previousSelection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphExplorer'!
!classDefinition: 'MorphExplorer class' category: 'MorphExplorer'!
MorphExplorer class
	instanceVariableNames: ''!

!classDefinition: #MorphExplorerWindow category: 'MorphExplorer'!
SystemWindow subclass: #MorphExplorerWindow
	instanceVariableNames: 'listMorph inspectorMorph activateWindows'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphExplorer'!
!classDefinition: 'MorphExplorerWindow class' category: 'MorphExplorer'!
MorphExplorerWindow class
	instanceVariableNames: ''!

!classDefinition: #MorphExplorerWrapper category: 'MorphExplorer'!
ListItemWrapper subclass: #MorphExplorerWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphExplorer'!
!classDefinition: 'MorphExplorerWrapper class' category: 'MorphExplorer'!
MorphExplorerWrapper class
	instanceVariableNames: ''!


!MorphExplorer commentStamp: 'MM 6/5/2023 16:43:21' prior: 0!
MorphExplorerWindow 
		open: (MorphExplorer new rootMorph: self runningWorld)!

!MorphExplorerWindow commentStamp: 'MM 6/5/2023 16:43:30' prior: 0!
MorphExplorerWindow 
		open: (MorphExplorer new rootMorph: self runningWorld)!

!MorphExplorer methodsFor: 'accessing' stamp: 'MM 6/5/2023 15:48:31'!
getList

	^Array with: (MorphExplorerWrapper with: rootMorph)! !

!MorphExplorer methodsFor: 'accessing' stamp: 'MM 6/5/2023 15:46:03'!
rootMorph
	"Answer the value of rootMorph"

	^ rootMorph! !

!MorphExplorer methodsFor: 'accessing' stamp: 'MM 6/5/2023 15:46:03'!
rootMorph: anObject
	"Set the value of rootMorph"

	rootMorph := anObject! !

!MorphExplorer methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:33:04'!
getPreviousSelection

	^ previousSelection! !

!MorphExplorer methodsFor: 'as yet unclassified' stamp: 'MM 6/6/2023 10:22:01'!
inspectSelectedMorph

	currentSelection item ifNotNil: [:aMorph | aMorph inspect]! !

!MorphExplorer methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:32:45'!
noteNewSelection: anItemWrapper

	previousSelection := currentSelection.
	
	^ super noteNewSelection: anItemWrapper! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:30:22'!
buildMorphicWindow

	listMorph := HierarchicalListMorph
				model: model
				listGetter: #getList
				indexGetter: #getCurrentSelection
				indexSetter: #noteNewSelection:
				mainView: self
				menuGetter: #morphMenu
				keystrokeAction: #exploreKey:from:.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelectedMorph.
	
	inspectorMorph := self inspectorMorphOn: model rootMorph.
	
	self layoutMorph
		addMorph: listMorph proportionalWidth: 0.5;
		addAdjusterAndMorph: inspectorMorph proportionalWidth: 0.5.
	
	self setLabel: model rootMorph shortPrintString.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:42:41'!
delete

	model getCurrentSelection ifNotNil: [:aWrapper | aWrapper item highlighted: false].
	
	super delete.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/6/2023 10:45:16'!
initialize

	super initialize.
	self beRow.
	activateWindows := false.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 15:37:41'!
inspectorMorphOn: aMorph

	|window|
	window := InspectorWindow new.
	window
		model: (Inspector inspect: aMorph);
		buildMorphicWindow.
	^window layoutMorph! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/6/2023 10:31:53'!
morphMenu

	|menu|
	
	menu := MenuMorph new.
	model getCurrentSelection item addStandardHaloMenuItemsTo: menu hand: nil.
		
	^ menu! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:27:31'!
replaceInspector

	self layoutMorph removeMorph: inspectorMorph.
	
	inspectorMorph := self inspectorMorphOn: model getCurrentSelection item.
	
	self layoutMorph addMorph: inspectorMorph layoutSpec: LayoutSpec useAll.
	self layoutMorph adoptWidgetsColor: self windowColor.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/6/2023 10:45:02'!
update: aSymbol

	aSymbol == #getCurrentSelection ifTrue: [ | selectedMorph |
		model getPreviousSelection ifNotNil: [:wrapper | wrapper item highlighted: false].
		selectedMorph := model getCurrentSelection ifNotNil: [:selection | selection item].
		selectedMorph ifNotNil: [
			selectedMorph highlighted: true.
			activateWindows ifTrue: [selectedMorph isKindOf: SystemWindow :: ifTrue: [selectedMorph activate]].
			self replaceInspector]].! !

!MorphExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:13:08'!
contents

	^ item submorphs collect: [:submorph | self class with: submorph]! !

!Morph methodsFor: '*MorphExplorer' stamp: 'MM 6/6/2023 10:46:39'!
explore

	MorphExplorerWindow 
		open: (MorphExplorer new rootMorph: self)! !
