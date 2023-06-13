'From Cuis 6.0 [latest update: #5848] on 13 June 2023 at 12:51:06 pm'!
'Description Tool that combines an explorer and an inspector specialized for Morphs.

Open from the Morphs Halo menu: Debug -> Explore.

Pickup morph to explore from the Window menu.'!
!provides: 'MorphExplorer' 1 15!
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

!MorphExplorer methodsFor: 'accessing' stamp: 'MM 6/13/2023 12:47:01'!
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

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:48:00'!
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
		addMorph: listMorph proportionalWidth: 0.4;
		addAdjusterAndMorph: inspectorMorph layoutSpec: LayoutSpec useAll.
	
	self setLabel: model rootMorph shortPrintString.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 11:46:56'!
buildWindowMenu

	|menu|
	
	menu := super buildWindowMenu.
	
	menu addLine.
	menu add: 'pick morph ...' action: #pickMorph icon: #worldIcon.
	
	^ menu! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 11:48:41'!
deeperMorphFrom: aMorph that: aBlock

	| found |
	
	aMorph submorphs do: [ :submorph |
		found := self
			deeperMorphFrom: submorph
			that: aBlock.
		found ifNotNil: [ ^ found ]].
	(aBlock value: aMorph) ifTrue: [ ^ aMorph ].

	^ nil.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:00:59'!
delete

	model getCurrentSelection ifNotNil: [:aWrapper | aWrapper withoutListWrapper highlighted: false].
	
	super delete.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:45:18'!
initialExtent

	^`500@400` * (Preferences at: #standardCodeFont) lineSpacing // 14! !

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

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:08:16'!
pickMorph

	| morph prevMorph delay world |
	
	morph := nil.
	prevMorph := nil.
	delay := Delay forMilliseconds: 50.
	world := self runningWorld.
	
	[Sensor isAnyButtonPressed] whileFalse: [
		prevMorph := morph.
		morph := self deeperMorphFrom: model rootMorph
					that: [:m | m displayBounds ifNotNil: [:bounds | bounds containsPoint: Sensor mousePoint] ifNil: [false]].
		prevMorph == morph ifFalse: [
			Transcript show: morph printString; newLine.
			prevMorph ifNotNil: [prevMorph highlighted: false].
			morph ifNotNil: [morph highlighted: true.
						model noteNewSelection: (MorphExplorerWrapper with: morph)]].
		world doOneCycleNow.
		delay wait].
	
	prevMorph ifNotNil: [prevMorph highlighted: false].
	morph ifNotNil: [
		morph highlighted: false.
		model noteNewSelection: (MorphExplorerWrapper with: morph)]
		! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:01:46'!
replaceInspector

	self layoutMorph removeMorph: inspectorMorph.
	
	inspectorMorph := self inspectorMorphOn: model getCurrentSelection withoutListWrapper .
	
	self layoutMorph addMorph: inspectorMorph layoutSpec: LayoutSpec useAll.
	self layoutMorph adoptWidgetsColor: self windowColor.! !

!MorphExplorerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:50:36'!
update: aSymbol

	aSymbol == #getCurrentSelection ifTrue: [ | selectedMorph |
		model getPreviousSelection ifNotNil: [:wrapper | wrapper item highlighted: false].
		selectedMorph := model getCurrentSelection ifNotNil: [:selection | selection item ].
		selectedMorph ifNotNil: [
			selectedMorph highlighted: true.
			activateWindows ifTrue: [selectedMorph isKindOf: SystemWindow :: ifTrue: [selectedMorph activate]].
			self replaceInspector]].! !

!MorphExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 16:13:08'!
contents

	^ item submorphs collect: [:submorph | self class with: submorph]! !

!MorphExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/13/2023 12:23:24'!
isAncestorOf: anObject

	self item allSubmorphsDo: [:submorph |
		submorph == anObject ifTrue: [^true]].
	
	^ false! !

!Morph methodsFor: '*MorphExplorer' stamp: 'MM 6/6/2023 10:46:39'!
explore

	MorphExplorerWindow 
		open: (MorphExplorer new rootMorph: self)! !
