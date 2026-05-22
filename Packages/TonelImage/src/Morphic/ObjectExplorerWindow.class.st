"
Morphic view for ObjectExplorer models. See category 'GUI building'.
"
Class {
	#name : 'ObjectExplorerWindow',
	#superclass : 'SystemWindow',
	#instVars : [
		'listMorph'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu commands' }
ObjectExplorerWindow >> addHaloToSelection [
	model getCurrentSelection item addHalo
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> browseClassRefs [
	Smalltalk browseAllCallsOnClass: model object class theNonMetaClass
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> browseFullProtocol [
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClass
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> browseHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	Smalltalk
		browseHierarchy: model selectedClass
		selector: nil.
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass := model selectedClass) ifNotNil: [
		Smalltalk browseFull: myClass selector: nil ].
]

{ #category : 'GUI building' }
ObjectExplorerWindow >> buildMorphicWindow [

	| textMorph |
	listMorph := (HierarchicalListMorph
			withModel: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:)
				menuNshortcutsKey: #listMenuSpec.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	"Expand the first level of the tree."
	listMorph selectionIndex: 1; navigateRight.
	textMorph := ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: model rootObject shortPrintString.

]

{ #category : 'menu commands' }
ObjectExplorerWindow >> copySelectionToClipboard [
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model object
]

{ #category : 'GUI building' }
ObjectExplorerWindow >> expandAll [

	| firstSelection list |
	list := model getList.
	firstSelection := list isEmpty ifFalse: [ list first ].
	model noteNewSelection: firstSelection.
	listMorph expandAll
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> exploreSelection [
	"Create and schedule an Explorer on the receiver's model's currently selected object."

	^ model object explore
]

{ #category : 'GUI building' }
ObjectExplorerWindow >> initialExtent [

	^`300@500` * (Preferences at: #standardCodeFont) lineSpacing // 14
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> inspectBasic [
	"Create and schedule a basic Inspector on the receiver's model's currently selected object."

	^ model object basicInspect
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> inspectSelection [
	"Create and schedule an Inspector on the receiver's model's currently selected object."

	^ model object inspect
]

{ #category : 'menu & kbd shortcuts' }
ObjectExplorerWindow >> listMenuSpec [
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| answer |
	model getCurrentSelection
		ifNil: [^#()]
		ifNotNil: [ :currSel |
			answer := #(
				#(10		'Inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
				#(20		'Explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
				#(30		'Copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
				#(40		'Basic inspect' 			nil 	window 	inspectBasic 				inspectIcon)
				#(50		'References finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
				#(60		'Weight explorer' 		nil 	window 	openWeightExplorer 		exploreIcon)
				#(70 		'-----')
				#(80		'Browse Full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
				#(90		'Browse Hierarch (h)' 		$h 	window 	browseHierarchy 			goTopIcon)
				#(100		'Browse Protocol (p)' 		$p 	window 	browseFullProtocol 		spreadsheetIcon)).
			(currSel item is: #Morph) ifTrue: [ answer := answer, #(
				#(110 		'-----')
				#(120		'Show morph Halo' 		nil 	window 	addHaloToSelection 		editFindReplaceIcon)) ].
			model class == ReferencesExplorer ifTrue: [ answer := answer, #(
				#(210 		'-----')
				#(220		'Rescan' 				nil 	window 	rescan 						editFindReplaceIcon)) ].
			answer := answer, #(
				#(310 		'-----')
				#(320		'Monitor changes' 		nil 	window 	monitorChanges 			editFindReplaceIcon)).
			model isMonitoring ifTrue: [ answer := answer, #(
				#(410 		'-----')
				#(420		'stop monitoring all' 		nil 	window 	stopMonitoring 			editFindReplaceIcon)) ].
			^answer ].
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> monitorChanges [
	"Start stepping and watching the given wrapper for changes."

	model getCurrentSelection ifNotNil: [ :wrapper |
		model monitorList at: wrapper put: wrapper asString.
		self startSteppingStepTime: 200 ]
]

{ #category : 'menu commands - reference finder' }
ObjectExplorerWindow >> objectsToSkipDuringRescan [
	"Avoid observing side effects of the reference finder itself"

	| answer |
	answer := OrderedCollection
		with: self
		with: model
		with: model getCurrentSelection.
	self allMorphsDo: [:each | answer add: each].
	^answer.
]

{ #category : 'menu commands - reference finder' }
ObjectExplorerWindow >> openReferencesFinder [
	"Create and schedule a References Finder on the receiver's model's currently selected object."
	
	ReferenceFinder openExplorerOn: model object
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> openWeightExplorer [
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model object
]

{ #category : 'menu commands - reference finder' }
ObjectExplorerWindow >> rescan [

	| objectsToSkip |
	objectsToSkip := self objectsToSkipDuringRescan.
	model noteNewSelection: nil.
	model rescanSkipping: objectsToSkip.
	self expandAll.
]

{ #category : 'stepping' }
ObjectExplorerWindow >> stepAt: millisecondSinceLast [

	super stepAt: millisecondSinceLast.	
	model monitorList isEmpty ifTrue: [
		self stopStepping ]
]

{ #category : 'menu commands' }
ObjectExplorerWindow >> stopMonitoring [

	model stopMonitoring.
	self stopStepping
]
