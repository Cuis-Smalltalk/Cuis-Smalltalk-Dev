Class {
	#name : 'FindInSourceDialog',
	#superclass : 'Dialog',
	#instVars : [
		'smalltalkEditor',
		'searchField',
		'initialSearchString',
		'scopeList',
		'package',
		'selectedScopeIndex'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'instance creation' }
FindInSourceDialog class >> openOn: aSmalltalkEditor [
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self new beColumn
		smalltalkEditor: aSmalltalkEditor;
		buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog.
]

{ #category : 'initialization' }
FindInSourceDialog >> buildWidgets [

	| buttonRow titleMorph |
	titleMorph := (LabelMorph contents: 'Find in source code' fontSize: 16 emphasis: 1).
	titleMorph useDefaultLayoutSizeSpec offAxisEdgeWeight: 0.
	searchField := EntryFieldMorph new.
	searchField
		crAction: [ self ok ];
		escAction: [ self cancel ].
	searchField useDefaultLayoutSizeSpec offAxisEdgeWeight: 0.
	initialSearchString ifNotNil: [ searchField contents: initialSearchString ].
	scopeList := PluggableListMorph
		withModel: self
		listGetter: #scopesList
		indexGetter: #scopeIndex
		indexSetter: #scopeIndex:.
	scopeList 
		doubleClickSelector: #ok.
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		gap: 14;
		addMorphKeepMorphHeight: titleMorph;
		addMorphKeepMorphHeight: searchField;
		addMorphUseAll: scopeList;
		addMorphKeepMorphHeight: buttonRow.

	self morphExtent: self minimumExtent * (2@1.5).
]

{ #category : 'events' }
FindInSourceDialog >> cancel [
	self delete.
]

{ #category : 'defaults' }
FindInSourceDialog >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'defaults' }
FindInSourceDialog >> defaultPadding [
	^ `30@20`
]

{ #category : 'initialization' }
FindInSourceDialog >> initialize [
	super initialize.
	selectedScopeIndex := 1.
]

{ #category : 'initialization' }
FindInSourceDialog >> intoWorld: aWorld [
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	"Steal focus even after some subwidget got it."
	self whenUIinSafeState: [aWorld activeHand newKeyboardFocus: self]
]

{ #category : 'events' }
FindInSourceDialog >> keyStroke: aKeyboardEvent [
	"Avoid the need to give focus to subwidgets"

	(aKeyboardEvent isArrowUp or: [  aKeyboardEvent isArrowDown ]) ifTrue: [
		scopeList keyStroke: aKeyboardEvent.
		aKeyboardEvent wasHandled: true.
		^self ].
	aKeyboardEvent isReturnKey ifTrue: [
		self ok.
		aKeyboardEvent wasHandled: true.
		^self ].
	aKeyboardEvent isEsc ifTrue: [
		self cancel.
		aKeyboardEvent wasHandled: true.
		^self ].
	searchField keyStroke: aKeyboardEvent.
]

{ #category : 'events' }
FindInSourceDialog >> ok [
	| includeSubclasses includeSuperclasses searchString scopeClass |
	self delete.
	
	scopeClass := smalltalkEditor selectedClass.
	includeSubclasses := false.
	includeSuperclasses := false.

	searchString := searchField contents.

	(selectedScopeIndex = 0) ifTrue:[^self].
	(selectedScopeIndex = 1) "This method"
		ifTrue:[^smalltalkEditor find: searchString].
	(selectedScopeIndex = 2) "All methods in image"
		ifTrue:[scopeClass := nil].
	"(selection = 3) ifTrue:[]. ""Class only - nothing to do"
	(selectedScopeIndex = 4) "Class and subclasses"
		ifTrue:[includeSubclasses := true].
	(selectedScopeIndex = 5) "Class, subclasses, and superclasses"
		ifTrue:[
			includeSubclasses := true.
			includeSuperclasses := true].
	(selectedScopeIndex = 6) "In my package if any "
		ifTrue: [^ Smalltalk browseMethodsWithSourceString: searchString in: package].
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses
]

{ #category : 'scopes list' }
FindInSourceDialog >> scopeIndex [
	^selectedScopeIndex
]

{ #category : 'scopes list' }
FindInSourceDialog >> scopeIndex: i [
	selectedScopeIndex := i.
]

{ #category : 'scopes list' }
FindInSourceDialog >> scopesList [
	| scopeClassName labelList |
	labelList := {
		'This method or text'.
		'All methods in image'. } asOrderedCollection.
	smalltalkEditor selectedClass ifNotNil:
		[ :scopeClass |
			scopeClassName := scopeClass name asString.
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := smalltalkEditor selectedMethod
				ifNotNil: [ :scopeMethod |CodePackage packageOfMethod: scopeMethod ifNone: nil]
				ifNil: [CodePackage packageOfClass: scopeClass ifNone: nil].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
	^labelList
]

{ #category : 'initialization' }
FindInSourceDialog >> smalltalkEditor: aSmalltalkEditor [
	smalltalkEditor := aSmalltalkEditor.
	initialSearchString := smalltalkEditor selectedString asPlainString.
]
