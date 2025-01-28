'From Cuis7.3 [latest update: #7029] on 28 January 2025 at 3:34:25 pm'!
!classDefinition: #FindInSourceDialog category: #'Morphic-Composite Widgets'!
Dialog subclass: #FindInSourceDialog
	instanceVariableNames: 'smalltalkEditor searchField initialSearchString scopeList package selectedScopeIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 1/28/2025 15:18:20'!
buildWidgets

	| buttonRow titleMorph |
	titleMorph := (LabelMorph contents: 'Find in source code' fontSize: 16 emphasis: 1).
	titleMorph layoutSpec offAxisEdgeWeight: 0.
	searchField := EntryFieldMorph new.
	searchField
		crAction: [ self ok ];
		escAction: [ self cancel ].
	searchField layoutSpec offAxisEdgeWeight: 0.
	initialSearchString ifNotNil: [ searchField contents: initialSearchString ].
	scopeList := PluggableListMorph
		withModel: self
		listGetter: #scopesList
		indexGetter: #scopeIndex
		indexSetter: #scopeIndex:.
	scopeList 
		doubleClickSelector: #ok.
	buttonRow := LayoutMorph newRow.
	buttonRow
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		gap: 14;
		addMorphKeepMorphHeight: titleMorph;
		addMorphKeepMorphHeight: searchField;
		addMorphUseAll: scopeList;
		addMorphKeepMorphHeight: buttonRow.
	layoutMorph
		position: self interiorOrigin
		extent: self interiorExtent! !

!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 1/28/2025 14:54:38'!
initialize
	super initialize.
	selectedScopeIndex := 1.! !

!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 1/28/2025 14:59:13'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	"Steal focus even after some subwidget got it."
	self whenUIinSafeState: [aWorld activeHand newKeyboardFocus: self]! !

!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 1/28/2025 14:54:56'!
smalltalkEditor: aSmalltalkEditor
	smalltalkEditor := aSmalltalkEditor.
	initialSearchString := smalltalkEditor selectedString asPlainString.! !

!FindInSourceDialog methodsFor: 'defaults' stamp: 'jmv 1/28/2025 15:05:25'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !

!FindInSourceDialog methodsFor: 'defaults' stamp: 'jmv 1/28/2025 15:02:02'!
defaultExtent
	^`640@320`! !

!FindInSourceDialog methodsFor: 'defaults' stamp: 'jmv 1/28/2025 15:13:51'!
defaultPadding
	^ `30@20`! !

!FindInSourceDialog methodsFor: 'events' stamp: 'jmv 1/28/2025 15:00:19'!
cancel
	self delete.! !

!FindInSourceDialog methodsFor: 'events' stamp: 'jmv 1/28/2025 15:00:23'!
keyStroke: aKeyboardEvent
	"Avoid the need to give focus to subwidgets"

	(aKeyboardEvent isArrowUp or: [  aKeyboardEvent isArrowDown ]) ifTrue: [
		^scopeList keyStroke: aKeyboardEvent ].
	aKeyboardEvent isReturnKey ifTrue: [
		^self ok ].
	aKeyboardEvent isEsc ifTrue: [
		^self cancel ].
	^searchField keyStroke: aKeyboardEvent! !

!FindInSourceDialog methodsFor: 'events' stamp: 'jmv 1/28/2025 15:00:27'!
ok
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
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !

!FindInSourceDialog methodsFor: 'event handling testing' stamp: 'jmv 1/28/2025 15:01:00'!
handlesKeyboard

	^self visible! !

!FindInSourceDialog methodsFor: 'scopes list' stamp: 'jmv 1/28/2025 14:59:59'!
scopeIndex
	^selectedScopeIndex! !

!FindInSourceDialog methodsFor: 'scopes list' stamp: 'jmv 1/28/2025 15:00:02'!
scopeIndex: i
	selectedScopeIndex := i.! !

!FindInSourceDialog methodsFor: 'scopes list' stamp: 'jmv 1/28/2025 15:00:05'!
scopesList
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
				ifNotNil: [ :scopeMethod |CodePackage packageOfMethod: scopeMethod ifNone: []]
				ifNil: [CodePackage packageOfClass: scopeClass ifNone: []].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
	^labelList! !


!FindInSourceDialog class methodsFor: 'instance creation' stamp: 'jmv 1/28/2025 14:58:30'!
openOn: aSmalltalkEditor
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self new beColumn
		smalltalkEditor: aSmalltalkEditor;
		buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog.! !


!SmalltalkEditor methodsFor: 'accessing' stamp: 'jmv 1/28/2025 15:19:43'!
selectedMethod
	self codeProvider isEditingExistingMethod ifTrue: [
		self codeProvider currentCompiledMethod asMethodReference ].! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 1/28/2025 15:20:00'!
findMethodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList package |

	true ifTrue: [
		^FindInSourceDialog openOn: self ].

	searchString := StringRequestMorph
		request: 'Enter search text:'
		initialAnswer: self selectedString
		orCancel: [^self].
	
	scopeClass := self selectedClass.
	scopeClassName := scopeClass name asString.
	includeSubclasses := false.
	includeSuperclasses := false.

	labelList := {
		'This method'. 
		'All methods in image'. } asOrderedCollection.
	(scopeClass notNil)
		ifTrue: [
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := self codeProvider isEditingExistingMethod
				ifTrue: [CodePackage packageOfMethod: self codeProvider currentCompiledMethod asMethodReference ifNone: []]
				ifFalse: [CodePackage packageOfClass: scopeClass ifNone: []].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
				
	selection := (SelectionMenu labelList: labelList selections: nil) startUpWithCaption: 'Search Scope'.

	(selection = 0) ifTrue:[^self].
	(selection = 1) "This method"
		ifTrue:[^self find: searchString].
	(selection = 2) "All methods in image"
		ifTrue:[scopeClass := nil].
	"(selection = 3) ifTrue:[]. ""Class only - nothing to do"
	(selection = 4) "Class and subclasses"
		ifTrue:[includeSubclasses := true].
	(selection = 5) "Class, subclasses, and superclasses"
		ifTrue:[
			includeSubclasses := true.
			includeSuperclasses := true].
	(selection = 6) "In my package if any "
		ifTrue: [^ Smalltalk browseMethodsWithSourceString: searchString in: package].
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !

