'From Cuis 5.0 of 7 November 2016 [latest update: #3293] on 21 April 2018 at 4:54:48 pm'!
!classDefinition: #DebuggerWindow category: #'Morphic-Tools'!
CodeWindow subclass: #DebuggerWindow
	instanceVariableNames: 'receiverInspector codePane receiverInspectorText stackList contextVariableInspector contextVariableInspectorText '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!SystemWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:46:39'!
canDiscardEditsOf: aMorphWithChanges 

	| okToLooseChanges |
	
	aMorphWithChanges canDiscardEdits ifTrue: [ ^true ].
	
	okToLooseChanges _ self isItOkToLooseChanges.
	okToLooseChanges ifTrue: [ aMorphWithChanges disregardUnacceptedEdits ].
	
	^okToLooseChanges

		! !

!SystemWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:03:28'!
isItOkToLooseChanges
	
	^ self confirm:
'Changes have not been saved.
Is it OK to cancel those changes?'.! !

!SystemWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 12:09:13'!
okToChangeDueTo: aMorph
	
	^self okToChange! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 4/21/2018 12:11:38'!
buildMorphicCodePane
	
	codePane _ super buildMorphicCodePane.
	^codePane! !

!DebuggerWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:47:36'!
okToChangeCodePane

	| okToLooseChanges |
	
	okToLooseChanges _ self canDiscardEditsOf: codePane.
	okToLooseChanges ifTrue: [
		receiverInspectorText disregardUnacceptedEdits.
		contextVariableInspectorText disregardUnacceptedEdits ].
	
	^okToLooseChanges ! !

!DebuggerWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:47:50'!
okToChangeContextVariableInspectorText
	
	^self canDiscardEditsOf: contextVariableInspectorText! !

!DebuggerWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:40:50'!
okToChangeDueTo: aMorph

	aMorph = stackList ifTrue: [ ^self okToChangeCodePane ].
	aMorph = receiverInspector ifTrue: [ ^self okToChangeReceiverInspectorText ].
	aMorph = contextVariableInspector ifTrue: [ ^self okToChangeContextVariableInspectorText ].
	
	^super okToChangeDueTo: aMorph 
	
	
	! !

!DebuggerWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:47:56'!
okToChangeReceiverInspectorText
	
	^self canDiscardEditsOf: receiverInspectorText! !


!PluggableListMorph methodsFor: 'events' stamp: 'HAW 4/21/2018 12:09:58'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].  "work-around the no-mans-land bug"
	"No change if model is locked"
	(autoDeselect and: [ row == self visualSelectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self setSelectionIndex: 0 ]]
		ifFalse: [ self setSelectionIndex: row ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'HAW 4/21/2018 12:09:32'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar numericValue.
	oldSelection _ self getCurrentSelectionIndex.
	nextSelection _ oldSelection.
	max _ self maximumSelection.
	min _ self minimumSelection.
	howMany _ self numSelectionsInView.	"get this exactly??"

	keyEvent = 31 ifTrue: [
		"down-arrow; move down one, wrapping to top if needed"
		nextSelection _ oldSelection + 1.
		nextSelection > max ifTrue: [ nextSelection _ 1 ]].

	keyEvent = 30 ifTrue: [
		"up arrow; move up one, wrapping to bottom if needed"
		nextSelection _ oldSelection - 1.
		nextSelection < 1 ifTrue: [ nextSelection _ max ]].

	keyEvent = 1 ifTrue: [ nextSelection _ 1 ].			"home"
	keyEvent = 4 ifTrue: [ nextSelection _ max ].		"end"
	keyEvent = 11 ifTrue: [ nextSelection _ min max: oldSelection - howMany ].		"page up"
	keyEvent = 12 ifTrue: [ nextSelection _ oldSelection + howMany min: max ].		"page down"
	nextSelection = oldSelection ifFalse: [
		w _ self owningWindow.
		(w isNil or: [ w okToChangeDueTo: self ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [
				self setSelectionIndex: nextSelection ].
			answer _ true ]].
	^answer! !

!PluggableListMorph methodsFor: 'model access' stamp: 'HAW 4/21/2018 12:09:49'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [^ self]].
	nextSelection _ list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self setSelectionIndex: nextSelection! !


!SystemWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 16:46:56'!
okToChange

	^self canDiscardEditsOf: self! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 4/21/2018 12:08:29'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |

	stackList _ PluggableListMorph
		model: model 
		listGetter: #contextStackList
		indexGetter: #contextStackIndex
		indexSetter: #toggleContextStackIndex:
		mainView: self
		menuGetter: #contextStackMenu
		keystrokeAction: #contextStackKey:from:.

	receiverInspector _ PluggableListMorph
			model: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #receiverFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	receiverInspector doubleClickSelector: #inspectSelection.
	receiverInspectorText _ TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	contextVariableInspector _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contextVariableInspector doubleClickSelector: #inspectSelection.
	contextVariableInspectorText _ TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2! !

!methodRemoval: DebuggerWindow #canDiscardEditsFrom:!
DebuggerWindow removeSelector: #canDiscardEditsFrom:!
!methodRemoval: DebuggerWindow #canDiscardEditsFrom:disregarding:!
DebuggerWindow removeSelector: #canDiscardEditsFrom:disregarding:!
!methodRemoval: DebuggerWindow #okToChangeStackList!
DebuggerWindow removeSelector: #okToChangeStackList!
!methodRemoval: SystemWindow #askShouldSaveChanges!
SystemWindow removeSelector: #askShouldSaveChanges!
!classDefinition: #DebuggerWindow category: #'Morphic-Tools'!
CodeWindow subclass: #DebuggerWindow
	instanceVariableNames: 'stackList receiverInspector receiverInspectorText contextVariableInspector contextVariableInspectorText codePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicCodePane buildMorphicWindow customButtonRow customButtonSpecs initialExtent optionalButtonTuples windowColor)
('menu building' contextFieldListMenu contextStackMenu receiverFieldListMenu)
('menu commands' contextClassHierarchy exploreContextSelection exploreReceiverSelection receiverClassHierarchy returnValue)
('keyboard shortcuts' browseClassRefsIn: browseFullProtocolIn: browseHierarchyIn: browseMethodFullIn: contextStackKey:from: exploreSelectionIn: inspectSelectionIn: inspectorKey:from:)
('open/close' closeView delete model:)
('user interface' okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
!

