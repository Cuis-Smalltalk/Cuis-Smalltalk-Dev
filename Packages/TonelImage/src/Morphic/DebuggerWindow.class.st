"
Morphic view for Debugger models. See category 'GUI building'.
"
Class {
	#name : 'DebuggerWindow',
	#superclass : 'CodeWindow',
	#instVars : [
		'stackList',
		'receiverInspector',
		'receiverInspectorText',
		'contextVariableInspector',
		'contextVariableInspectorText',
		'codePane'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'services' }
DebuggerWindow class >> closeAllDebuggers [
	"DebuggerWindow closeAllDebuggers"

	self closeAllDebuggersSuchThat: [:ign| true]
]

{ #category : 'services' }
DebuggerWindow class >> closeAllDebuggersSuchThat: aBlock [
	"DebuggerWindow closeAllDebuggers"

	UISupervisor ui ifNotNil:
		[ :world |
		world submorphsDo:
			[ :morph |
			((morph class == DebuggerWindow or: [morph class == PreDebugWindow ])
			 and: [aBlock value: morph])
				ifTrue: [morph delete ]]]
]

{ #category : 'services' }
DebuggerWindow class >> debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock [
	"For all open debugger windows whose model (the Debugger itself) satisfies qualifyingBlock,
	evaluate anotherBlock with the window as argument.
	GUI support."

	UISupervisor ui ifNotNil: [ :world |
		world submorphsDo: [ :morph |
			morph class == DebuggerWindow ifTrue: [
				(qualifyingBlock value: morph model) ifTrue: [
					anotherBlock value: morph ]]]].
]

{ #category : 'GUI building' }
DebuggerWindow >> buildLowerPanes [
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons := LinearLayoutMorph newColumn.
	h := self defaultButtonPaneHeight.
	(Preferences at: #optionalButtons) ifTrue: [
		h := self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
]

{ #category : 'GUI building' }
DebuggerWindow >> buildMorphicWindow [
	"Open a full morphic debugger with the given label"

	| bottomMorph |
	bottomMorph := LinearLayoutMorph newRow.
	bottomMorph
		addMorph: self receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: self contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: self stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2.
]

{ #category : 'open/close' }
DebuggerWindow >> closeView [
	self model: nil.
	self delete
]

{ #category : 'menu commands' }
DebuggerWindow >> contextClassBrowse [

	Smalltalk browseFull: model contextVariablesInspector selectedClassOrMetaClass.
]

{ #category : 'menu commands' }
DebuggerWindow >> contextClassHierarchy [

	Smalltalk
		browseHierarchy: model contextVariablesInspector selectedClassOrMetaClass
		selector: nil.
]

{ #category : 'menu commands' }
DebuggerWindow >> contextClassProtocol [

	ProtocolBrowserWindow
		openFullProtocolForClass: model contextVariablesInspector selectedClassOrMetaClass
]

{ #category : 'menu & kbd shortcuts' }
DebuggerWindow >> contextStackMenuSpec [
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Browse (b)' 				$b 	window 	browseMethodFull 		nil 	'open a Browser on method')
		#(20	'Full Stack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(30	'Proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(40	'Restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(50	'Step Into Send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(60	'Step Over Send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(70	'Step into Block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(80	'Run to Cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(90	'Where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(100	'Peel to First like this' 	nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(110 	'-----')
		#(120	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(130	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		"Keyboard shortcut only. No menu entry."
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)
]

{ #category : 'GUI building' }
DebuggerWindow >> contextVariableInspector [

	^ contextVariableInspector ifNil: [
		contextVariableInspector := (PluggableListMorph
			withModel: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				doubleClickSelector: #inspectContextSelection;
				menuNshortcutsKey: #contextVariablesListMenuSpec ].		
]

{ #category : 'GUI building' }
DebuggerWindow >> contextVariableInspectorText [

	^ contextVariableInspectorText ifNil: [
		contextVariableInspectorText := (TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Context scope' ]
]

{ #category : 'menu & kbd shortcuts' }
DebuggerWindow >> contextVariablesListMenuSpec [
	"Spec for the menu and shortcut keys for the list of variables in this context.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Inspect (i)' 			$i 	window 	inspectContextSelection 		inspectIcon)
		#(20	'Explore (I)' 			$I 	window 	exploreContextSelection 		exploreIcon)
		#(30 	'-----')
		#(40	'Browse Full (b)' 		$b 	window 	contextClassBrowse 			editFindReplaceIcon)
		#(50	'Browse Hierarch (h)' 	$h 	window 	contextClassHierarchy 			goTopIcon)
		#(60	'Browse Protocol (p)' 	$p 	window 	contextClassProtocol 			spreadsheetIcon)
	)
]

{ #category : 'GUI building' }
DebuggerWindow >> createCodePaneMorph [
	
	codePane := super createCodePaneMorph.
	^codePane
]

{ #category : 'context stack menu' }
DebuggerWindow >> createMethod [

	^ self ifOkToChangeCodePaneDo: [ model createMethod ]
]

{ #category : 'GUI building' }
DebuggerWindow >> customButtonRow [
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons := OrderedCollection new.
	buttonColor := self buttonColor.
	"button with target = self"
	button := PluggableButtonMorph 
		model: self
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button := PluggableButtonMorph 
					model: self
					action: tuple second.
		button color: buttonColor.
		button label: tuple first.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row
]

{ #category : 'GUI building' }
DebuggerWindow >> customButtonSpecs [
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send				'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Run to Cursor'	runToCursor		'run to cursor')
		('Full Stack'		fullStack			'show full stack')
		('Where'			where				'select current pc range')
		('Create'			createMethod		'create method'))
]

{ #category : 'open/close' }
DebuggerWindow >> delete [
	model ifNotNil: [ model windowIsClosing ].
	super delete
]

{ #category : 'context stack menu' }
DebuggerWindow >> doStep [

	^ self ifOkToChangeCodePaneDo: [ model doStep ]
]

{ #category : 'menu commands' }
DebuggerWindow >> exploreContextSelection [

	model contextVariablesInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model contextVariablesInspector selection explore
]

{ #category : 'menu commands' }
DebuggerWindow >> exploreReceiverSelection [
	model receiverInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model receiverInspector  selection explore
]

{ #category : 'context stack menu' }
DebuggerWindow >> fullStack [

	^ self ifOkToChangeCodePaneDo: [ model fullStack ]
]

{ #category : 'user interface' }
DebuggerWindow >> ifOkToChangeCodePaneDo: aBlock [

	^self okToChangeCodePane ifTrue: aBlock 

]

{ #category : 'GUI building' }
DebuggerWindow >> initialExtent [
	^ RealEstateAgent standardWindowExtent * 3 // 2
]

{ #category : 'menu commands' }
DebuggerWindow >> inspectContextSelection [

	model contextVariablesInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model contextVariablesInspector selection inspect
]

{ #category : 'menu commands' }
DebuggerWindow >> inspectReceiverSelection [
	model receiverInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model receiverInspector  selection inspect
]

{ #category : 'initialization' }
DebuggerWindow >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model when: #closeViews send: #closeView to: self.
]

{ #category : 'user interface' }
DebuggerWindow >> okToChangeCodePane [

	| okToLooseChanges |
	
	okToLooseChanges := self canDiscardEditsOf: codePane.
	okToLooseChanges ifTrue: [
		receiverInspectorText disregardUnacceptedEdits.
		contextVariableInspectorText disregardUnacceptedEdits ].
	
	^okToLooseChanges 
]

{ #category : 'user interface' }
DebuggerWindow >> okToChangeContextVariableInspectorText [
	
	^self canDiscardEditsOf: contextVariableInspectorText
]

{ #category : 'user interface' }
DebuggerWindow >> okToChangeDueTo: aMorph [

	aMorph = stackList ifTrue: [ ^self okToChangeCodePane ].
	aMorph = receiverInspector ifTrue: [ ^self okToChangeReceiverInspectorText ].
	aMorph = contextVariableInspector ifTrue: [ ^self okToChangeContextVariableInspectorText ].
	
	^super okToChangeDueTo: aMorph 
	
	
	
]

{ #category : 'user interface' }
DebuggerWindow >> okToChangeReceiverInspectorText [
	
	^self canDiscardEditsOf: receiverInspectorText
]

{ #category : 'context stack menu' }
DebuggerWindow >> peelToFirst  [
	
	^ self ifOkToChangeCodePaneDo: [ model peelToFirst ]
]

{ #category : 'context stack menu' }
DebuggerWindow >> proceed [

	^ self ifOkToChangeCodePaneDo: [ model proceed ]
]

{ #category : 'menu commands' }
DebuggerWindow >> receiverClassBrowse [

	Smalltalk browseFull: model receiverInspector selectedClassOrMetaClass.
]

{ #category : 'menu commands' }
DebuggerWindow >> receiverClassHierarchy [

	Smalltalk
		browseHierarchy: model receiverInspector selectedClassOrMetaClass
		selector: nil.
]

{ #category : 'menu commands' }
DebuggerWindow >> receiverClassProtocol [

	ProtocolBrowserWindow
		openFullProtocolForClass: model receiverInspector selectedClassOrMetaClass
]

{ #category : 'GUI building' }
DebuggerWindow >> receiverInspector [

	^ receiverInspector ifNil: [
		receiverInspector := (PluggableListMorph
			withModel: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				doubleClickSelector: #inspectReceiverSelection;
				menuNshortcutsKey: #receiverVariablesListMenuSpec ].
]

{ #category : 'GUI building' }
DebuggerWindow >> receiverInspectorText [

	^ receiverInspectorText ifNil: [
		receiverInspectorText := (TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Receiver scope' ]
]

{ #category : 'menu & kbd shortcuts' }
DebuggerWindow >> receiverVariablesListMenuSpec [
	"Spec for the menu and shortcut keys for the list of instance variables in the receiver.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Inspect (i)' 			$i 	window 	inspectReceiverSelection 		inspectIcon)
		#(20	'Explore (I)' 			$I 	window 	exploreReceiverSelection 		exploreIcon)
		#(30 	'-----')
		#(40	'Browse Full (b)' 		$b 	window 	receiverClassBrowse 			editFindReplaceIcon)
		#(50	'Browse Hierarch (h)' 	$h 	window 	receiverClassHierarchy 		goTopIcon)
		#(60	'Browse Protocol (p)' 	$p 	window 	receiverClassProtocol 			spreadsheetIcon)
	)
]

{ #category : 'context stack menu' }
DebuggerWindow >> restart  [
	
	^ self ifOkToChangeCodePaneDo: [ model restart ]
]

{ #category : 'menu commands' }
DebuggerWindow >> returnValue [
	self request: 'Enter expression for return value:' do: [:aString| model returnValue: aString]
]

{ #category : 'context stack menu' }
DebuggerWindow >> runToCursor [

	^ self ifOkToChangeCodePaneDo: [ 
		[model runToCursorAt: codePane startIndex ]
			on: [ :anError | anError messageText = model invalidPositionErrorDescription ]
			do: [ :anError | self inform:  'Invalid cursor position. Put the cursor in a message send to run until it' ]]
]

{ #category : 'context stack menu' }
DebuggerWindow >> send [

	^ self ifOkToChangeCodePaneDo: [ model send ]
]

{ #category : 'menu commands' }
DebuggerWindow >> shiftClickPressedOn: aCodePane at: aMousePosition [
	
	aCodePane editor quickPrintOfNodeUnder: aMousePosition
	
]

{ #category : 'GUI building' }
DebuggerWindow >> stackList [

	^ stackList ifNil: [
		stackList := (PluggableListMorph
			withModel: model 
			listGetter: #contextStackList
			indexGetter: #contextStackIndex
			indexSetter: #toggleContextStackIndex:)
				menuNshortcutsKey: #contextStackMenuSpec ].

]

{ #category : 'context stack menu' }
DebuggerWindow >> stepIntoBlock [

	^ self ifOkToChangeCodePaneDo: [ model stepIntoBlock ]
]

{ #category : 'context stack menu' }
DebuggerWindow >> where [

	^ self ifOkToChangeCodePaneDo: [ model where ]
]

{ #category : 'GUI building' }
DebuggerWindow >> windowColor [
	^ Theme current debugger
]
