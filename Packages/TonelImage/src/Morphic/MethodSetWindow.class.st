"
Morphic view for MessageSet models. See category 'GUI building'.
"
Class {
	#name : 'MethodSetWindow',
	#superclass : 'CodeWindow',
	#instVars : [
		'codePane'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'opening' }
MethodSetWindow class >> open: model label: aString [

	| window |
	
	window := super open: model label: aString.
	model messageListIndex: 1.
	
	^window
]

{ #category : 'opening' }
MethodSetWindow class >> openMessageList: methodReferences label: aString [
	"Create a standard system view for the message set on the list, anArray.
	The label of the view is aString."

	| methodSet |
	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	^self open: methodSet label: aString.
]

{ #category : 'opening' }
MethodSetWindow class >> openMessageList: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean [
	"Open a system view for a MethodSet on messageList."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString 
]

{ #category : 'opening' }
MethodSetWindow class >> openMessageListUnsorted: methodReferences label: labelString [
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.

	^self open: methodSet label: labelString 
]

{ #category : 'opening' }
MethodSetWindow class >> openMessageListUnsorted: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean [
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString 
]

{ #category : 'menu commands' }
MethodSetWindow >> browsePackage [

	| package browser |
	"Try to get package of the selected method"
	model currentCompiledMethod ifNotNil: [ :method |
		package := CodePackage packageOfMethod: model currentCompiledMethod asMethodReference ifNone: nil.
		browser := SinglePackageBrowser new.
		browser package: package.
		BrowserWindow open: browser label: browser labelString ].
]

{ #category : 'GUI building' }
MethodSetWindow >> buildLowerPanes [
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
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
MethodSetWindow >> buildMorphicMessageList [
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.
]

{ #category : 'GUI building' }
MethodSetWindow >> buildMorphicWindow [
	"Answer a morphic window with the given label that can display the receiver"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := self buildMorphicMessageList.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
	model changed: #editSelection
]

{ #category : 'GUI building' }
MethodSetWindow >> createCodePaneMorph [
	
	codePane := super createCodePaneMorph.
	^codePane
]

{ #category : 'updating' }
MethodSetWindow >> disableCodePaneEditing [

	codePane ifNotNil: [ codePane disableEditing ]
]

{ #category : 'updating' }
MethodSetWindow >> editSelectionChanged [
	
	model isEditingNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]
]

{ #category : 'updating' }
MethodSetWindow >> enableCodePaneEditing [

	codePane ifNotNil: [ codePane enableEditing ]
]

{ #category : 'menu commands' }
MethodSetWindow >> filterMessageList [
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [ ^self inform: 'this is not a propitious filtering situation' ].

	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		add: 'unsent messages' action: #filterToUnsentMessages balloonText: 'filter to show only messages that have no senders';
		addLine;
		add: 'messages that send...' action: #filterToSendersOf balloonText: 'filter to show only messages that send a selector I specify';
		add: 'messages that do not send...' action: #filterToNotSendersOf balloonText: 'filter to show only messages that do not send a selector I specify';
		addLine;
		add: 'messages whose selector is...' action: #filterToImplementorsOf balloonText: 'filter to show only messages with a given selector I specify';
		add: 'messages whose selector is NOT...' action: #filterToNotImplementorsOf balloonText: 'filter to show only messages whose selector is NOT a seletor I specify';
		addLine;
		add: 'messages in any change set' action: #filterToAnyChangeSet balloonText: 'filter to show only messages that occur in at least one change set';
		add: 'messages not in any change set' action: #filterToNotAnyChangeSet balloonText: 'filter to show only messages that do not occur in any change set in the system';
		addLine;
		add: 'messages authored by me' action: #filterToCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp has my initials';
		add: 'messages not authored by me' action: #filterToNotCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp does not have my initials';
		addLine;
		add: 'messages logged in .changes file' action: #filterToMessagesInChangesFile balloonText: 'filter to show only messages whose latest source code is logged in the .changes file';
		add: 'messages only in .sources file' action: #filterToMessagesInSourcesFile balloonText: 'filter to show only messages whose latest source code is logged in the .sources file';
		addLine;
		add: 'messages with prior versions' action: #filterToMessagesWithPriorVersions balloonText: 'filter to show only messages that have at least one prior version';
		add: 'messages without prior versions' action: #filterToMessagesWithoutPriorVersions balloonText: 'filter to show only messages that have no prior versions';
		addLine;
		add: 'uncommented messages' action: #filterToUncommentedMethods balloonText: 'filter to show only messages that do not have comments at the beginning';
		add: 'commented messages' action: #filterToCommentedMethods balloonText: 'filter to show only messages that have comments at the beginning'.
	aMenu popUpInWorld: self world
]

{ #category : 'menu & kbd shortcuts' }
MethodSetWindow >> messageListMenuSpec [
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 				false)
		#(10	'Browse Full (b)' 				$b 	window browseMethodFull 						editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 		$h 	window browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 			$O 	window openSingleMessageBrowser 				scriptIcon)
		#(40	'Browse Protocol (p)' 			$p 	window browseFullProtocol 						spreadsheetIcon)
		#(45	'Browse Package' 			nil 	window browsePackage 							packageIcon)
		#(50 	'-----')
		#(60	'File Out' 					nil 	model 	fileOutMessage 							fileOutIcon)
		#(61	'File Out All' 					nil 	model 	fileOutAllMethods 							fileOutIcon)
		#(70	'Run Test (t)' 				$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(103	'Refactorings' 				nil 	nil 		1000 										listAddIcon)
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'Refactoring' SubMenu"
		#(1010	'Rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'Change keyword Order...' 				nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'Add Parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'Remove Parameter... (S)' 				$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'Inline Method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'Move to Instance/Class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsibility' nil model 		addAsSubclassResponsibilitySelector 		goTopIcon)
		#(1100	'Push Down to Subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push Down to One Subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

		" 'more' SubMenu"
		#(2010	'Sample instance' 			nil 	window makeSampleInstance 						instanceIcon)
		#(2020	'Inspect instances' 			nil 	window inspectInstances 							inspectIcon)
		#(2030	'Inspect subinstances' 		nil 	window inspectSubInstances 						inspectIcon)
		#(2040 	'-----')
		#(2050 	'Remove from this Browser' 	nil 	model 	removeMessageFromBrowser 				listRemoveIcon)
		#(2060 	'Filter message list...' 			nil 	window filterMessageList 							findIcon)
		#(2070	'Change category...' 			nil 	model 	changeCategory 							saveAsIcon)
		#(2080	'Change sets with this Method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'Revert to previous Version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'Sort by class Hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'Sort by class Name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'Sort by Selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'Sort by Date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'Reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).

]

{ #category : 'updating' }
MethodSetWindow >> update: anEvent [
	super update: anEvent.
	anEvent = #editSelection ifTrue: [self editSelectionChanged ] 
]

{ #category : 'GUI building' }
MethodSetWindow >> windowColor [
	^ Theme current messageSet
]
