'From Cuis6.3 [latest update: #6256] on 8 March 2024 at 3:44:43 pm'!
!classDefinition: #MethodSetWindow category: #'Morphic-Tool Windows'!
CodeWindow subclass: #MethodSetWindow
	instanceVariableNames: 'codePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!MethodSetWindow commentStamp: '<historical>' prior: 0!
Morphic view for MessageSet models. See category 'GUI building'.!


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/8/2024 15:31:36'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons := LayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/8/2024 15:37:02'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^PluggableListMorph
		model: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:
		mainView: self
		menuGetter: #messageListMenu
		keystrokeAction: #messageListKey:from:! !

!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/8/2024 15:31:36'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := self buildMorphicMessageList.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
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
	model changed: #editSelection! !

!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/8/2024 15:32:26'!
createCodePaneMorph
	
	codePane := super createCodePaneMorph.
	^codePane! !

!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/8/2024 15:31:36'!
windowColor
	^ Theme current messageSet! !

!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 3/8/2024 15:31:36'!
addExtraMenu2ItemsTo: options
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		options add: `{
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		31.		
			#label 			-> 		'remove from this browser'.
			#object 			-> 		#model.
			#selector 			-> 		#removeMessageFromBrowser.
			#icon 			-> 		#listRemoveIcon 
		} asDictionary`.
		options add:`{
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		32.		
			#label 			-> 		'filter message list...'.
			#selector 			-> 		#filterMessageList.
			#icon 			-> 		#findIcon
		} asDictionary`	].

	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		33.		
			#label 			-> 		'sort by class hierarchy'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByClassHierarchy.
			#icon 			-> 		#classIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		34.		
			#label 			-> 		'sort by class name'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByClassName.
			#icon 			-> 		#classIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		35.		
			#label 			-> 		'sort by selector'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortBySelector.
			#icon 			-> 		#scriptIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		36.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		37.		
			#label 			-> 		'reverse sort'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortReverse.
			#icon 			-> 		#redoIcon
		} asDictionary`.

! !

!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 3/8/2024 15:36:56'!
messageListMenu
	"Answer the message-list menu"

	^DynamicMenuBuilder buildTitled: 'Message Set' targeting: self collectingMenuOptionsWith: #messageSetMenuOptions.
	! !

!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 3/8/2024 15:32:42'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	self flag: #assumesKeyboardShortcuts.
	^DynamicMenuBuilder 
		buildTitled: 'Message Set' 
		targeting: self 
		collectingMenuOptionsWith: #messageSetMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 			-> 		40.
					#itemOrder 			-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 			-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
		
	! !

!MethodSetWindow methodsFor: 'menu commands' stamp: 'jmv 3/8/2024 15:33:26'!
browseCategoryOfCurrentMethod

	model categoryOfCurrentMethod ifNotNil: [:category| |catIndex selectedMessage|
		selectedMessage := model selectedMessageName.
		catIndex := model classOrMetaClassOrganizer categories indexOf: category.
		model messageCategoryListIndex: catIndex + 1.
		model selectedMessageName: selectedMessage]! !

!MethodSetWindow methodsFor: 'menu commands' stamp: 'jmv 3/8/2024 15:31:36'!
filterMessageList
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
	aMenu popUpInWorld: self world! !

!MethodSetWindow methodsFor: 'updating' stamp: 'jmv 3/8/2024 15:34:12'!
disableCodePaneEditing

	codePane ifNotNil: [ codePane disableEditing ]! !

!MethodSetWindow methodsFor: 'updating' stamp: 'jmv 3/8/2024 15:34:15'!
editSelectionChanged
	
	model isEditingNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]! !

!MethodSetWindow methodsFor: 'updating' stamp: 'jmv 3/8/2024 15:34:18'!
enableCodePaneEditing

	codePane ifNotNil: [ codePane enableEditing ]! !

!MethodSetWindow methodsFor: 'updating' stamp: 'jmv 3/8/2024 15:34:20'!
update: anEvent
	super update: anEvent.
	anEvent = #editSelection ifTrue: [self editSelectionChanged ] ! !


!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
findInSourceCode
	self request: 'Text to search source code for?' do: [:aString|
		Smalltalk browseMethodsWithSourceString: aString]! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
open: model label: aString

	| window |
	
	window := super open: model label: aString.
	model messageListIndex: 1.
	
	^window! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openFullProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openFullProtocolForClass: ProtocolBrowser."

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new on: aClass.
	label := 'Entire protocol of: ', aClass name.

	^self open: aPBrowser label: label! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openMessageList: methodReferences label: aString
	"Create a standard system view for the message set on the list, anArray.
	The label of the view is aString."

	| methodSet |
	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	^self open: methodSet label: aString.! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openMessageList: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MethodSet on messageList."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString ! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openMessageListUnsorted: methodReferences label: labelString
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.

	^self open: methodSet label: labelString ! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openMessageListUnsorted: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString ! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:31:36'!
openSubProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openSubProtocolForClass: ProtocolBrowser."

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new onSubProtocolOf: aClass.
	label := 'Sub-protocol of: ', aClass name.

	^self open: aPBrowser label: label! !

!MethodSetWindow class methodsFor: 'browser menues' stamp: 'jmv 3/8/2024 15:34:41'!
messageSetMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 		-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 		-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 		-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'spawn sub-protocol'.
				#selector 		-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'spawn full protocol'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 		-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 		-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 		-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!MethodSetWindow class methodsFor: 'browser menues' stamp: 'jmv 3/8/2024 15:34:44'!
messageSetMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'what to show...'.
				#selector 			-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 			-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{	#itemGroup  			-> 		20.
				#itemOrder  			-> 		50.
				#label            			-> 		'browse method category (c)'.
				#selector       		-> 			#browseCategoryOfCurrentMethod.
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect CompiledMethod'.
				#object 			-> 		#model.
				#selector 			-> 		#inspectCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 			-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 			-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 			-> 		#methodInheritance.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 			-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		40.
				#label 			-> 		'class variables'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 			-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

