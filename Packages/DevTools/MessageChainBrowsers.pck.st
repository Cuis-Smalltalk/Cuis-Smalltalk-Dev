'From Cuis 5.0 [latest update: #4040] on 18 February 2020 at 10:47:41 am'!
'Description Browsers of chain of messages.

Author: Mariano Montone <marianomontone@gmail.com>'!
!provides: 'MessageChainBrowsers' 1 21!
SystemOrganization addCategory: #MessageChainBrowsers!


!classDefinition: #MessageChainBrowser category: #MessageChainBrowsers!
CodeProvider subclass: #MessageChainBrowser
	instanceVariableNames: 'method selectedMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageChainBrowser class' category: #MessageChainBrowsers!
MessageChainBrowser class
	instanceVariableNames: ''!

!classDefinition: #ImplementorsChainBrowser category: #MessageChainBrowsers!
MessageChainBrowser subclass: #ImplementorsChainBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'ImplementorsChainBrowser class' category: #MessageChainBrowsers!
ImplementorsChainBrowser class
	instanceVariableNames: ''!

!classDefinition: #SendersChainBrowser category: #MessageChainBrowsers!
MessageChainBrowser subclass: #SendersChainBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'SendersChainBrowser class' category: #MessageChainBrowsers!
SendersChainBrowser class
	instanceVariableNames: ''!

!classDefinition: #MessageChainExplorer category: #MessageChainBrowsers!
AbstractHierarchicalList subclass: #MessageChainExplorer
	instanceVariableNames: 'methodReference'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageChainExplorer class' category: #MessageChainBrowsers!
MessageChainExplorer class
	instanceVariableNames: ''!

!classDefinition: #ImplementorsChainExplorer category: #MessageChainBrowsers!
MessageChainExplorer subclass: #ImplementorsChainExplorer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'ImplementorsChainExplorer class' category: #MessageChainBrowsers!
ImplementorsChainExplorer class
	instanceVariableNames: ''!

!classDefinition: #SendersChainExplorer category: #MessageChainBrowsers!
MessageChainExplorer subclass: #SendersChainExplorer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'SendersChainExplorer class' category: #MessageChainBrowsers!
SendersChainExplorer class
	instanceVariableNames: ''!

!classDefinition: #MessageChainExplorerMorph category: #MessageChainBrowsers!
HierarchicalListMorph subclass: #MessageChainExplorerMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageChainExplorerMorph class' category: #MessageChainBrowsers!
MessageChainExplorerMorph class
	instanceVariableNames: ''!

!classDefinition: #ImplementorsChainExplorerMorph category: #MessageChainBrowsers!
MessageChainExplorerMorph subclass: #ImplementorsChainExplorerMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'ImplementorsChainExplorerMorph class' category: #MessageChainBrowsers!
ImplementorsChainExplorerMorph class
	instanceVariableNames: ''!

!classDefinition: #SendersChainExplorerMorph category: #MessageChainBrowsers!
MessageChainExplorerMorph subclass: #SendersChainExplorerMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'SendersChainExplorerMorph class' category: #MessageChainBrowsers!
SendersChainExplorerMorph class
	instanceVariableNames: ''!

!classDefinition: #MessageChainBrowserWindow category: #MessageChainBrowsers!
CodeWindow subclass: #MessageChainBrowserWindow
	instanceVariableNames: 'chainMorph codePane chainExplorer rightPane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageChainBrowserWindow class' category: #MessageChainBrowsers!
MessageChainBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #ImplementorsChainBrowserWindow category: #MessageChainBrowsers!
MessageChainBrowserWindow subclass: #ImplementorsChainBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'ImplementorsChainBrowserWindow class' category: #MessageChainBrowsers!
ImplementorsChainBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #SendersChainBrowserWindow category: #MessageChainBrowsers!
MessageChainBrowserWindow subclass: #SendersChainBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'SendersChainBrowserWindow class' category: #MessageChainBrowsers!
SendersChainBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #MessageChainListItemMorph category: #MessageChainBrowsers!
IndentingListItemMorph subclass: #MessageChainListItemMorph
	instanceVariableNames: 'chainHighlight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageChainListItemMorph class' category: #MessageChainBrowsers!
MessageChainListItemMorph class
	instanceVariableNames: ''!

!classDefinition: #MessageImplementorsWrapper category: #MessageChainBrowsers!
ListItemWrapper subclass: #MessageImplementorsWrapper
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageImplementorsWrapper class' category: #MessageChainBrowsers!
MessageImplementorsWrapper class
	instanceVariableNames: ''!

!classDefinition: #MessageSendersWrapper category: #MessageChainBrowsers!
ListItemWrapper subclass: #MessageSendersWrapper
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MessageSendersWrapper class' category: #MessageChainBrowsers!
MessageSendersWrapper class
	instanceVariableNames: ''!

!classDefinition: #MethodImplementorsWrapper category: #MessageChainBrowsers!
ListItemWrapper subclass: #MethodImplementorsWrapper
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageChainBrowsers'!
!classDefinition: 'MethodImplementorsWrapper class' category: #MessageChainBrowsers!
MethodImplementorsWrapper class
	instanceVariableNames: ''!


!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:51:32'!
acceptedContents
	^ self selectedMethod ifNotNil: [:selected |
		selected isSymbol ifTrue: [''] ifFalse: [selected  sourceCode ]]
	ifNil: ['']! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
contents: aString notifying: aRequestor

	| category selector class oldSelector |
	
	self okayToAccept ifFalse: [^ false].
	self selectedMessage ifNil: [^ false].
	class _ self selectedClassOrMetaClass.
	oldSelector _ self selectedMessage.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	"selector == oldSelector ifFalse: [
		self reformulateListNoting: selector]."
	self triggerEvent: #annotationChanged.
	^ true! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
contentsSelection
	^ nil! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
editorClassFor: aSelector
	^ SmalltalkEditor! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
method
	^ method! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
method: aMethodReference

	method _ aMethodReference! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
selected: aMethodReference
	selectedMethod _ aMethodReference.
	self changed: #acceptedContents! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
selectedClassOrMetaClass
	^ self selectedMethod ifNotNil: [:m |
		m actualClass ]! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
selectedMessageName
	^ self selectedMethod ifNotNil: [:m |
		m selector]! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
selectedMethod
	^ selectedMethod ifNil: [method]! !

!MessageChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:52:10'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self selectedMethod ifNotNil: [:selected|
		selected isSymbol ifFalse: [anSHTextStyler classOrMetaClass: selected actualClass]].
	^ true! !

!MessageChainBrowser class methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:35:58'!
on: aMethodReference
	^ self new method: aMethodReference! !

!ImplementorsChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:57:20'!
acceptedContents
	^ self selectedMethod ifNotNil: [:selected |
		selected isSymbol ifTrue: [''] ifFalse: [selected  sourceCode ]]
	ifNil: ['']! !

!ImplementorsChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:56:54'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self selectedMethod ifNotNil: [:selected|
		selected isSymbol ifFalse: [anSHTextStyler classOrMetaClass: selected actualClass]].
	^ true! !

!SendersChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 21:47:20'!
acceptedContents
	^ self selectedMethod ifNotNil: [:methodReference |
		methodReference  sourceCode ]
	ifNil: ['']! !

!SendersChainBrowser methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 21:47:44'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self selectedMethod ifNotNil: [:methodReference|
		anSHTextStyler classOrMetaClass: methodReference actualClass].
	^ true! !

!SendersChainBrowser class methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 21:28:43'!
on: aMethodReference
	^ self new method: aMethodReference! !

!MessageChainExplorer methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:39:52'!
initialize: aMethodReference
	methodReference _ aMethodReference! !

!MessageChainExplorer methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:41:49'!
setSelectedTo: anObject
	self triggerEvent: #selected with: anObject! !

!MessageChainExplorer class methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:39:52'!
on: aMethodReference
	^ self new initialize: aMethodReference! !

!ImplementorsChainExplorer methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:43:32'!
getList

	^Array with: (MethodImplementorsWrapper with: methodReference  model: self)! !

!SendersChainExplorer methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 16:19:47'!
getList

	^Array with: (MessageSendersWrapper with: methodReference  model: self)! !

!SendersChainExplorer class methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 16:20:17'!
on: aMethodReference
	^ self new initialize: aMethodReference! !

!MessageChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:50'!
expandRoot
	scroller submorphs first toggleExpandedState ! !

!MessageChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:50'!
highlightCurrentChain: aMorph

	|indentLevel index currentListItem |
	
	indentLevel _ aMorph indentLevel.
	"index _ self visualSelectionIndex."
	index _ scroller submorphs indexOf: aMorph.
	
	scroller submorphs do: [:li | li chainHighlight: false].
	
	[indentLevel > 1] whileTrue: [
		indentLevel _ indentLevel - 1.
		index _ index - 1.
		currentListItem _ self listMorphAt: index.
		[currentListItem indentLevel = indentLevel] whileFalse: [
			index _ index - 1.
			currentListItem _ self listMorphAt: index].
		currentListItem chainHighlight: true].
	
	currentListItem _ self listMorphAt: 1.
	currentListItem chainHighlight: true.! !

!MessageChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:40:03'!
indentingItemClass
	
	^MessageChainListItemMorph ! !

!MessageChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:50'!
setSelectedMorph: aMorph
	aMorph ifNotNil: [
		self highlightCurrentChain: aMorph].
	
	^ super setSelectedMorph: aMorph.! !

!MessageChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:50'!
visualizeCurrentChain: aMorph

	|indentLevel index currentListItem |
	
	indentLevel _ aMorph indentLevel.
	"index _ self visualSelectionIndex."
	index _ scroller submorphs indexOf: aMorph.
	
	scroller submorphs do: [:li | li visible: false].
	
	[indentLevel > 1] whileTrue: [
		indentLevel _ indentLevel - 1.
		index _ index - 1.
		currentListItem _ self listMorphAt: index.
		[currentListItem indentLevel = indentLevel] whileFalse: [
			index _ index - 1.
			currentListItem _ self listMorphAt: index].
		currentListItem visible: true].
	
	currentListItem _ self listMorphAt: 1.
	currentListItem visible: true.! !

!ImplementorsChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:40:03'!
indentingItemClass
	
	^ MessageChainListItemMorph ! !

!SendersChainExplorerMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:40:03'!
indentingItemClass
	
	^MessageChainListItemMorph ! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:24'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	^ (TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)'! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:38:37'!
buildMorphicWindow

	self layoutMorph beRow.
	
	chainExplorer _ self initializeChainExplorer.
	chainExplorer when: #selected send: #selected: to: model.
	
	rightPane _ LayoutMorph newColumn.

	chainMorph _ self initializeChainExplorerMorph.
	chainMorph selectionIndex: 1; expandRoot.
			
	codePane _ self buildMorphicCodePane.
			
	self layoutMorph addMorph: chainMorph proportionalWidth: 0.333.
	
	rightPane addMorph: self optionalButtonRow fixedHeight: 30.
	rightPane addMorphUseAll: codePane.
	self layoutMorph addAdjusterAndMorph: rightPane proportionalWidth: 0.666.
	! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/8/2020 12:34:15'!
initializeChainExplorer

	^ self subclassResponsibility ! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/8/2020 12:34:07'!
initializeChainExplorerMorph

	^ self subclassResponsibility ! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 08:36:24'!
label
	^ 'Senders chain of ', model printString! !

!MessageChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/8/2020 12:38:45'!
messageListMenu
	| aMenu itemColl |
	
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message Chain'.
	itemColl _ OrderedCollection new.
	
	itemColl addAll:
			{
				{#label -> 'senders (n)'. #selector -> #browseSenders. #icon -> #mailForwardIcon} asDictionary. 
				{#label -> 'implementors (m)'. #selector -> #browseImplementors. #icon -> #developmentIcon} asDictionary. 
				{#label -> 'method inheritance (h)'. #selector -> #methodHierarchy. #icon -> #goDownIcon} asDictionary. 
				{#label -> 'versions (v)'. #selector -> #browseVersions. #icon -> #clockIcon} asDictionary
			} .
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!MessageChainBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 2/18/2020 10:47:30'!
windowColor
    ^ Theme current messageSet! !

!MessageChainBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:49:46'!
open: aMethodReference

	|title model|
	
	model _ SendersChainBrowser on: aMethodReference.
	
	title _ 'Senders chain of ', aMethodReference actualClass asString, '>>', aMethodReference selector.

	^self open: model label: 'Chain of senders'! !

!ImplementorsChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:44:46'!
initializeChainExplorer

	^ ImplementorsChainExplorer on: model method! !

!ImplementorsChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/8/2020 12:35:03'!
initializeChainExplorerMorph

	^ ImplementorsChainExplorerMorph
			model: chainExplorer
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #openMessageListMenu
			keystrokeAction: #explorerKey:from:! !

!ImplementorsChainBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 17:11:37'!
messageListMenuOptions
	
	^ `{
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		21.
				#label 			-> 		'chain of implementors of...'.
				#selector 		-> 		#browseImplementorsChain.
				#icon 			-> 		#developmentIcon.
			} asDictionary.
		}`! !

!ImplementorsChainBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:50:28'!
open: aMethodReference

	|model|
	
	model _ ImplementorsChainBrowser on: aMethodReference.
	
	^self open: model label: 'Chain of implementors of ...'! !

!SendersChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 17:02:14'!
initializeChainExplorer

	^ SendersChainExplorer on: model method! !

!SendersChainBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 2/8/2020 12:34:57'!
initializeChainExplorerMorph

	^ SendersChainExplorerMorph
			model: chainExplorer
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #openMessageListMenu
			keystrokeAction: #explorerKey:from:! !

!SendersChainBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 2/4/2020 10:04:56'!
messageListMenuOptions
	
	^ `{
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'chain of senders of...'.
				#selector 		-> 		#browseSendersChain.
				#icon 			-> 		#mailForwardIcon.
			} asDictionary.
		}`! !

!SendersChainBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 2/11/2020 15:49:02'!
open: aMethodReference

	|title model|
	
	model _ SendersChainBrowser on: aMethodReference.
	
	title _ 'Senders chain of ', aMethodReference actualClass asString, '>>', aMethodReference selector.

	^self open: model label: 'Chain of senders of...'! !

!MessageChainListItemMorph methodsFor: 'accessing' stamp: 'MM 2/4/2020 09:56:40'!
chainHighlight
	"Answer the value of chainHighlight"

	^ chainHighlight! !

!MessageChainListItemMorph methodsFor: 'accessing' stamp: 'MM 2/4/2020 10:00:07'!
chainHighlight: anObject
	"Set the value of chainHighlight"

	chainHighlight _ anObject.
	self redrawNeeded.! !

!MessageChainListItemMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/4/2020 09:56:47'!
drawOn: aCanvas

	super drawOn: aCanvas.
	
	chainHighlight ifTrue: [
		aCanvas
			fillRectangle: (`0@0` extent: extent)
			color: (Color orange alpha: 0.3) ].! !

!MessageChainListItemMorph methodsFor: 'as yet unclassified' stamp: 'MM 2/4/2020 09:56:51'!
initialize
	chainHighlight _ false.
	super initialize.! !

!MessageImplementorsWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:34:59'!
contents
	^ contents ifNil: [contents _ (Smalltalk allImplementorsOf: item)
		collect: [:methodRef | MethodImplementorsWrapper with: methodRef]]! !

!MessageImplementorsWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:32:39'!
settingSelector
	^ #setSelectedTo:! !

!MessageSendersWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 16:26:09'!
asString

	^ item actualClass asString, '>>', item selector asString! !

!MessageSendersWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/3/2020 16:24:27'!
contents
	^ contents ifNil: [contents _ (Smalltalk allCallsOn: item selector)
		collect: [:methodRef | MessageSendersWrapper with: methodRef]]! !

!MessageSendersWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 17:00:47'!
settingSelector
	^ #setSelectedTo:! !

!MethodImplementorsWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:36:08'!
asString

	^ item actualClass asString, '>>', item selector asString! !

!MethodImplementorsWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/11/2020 15:43:51'!
contents
	^ contents ifNil: [contents _ item compiledMethod messages
		collect: [:selector | |implementors|
			implementors _ Smalltalk allImplementorsOf: selector.
			implementors size == 1 
				ifTrue: [MethodImplementorsWrapper with: implementors first]
				ifFalse: [MessageImplementorsWrapper with: selector]]]! !

!MethodImplementorsWrapper methodsFor: 'as yet unclassified' stamp: 'MM 2/5/2020 16:32:27'!
settingSelector
	^ #setSelectedTo:! !

!CodeWindow methodsFor: '*MessageChainBrowsers' stamp: 'MM 2/5/2020 10:13:05'!
browseImplementorsChain
	"Create and schedule a message set browser on all senders of the currently selected message selector.  Of there is no message currently selected, offer a type-in"
	
	ImplementorsChainBrowserWindow open: 
		(MethodReference class: model selectedClassOrMetaClass 
								selector: model selectedMessageName ).

	 ! !

!CodeWindow methodsFor: '*MessageChainBrowsers' stamp: 'MM 2/3/2020 18:31:27'!
browseSendersChain
	"Create and schedule a message set browser on all senders of the currently selected message selector.  Of there is no message currently selected, offer a type-in"
	
	SendersChainBrowserWindow open: 
		(MethodReference class: model selectedClassOrMetaClass 
								selector: model selectedMessageName ).

	 ! !
