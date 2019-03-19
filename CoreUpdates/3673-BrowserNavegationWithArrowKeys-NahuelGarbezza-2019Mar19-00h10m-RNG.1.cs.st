'From Cuis 5.0 of 7 November 2016 [latest update: #3672] on 19 March 2019 at 12:25:43 am'!
"Change Set:		3673-CuisCore-AuthorName-2019Mar19-00h10m
Date:			19 March 2019
Author:			Nahuel Garbezza

Support navigation between PluggableListMorph using left and right arrows. Refactored the key events handler"!

!classDefinition: #PluggableListMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView leftSibling rightSibling '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!PluggableListMorph methodsFor: 'events' stamp: 'RNG 3/19/2019 00:18:17'!
gainFocusFrom: aHand

	aHand newKeyboardFocus: self.
	self getCurrentSelectionIndex = 0 ifTrue: [ self selectionIndex: 1 ].! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:24:13'!
navigateDown
	"move down, wrapping to top if needed"

	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection _ 1 ].
	
	self changeSelectionTo: nextSelection! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/19/2019 00:16:46'!
navigateLeft

	leftSibling ifNotNil: [ leftSibling gainFocusFrom: self activeHand ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:21:05'!
navigateOnePageDown

	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex + self numSelectionsInView min: self maximumSelection.
	
	self getCurrentSelectionIndex ~= nextSelection
		ifTrue: [ self changeSelectionTo: nextSelection ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:23:27'!
navigateOnePageUp

	self changeSelectionTo: (self minimumSelection max: self getCurrentSelectionIndex - self numSelectionsInView)! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/19/2019 00:16:35'!
navigateRight

	rightSibling ifNotNil: [ rightSibling gainFocusFrom: self activeHand ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:23:47'!
navigateToBottom
	
	self changeSelectionTo: self maximumSelection! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:23:57'!
navigateToTop
	
	self changeSelectionTo: 1! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/16/2019 14:24:07'!
navigateUp
	"move up, wrapping to bottom if needed"

	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex - 1.
	nextSelection < 1 ifTrue: [ nextSelection _ self maximumSelection ].
	
	self changeSelectionTo: nextSelection! !

!PluggableListMorph methodsFor: 'private' stamp: 'RNG 3/16/2019 14:22:49'!
changeSelectionTo: nextSelection

	self getCurrentSelectionIndex ~= nextSelection ifTrue: [
		| window |
		window _ self owningWindow.
		(window isNil or: [ window okToChangeDueTo: self ]) ifTrue: [
			"No change if model is locked"
			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.
			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		]
	]! !

!PluggableListMorph methodsFor: 'siblings' stamp: 'RNG 3/16/2019 14:08:59'!
leftSibling: aListMorphToTheLeft

	leftSibling _ aListMorphToTheLeft! !

!PluggableListMorph methodsFor: 'siblings' stamp: 'RNG 3/16/2019 14:08:42'!
leftSibling: aListMorphToTheLeft rightSibling: aListMorphToTheRight

	self leftSibling: aListMorphToTheLeft.
	self rightSibling: aListMorphToTheRight.! !

!PluggableListMorph methodsFor: 'siblings' stamp: 'RNG 3/16/2019 14:08:55'!
rightSibling: aListMorphToTheRight

	rightSibling _ aListMorphToTheRight! !


!PluggableListMorph methodsFor: 'events' stamp: 'RNG 3/16/2019 14:27:45'!
keyStroke: aKeyboardEvent 
	"Process keys"
	
	| aCharacter |
	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	aCharacter numericValue = 27 ifTrue: [	" escape key"
		^ self mouseButton2Activity].
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'RNG 3/19/2019 00:12:56'!
arrowKey: aKeyboardEvent

	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'RNG 3/16/2019 14:29:57'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCatList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	systemCatList rightSibling: classList.
	classList leftSibling: systemCatList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	classList makeItemsDraggable.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
		
	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'RNG 3/19/2019 00:24:39'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classColumn _ self buildMorphicClassColumn.
	classList _ classColumn submorphs third.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont height + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!methodRemoval: PluggableListMorph #leftKeyPressed!
PluggableListMorph removeSelector: #leftKeyPressed!
!methodRemoval: PluggableListMorph #rightKeyPressed!
PluggableListMorph removeSelector: #rightKeyPressed!
!classDefinition: #PluggableListMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView leftSibling rightSibling'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' acceptDropsFrom:performing:whenOutsideList: makeItemsDraggable makeItemsUndraggable rejectDrops)
('drawing' drawOn: flashRow:)
('dropping/grabbing' acceptDroppingMorph:atRow:withEvent: acceptDroppingMorph:event: acceptDroppingMorph:outsideListWithEvent: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' allowsMorphDrop handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: gainFocusFrom: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction: textColor)
('keyboard navigation' arrowKey: navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo: itemsAreDraggable listMorph)
('siblings' leftSibling: leftSibling:rightSibling: rightSibling:)
!

