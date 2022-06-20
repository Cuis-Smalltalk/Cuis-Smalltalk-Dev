'From Cuis 6.0 [latest update: #5303] on 17 June 2022 at 11:19:28 am'!
!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView leftSibling rightSibling scrollSiblings '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph commentStamp: '<historical>' prior: 0!
When a PluggableListMorph is in focus, type in a letter (or several letters quickly) to go to the next item that begins with that letter. Special keys (up, down, home, etc.) are also supported.

It is possible to set sibling lists for multiscrolling by calling >>#scrollSiblings: with a collection of siblings. The sibling lists are scrolled vertically together with the receiver. For an example of usage see CodePackageListWindow >>#buildMorphicWindow.
!


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'len 6/17/2022 07:27:35'!
currentChangeSetName
	^ (((myChangeSet isForBaseSystem or: [ myChangeSet codePackage notNil ]) and: [ myChangeSet hasUnsavedChanges ]) ifTrue: ['*'] ifFalse: ['']), myChangeSet name! !


!SingleSetChangeSorter methodsFor: 'accessing' stamp: 'len 6/17/2022 07:28:14'!
currentChangeSetName
	^ (myChangeSet hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), myChangeSet name! !


!HandMorph methodsFor: 'focus handling' stamp: 'len 6/17/2022 11:13:10'!
keyboardFocusNextWithinWindow
	| window |
	window _ (keyboardFocus ifNil: [^ false]) owningWindow.
	(keyboardFocus previousMorphThat: [ :m |
		m visible and: [m handlesKeyboard and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			^true ].
	^false! !

!HandMorph methodsFor: 'focus handling' stamp: 'len 6/17/2022 11:13:03'!
keyboardFocusPreviousWithinWindow
	| window |
	window _ (keyboardFocus ifNil: [^ false]) owningWindow.
	(keyboardFocus nextMorphThat: [ :m |
		m visible and: [m handlesKeyboard and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			^true ].
	^false! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'len 6/17/2022 11:18:26'!
focusKeyboardFor: aKeyboardEvent
{aKeyboardEvent . aKeyboardEvent isTab . aKeyboardEvent shiftPressed . aKeyboardEvent controlKeyPressed} print.
	(super focusKeyboardFor: aKeyboardEvent) ifTrue: [^ true].
	aKeyboardEvent shiftPressed ifTrue: [
		aKeyboardEvent isArrowLeft
			ifTrue: [ aKeyboardEvent hand keyboardFocusPreviousWithinWindow. ^ true ].
		aKeyboardEvent isArrowRight
			ifTrue: [ aKeyboardEvent hand keyboardFocusNextWithinWindow. ^ true ]].
	aKeyboardEvent isTab ifTrue: [
		aKeyboardEvent shiftPressed
			ifTrue: [ aKeyboardEvent hand keyboardFocusPreviousWithinWindow ]
			ifFalse: [ aKeyboardEvent hand keyboardFocusNextWithinWindow ]].
	^ true! !


!PluggableListMorph methodsFor: 'events' stamp: 'len 6/17/2022 10:55:01'!
focusKeyboardFor: aKeyboardEvent
	(super focusKeyboardFor: aKeyboardEvent) ifTrue: [^ true].
{aKeyboardEvent . aKeyboardEvent isTab . aKeyboardEvent shiftPressed . aKeyboardEvent controlKeyPressed} print.
	(aKeyboardEvent isArrowLeft or: [aKeyboardEvent isTab and: [aKeyboardEvent shiftPressed]])
		ifTrue: [ aKeyboardEvent hand keyboardFocusPreviousWithinWindow. ^ true ].
	(aKeyboardEvent isArrowRight or: [aKeyboardEvent isTab])
		ifTrue: [ aKeyboardEvent hand keyboardFocusNextWithinWindow. ^ true ].
	^ false! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:39:41'!
updateSiblingsScroller
	"Update the siblings scroller to match the receiver."
	(self valueOfProperty: #scrollSiblings) ifNotNil: [:aCollection|
		| yOffset |
		yOffset _ self scrollerOffset y.
		aCollection do: [:each| each scrollerOffset: each scrollerOffset x @ yOffset]]! !


!ChangeSorter methodsFor: 'access' stamp: 'len 6/17/2022 07:26:02'!
showChangeSet: chgSet

	myChangeSet == chgSet ifFalse: [
		myChangeSet _ chgSet.
		currentClassName _ nil.
		currentSelector _ nil].
	self changed: #relabel.
	self changed: #currentChangeSetName.	"new -- list of sets"
	self changed: #mainButtonName.	"old, button"
	self changed: #classList.
	self changed: #messageList.
	self acceptedContentsChanged.! !

!ChangeSorter methodsFor: 'access' stamp: 'len 6/17/2022 07:24:21'!
showChangeSetNamed: aString
	| name |
	name _ (aString beginsWith: '*') ifTrue: [aString allButFirst] ifFalse: [aString].
	self showChangeSet: (ChangeSet changeSetNamed: name) ! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'len 6/17/2022 07:14:27'!
changeSetList
	"Answer a list of ChangeSet names to be shown in the change sorter."

	^ (ChangeSet allChangeSets collect: [ :each |
		(((each isForBaseSystem or: [ each codePackage notNil ]) and: [ each hasUnsavedChanges ]) ifTrue: ['*'] ifFalse: ['']), each name ]) reversed! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'len 6/17/2022 07:15:33'!
updateIfNeeded
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList ].

	self acceptedContentsChanged! !


!SingleSetChangeSorter methodsFor: 'accessing' stamp: 'len 6/17/2022 07:18:34'!
changeSetList
	^{ (myChangeSet hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), myChangeSet name }! !


!CodePackageList methodsFor: 'accessing' stamp: 'len 6/17/2022 07:06:51'!
packageNames
	^ packages collect: [ :each | (each hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), each packageNameAndVersion ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'len 6/17/2022 07:07:23'!
selectionIndex: anInteger

	selection _ anInteger = 0 ifFalse: [ packages at: anInteger ].
	self
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements! !

!CodePackageList methodsFor: 'events' stamp: 'len 6/17/2022 07:08:14'!
updatePackageList
	
	| newPackages oldPackages |
	oldPackages _ (packages ifNil: [#()]) asIdentitySet.
	newPackages _ CodePackage installedPackages asIdentitySet.
	oldPackages do: [ :old |
		(newPackages includes: old) ifFalse: [
			old removeActionsWithReceiver: self ]].
	newPackages do: [ :new |
		(oldPackages includes: new) ifFalse: [
			new when: #dirtyFlagChanged send:#updatePackageList to: self ]].
	newPackages _ newPackages asArray sort: [ :a :b |
		 a packageName < b packageName ].
	packages _ newPackages.
	self
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 12/21/2018 09:21:39'!
focusKeyboardFor: aKeyboardEvent

	"If aKeyboardEvent ctrl-tab or shift-ctrl-tab use it to navigate keyboard focus.
	Warning: This doesn't work on Windows... the event is not sent"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent controlKeyPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ]
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ].
			^ true ].
	"On Windows use at least some keystroke to navigate morphs... even shift-Tab that should navigate backwards"
"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent shiftPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent hand keyboardFocusNext.
			^ true ].
"

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'len 6/17/2022 07:33:29'!
initialize
	super initialize.
	scroller morphWidth: extent x! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'len 6/17/2022 06:48:51'!
arrowKey: aKeyboardEvent
	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:40:12'!
scrollBy: delta
	"Scroll self and any siblings"
	super scrollBy: delta.
	self updateSiblingsScroller! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:40:38'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			r _ scroller externalizeBoundingRectOf: (scroller drawBoundsForRow: row).
			self scrollToShow: r ].
	self updateSiblingsScroller! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:43:05'!
scrollSiblings: aCollection
	"Set a collection of scroll panes to be scroller together with the receiver."
	self setProperty: #scrollSiblings toValue: aCollection! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:41:26'!
scrollToShow: aRectangle
	super scrollToShow: aRectangle.
	self updateSiblingsScroller! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'len 6/17/2022 11:41:55'!
vScrollBarValue: scrollValue
	super vScrollBarValue: scrollValue.
	self updateSiblingsScroller! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'len 6/17/2022 11:44:13'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| names fileNames namesPane fileNamesPane 
	upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	namesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor;
			   alwaysHideVerticalScrollbar.
	fileNamesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: namesPane proportionalWidth: 0.4;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
	
	names scrollSiblings: {fileNames}.
	fileNames scrollSiblings: {names}.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 6/17/2022 07:30:20'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCategoryList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList allowItemDrag: true.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategory:class:.
		
	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.
	
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

!BrowserWindow methodsFor: 'GUI building' stamp: 'len 6/17/2022 07:30:29'!
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

	classList _ self buildMorphicClassList.
	classColumn _ self buildMorphicClassColumnWith: classList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (PreferenceNG at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'len 6/17/2022 07:30:42'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (PreferenceNG at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'len 6/17/2022 07:26:15'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !

!methodRemoval: PluggableListMorph #vPrivateScrollTo: stamp: 'len 6/17/2022 10:50:11'!
PluggableListMorph removeSelector: #vPrivateScrollTo:!
!methodRemoval: PluggableListMorph #rightSibling: stamp: 'len 6/17/2022 10:49:22'!
PluggableListMorph removeSelector: #rightSibling:!
!methodRemoval: PluggableListMorph #vScrollLeftSibling: stamp: 'len 6/17/2022 10:49:55'!
PluggableListMorph removeSelector: #vScrollLeftSibling:!
!methodRemoval: PluggableListMorph #gainFocusFrom: stamp: 'len 6/17/2022 10:51:03'!
PluggableListMorph removeSelector: #gainFocusFrom:!
!methodRemoval: PluggableListMorph #scrollMySiblings stamp: 'len 6/17/2022 10:48:53'!
PluggableListMorph removeSelector: #scrollMySiblings!
!methodRemoval: PluggableListMorph #navigateRight stamp: 'len 6/17/2022 10:50:58'!
PluggableListMorph removeSelector: #navigateRight!
!methodRemoval: PluggableListMorph #leftSibling:rightSibling: stamp: 'len 6/17/2022 10:48:57'!
PluggableListMorph removeSelector: #leftSibling:rightSibling:!
!methodRemoval: PluggableListMorph #leftSibling: stamp: 'len 6/17/2022 10:48:59'!
PluggableListMorph removeSelector: #leftSibling:!
!methodRemoval: PluggableListMorph #scrollSiblings stamp: 'len 6/17/2022 10:51:16'!
PluggableListMorph removeSelector: #scrollSiblings!
!methodRemoval: PluggableListMorph #vScrollRightSibling: stamp: 'len 6/17/2022 10:50:06'!
PluggableListMorph removeSelector: #vScrollRightSibling:!
!methodRemoval: PluggableListMorph #navigateLeft stamp: 'len 6/17/2022 10:50:58'!
PluggableListMorph removeSelector: #navigateLeft!
!methodRemoval: CodePackageList #updateDirtyFlags stamp: 'len 6/17/2022 10:49:05'!
CodePackageList removeSelector: #updateDirtyFlags!
!methodRemoval: CodePackageList #packageDirtyFlags stamp: 'len 6/17/2022 10:49:09'!
CodePackageList removeSelector: #packageDirtyFlags!
!methodRemoval: SingleSetChangeSorter #changeSetDirtyFlags stamp: 'len 6/17/2022 10:49:12'!
SingleSetChangeSorter removeSelector: #changeSetDirtyFlags!
!methodRemoval: ChangeSorter #changeSetDirtyFlags stamp: 'len 6/17/2022 10:49:14'!
ChangeSorter removeSelector: #changeSetDirtyFlags!
!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' rejectDrops)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateOnePageDown navigateOnePageUp navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo:)
('siblings')
('scrolling' scrollBy: scrollSelectionIntoView scrollSiblings: scrollToShow: updateSiblingsScroller vScrollBarValue:)
!

