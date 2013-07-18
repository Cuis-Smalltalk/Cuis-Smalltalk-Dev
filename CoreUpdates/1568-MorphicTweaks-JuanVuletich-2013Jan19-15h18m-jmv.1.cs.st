'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:22:36 pm'!

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:20'!
buildMorphicClassList

	^PluggableListMorph
		model: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:
		mainView: self
		menuGetter: #classListMenu
		keystrokeAction: #classListKey:from:! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:20'!
buildMorphicMessageCatList

	^PluggableListMorph
		model: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:
		mainView: self
		menuGetter: #messageCategoryMenu
		keystrokeAction: nil! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:20'!
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

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:20'!
buildMorphicSystemCatList

	^PluggableListMorph
		model: model
		listGetter: #systemCategoryList
		indexGetter: #systemCategoryListIndex
		indexSetter: #systemCategoryListIndex:
		mainView: self
		menuGetter: #systemCategoryMenu
		keystrokeAction: #systemCatListKey:from:! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:20'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicClassColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont height + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	self setUpdatablePanesFrom: #(classList messageCategoryList messageList).
	model changed: #editSelection! !


!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 1/19/2013 15:19'!
initialize
	"initialize the state of the receiver"
	super initialize.
	self clipSubmorphs: true! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:21'!
buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 _ PluggableListMorph
				model: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:
				mainView: self
				menuGetter: #processListMenu
				keystrokeAction: #processListKey:from:.
	list2 _ PluggableListMorph
				model: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:
				mainView: self
				menuGetter: #stackListMenu
				keystrokeAction: #stackListKey:from:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph _ TextModelMorph
				textProvider: model
				textGetter: #selectedMethod.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setUpdatablePanesFrom: #(#processNameList #stackNameList ).
	self setLabel: 'Process Browser'! !

!methodRemoval: Morph #enableDragNDrop!
Morph removeSelector: #enableDragNDrop!
!methodRemoval: Morph #enableDragNDrop:!
Morph removeSelector: #enableDragNDrop:!
