'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 8:18:20 am'!

!BrowserWindow class methodsFor: 'instance creation' stamp: 'HAW 2/21/2019 08:14:05'!
openNoSysCat: model label: aString

	self new
		model: model;
		buildNoSysCatMorphicWindow;
		setLabel: aString;
		openInWorld! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 2/21/2019 08:16:48'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicClassColumn proportionalWidth: 0.3;
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

!methodRemoval: CodeWindow class #openNoSysCat:label:!
CodeWindow class removeSelector: #openNoSysCat:label:!

!CodeWindow class reorganize!
('as yet unclassified')
!


!BrowserWindow class reorganize!
('GUI building' fullOnClass:selector: openBrowser)
('browser menues' classListMenu2Options classListMenuOptions messageCategoryMenuOptions messageListMenu2Options messageListMenuOptions systemCatSingletonMenuOptions systemCategoryMenuOptions)
('menu-world' worldMenuForOpenGroup)
('instance creation' openNoSysCat:label:)
!

