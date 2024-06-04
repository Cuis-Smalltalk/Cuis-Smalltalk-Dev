'From Cuis7.1 [latest update: #6431] on 4 June 2024 at 6:11:25 am'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:10:30'!
systemCatSingletonMenuSpec
	"Spec for the menu and shortcut keys the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class category' 					false)
		#(10		'find class... (f)' 						$f 	window 			findClass 									findIcon)
		#(20 		'-----')
		#(30		'browse all (B)' 						$B 	window 			browseAllClasses 									editFindReplaceIcon)
		#(40		'browse (b)' 						$b 	window 			openSystemCategoryBrowser 									packageIcon)
		#(50 		'-----')
		#(60		'fileOut (o)' 						$o 	model 			fileOutSystemCategory 									fileOutIcon)
		#(70 		'-----')
		#(80		'update (u)' 						$u 	model 			updateSystemCategories 									updateIcon)
		#(90		'rename... (R)' 						$R 	model 			renameSystemCategory 									saveAsIcon)
		#(100		'remove (x)' 						$x 	model 			removeSystemCategory 									deleteIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:10:23'!
systemCategoriesListMenuSpec
	"Spec for the menu and shortcut keys the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 					false)
		#(10		'find class... (f)' 						$f 	window 			findClass 									findIcon)
		#(20		'recent classes... (r)' 						$r 	model 			recent 									clockIcon)
		#(30 		'-----')
		#(40		'browse all (B)' 						$B 	window 			browseAllClasses 									editFindReplaceIcon)
		#(50		'browse (b)' 						$b 	window 			openSystemCategoryBrowser 									packageIcon)
		#(60 		'-----')
		#(70		'fileOut (o)' 						$o 	model 			fileOutSystemCategory 									fileOutIcon)
		#(80 		'-----')
		#(90		'reorganize' 						nil 	model 			editSystemCategories 									sendReceiveIcon)
		#(100		'alphabetize (A)' 						$A 	model 			alphabetizeSystemCategories 									fontXGenericIcon)
		#(110 		'-----')
		#(120		'update (u)' 						$u 	model 			updateSystemCategories 									updateIcon)
		#(130		'add item... (a)' 						$a 	model 			addSystemCategory 									newIcon)
		#(140		'rename... (R)' 						$R 	model 			renameSystemCategory 									saveAsIcon)
		#(150		'move to... (m)' 						$m 	model 			moveAllToOtherSystemCategory 									saveAsIcon)
		#(160		'remove (x)' 						$x 	model 			removeSystemCategory 									deleteIcon)
		#(170 		'-----')
		#(180		'move to top' 						nil 	model 			moveSystemCategoryTop 									goTopIcon)
		#(190		'move up' 						nil 	model 			moveSystemCategoryUp 									goUpIcon)
		#(200		'move down' 						nil 	model 			moveSystemCategoryDown 									goDownIcon)
		#(210		'move to bottom' 						nil 	model 			moveSystemCategoryBottom 									fileOutIcon)
		#(220 		'-----')
		#(230		'run tests (t)' 						$t 	model 			runSystemCategoryTests 									weatherFewCloudsIcon)
	)! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/1/2024 11:20:24'!
buildMorphicSystemCategoryList
	^(HierarchicalListMorph
		withModel: model
		listGetter: #systemCategoryRoots
		indexGetter: #selectedSystemCategory
		indexSetter: #setSelectedSystemCategoryTreeItem:)
			menuNshortcutsKey: #systemCategoriesListMenuSpec;
			autoDeselect: true;
			autoExpand: true;
			yourself! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/1/2024 11:19:09'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList := (PluggableListMorph
				withModel: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:)
					menuNshortcutsKey: #systemCatSingletonMenuSpec.
	mySingletonList hideScrollBarsIndefinitely.

	classList := self buildMorphicClassList.
	classColumn := self buildMorphicClassColumnWith: classList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!methodRemoval: HierarchyBrowserWindow #systemCatSingletonKey:from: stamp: 'jmv 6/4/2024 06:09:21'!
HierarchyBrowserWindow removeSelector: #systemCatSingletonKey:from:!
!methodRemoval: BrowserWindow class #systemCategoryMenuOptions stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow class removeSelector: #systemCategoryMenuOptions!
!methodRemoval: BrowserWindow class #systemCatSingletonMenuOptions stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow class removeSelector: #systemCatSingletonMenuOptions!
!methodRemoval: BrowserWindow #systemCategoryMenu stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow removeSelector: #systemCategoryMenu!
!methodRemoval: BrowserWindow #systemCatSingletonKey:from: stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow removeSelector: #systemCatSingletonKey:from:!
!methodRemoval: BrowserWindow #systemCatSingletonMenu stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow removeSelector: #systemCatSingletonMenu!
!methodRemoval: BrowserWindow #systemCatListKey:from: stamp: 'jmv 6/4/2024 06:09:21'!
BrowserWindow removeSelector: #systemCatListKey:from:!
