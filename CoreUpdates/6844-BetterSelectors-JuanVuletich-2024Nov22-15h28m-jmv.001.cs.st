'From Cuis7.1 [latest update: #6843] on 22 November 2024 at 3:30:25 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:28:55'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 					false)
		#(5		'browse package (p)' 								$p 	window 			browsePackage 										packageIcon)
		#(6		'-----')
		#(10		'fileOut (o)' 								$o 	model 			fileOutMessageCategories 										fileOutIcon)
		#(20 		'-----')
		#(30		'reorganize (r)' 								$r 	model 			editMessageCategories 										sendReceiveIcon)
		#(40		'alphabetize (a)' 								$a 	model 			alphabetizeMessageCategories 										fontXGenericIcon)
		#(50		'remove empty categories (e)' 								$e 	model 			removeEmptyCategories 										listRemoveIcon)
		#(60		'categorize all uncategorized (c)' 								$c 	model 			categorizeAllUncategorizedMethods 										packageIcon)
		#(70		'new category... (n)' 								$n 	model 			addCategory 										newIcon)
		#(80		'new extension category... (N)' 								$N 	model 			addExtensionCategory 										listAddIcon)
		#(90 		'-----')
		#(100		'rename... (R)' 								$R 	model 			renameCategory 										saveAsIcon)
		#(110		'remove (x)' 								$x 	model 			removeMessageCategory 										deleteIcon)
		#(120 		'-----')
		#(130		'run tests (t)' 								$t 	model 			runMessageCategoryTests 										weatherFewCloudsIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:29:41'!
systemCatListMenuSpec
	"Spec for the menu and shortcut keys for the system categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 	false)
		#(10	'find class... (f)' 		$f 	window 	findClass 								findIcon)
		#(20	'recent classes... (r)' 	$r 	model 		recent 									clockIcon)
		#(30 	'-----')
		#(40	'browse all (B)' 		$B 	window 	browseAllClasses 						editFindReplaceIcon)
		#(50	'browse (b)' 			$b 	window 	openSystemCategoryBrowser 			packageIcon)
		#(55	'browse package (p)' 	$p 	window 	browsePackage 						packageIcon)
		#(60 	'-----')
		#(70	'fileOut (o)' 			$o 	model 		fileOutSystemCategory 				fileOutIcon)
		#(80 	'-----')
		#(90	'reorganize' 			nil 	model 		editSystemCategories 					sendReceiveIcon)
		#(100	'alphabetize (A)' 		$A 	model 		alphabetizeSystemCategories 			fontXGenericIcon)
		#(110 	'-----')
		#(120	'update (u)' 			$u 	model 		updateSystemCategories 				updateIcon)
		#(130	'add item... (a)' 		$a 	model 		addSystemCategory 					newIcon)
		#(140	'rename... (R)' 		$R 	model 		renameSystemCategory 				saveAsIcon)
		#(150	'move to... (m)' 		$m 	model 		moveAllToOtherSystemCategory 		saveAsIcon)
		#(160	'remove (x)' 			$x 	model 		removeSystemCategory 				deleteIcon)
		#(170 	'-----')
		#(180	'move to top' 		nil 	model 		moveSystemCategoryTop 				goTopIcon)
		#(190	'move up' 			nil 	model 		moveSystemCategoryUp 				goUpIcon)
		#(200	'move down' 		nil 	model 		moveSystemCategoryDown 			goDownIcon)
		#(210	'move to bottom' 	nil 	model 		moveSystemCategoryBottom 			fileOutIcon)
		#(220 	'-----')
		#(230	'run tests (t)' 		$t 	model 		runSystemCategoryTests 				weatherFewCloudsIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:29:00'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' 			false)
				#(10		'fileIn' 					nil 	model 			fileInMessageCategories 								updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' 			false)
			)].
	answer := answer, 
			#(
				#(20		'fileOut' 					nil 	model 			fileOutMessageCategories 								fileOutIcon)
				#(30 		'-----')
				#(40		'reorganize' 					nil 	model 			editMessageCategories 								sendReceiveIcon)
				#(50 		'-----')
				#(60		'add item...' 					nil 	model 			addCategory 								newIcon)
				#(70 		'-----')
				#(80		'rename...' 					nil 	model 			renameCategory 								saveAsIcon)
				#(90		'remove' 					nil 	model 			removeMessageCategory 								listRemoveIcon)
			).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 		'-----')
				#(100		'remove existing' 					nil 	model 			removeUnmodifiedMethods 								deleteIcon)
		) ].
	^answer! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 11/22/2024 15:29:11'!
buildMorphicMessageCatList

	^(PluggableListMorph
		withModel: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:)
			menuNshortcutsKey: #messageCatListMenuSpec.
! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 11/22/2024 15:29:47'!
buildMorphicSystemCategoryList
	^(HierarchicalListMorph
		withModel: model
		listGetter: #systemCategoryRoots
		indexGetter: #selectedSystemCategory
		indexSetter: #setSelectedSystemCategoryTreeItem:)
			menuNshortcutsKey: #systemCatListMenuSpec;
			autoDeselect: true;
			autoExpand: true;
			yourself! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 11/22/2024 15:29:15'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList := (PluggableListMorph
			withModel: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:)
				menuNshortcutsKey: #codeFileListMenuSpec.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList := (PluggableListMorph
			withModel: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:)
				menuNshortcutsKey: #messageCatListMenuSpec.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !

!methodRemoval: CodeFileBrowserWindow #methodCategoriesListMenuSpec stamp: 'jmv 11/22/2024 15:29:24'!
CodeFileBrowserWindow removeSelector: #methodCategoriesListMenuSpec!
!methodRemoval: BrowserWindow #systemCategoriesListMenuSpec stamp: 'jmv 11/22/2024 15:29:50'!
BrowserWindow removeSelector: #systemCategoriesListMenuSpec!
!methodRemoval: BrowserWindow #methodCategoriesListMenuSpec stamp: 'jmv 11/22/2024 15:29:23'!
BrowserWindow removeSelector: #methodCategoriesListMenuSpec!
