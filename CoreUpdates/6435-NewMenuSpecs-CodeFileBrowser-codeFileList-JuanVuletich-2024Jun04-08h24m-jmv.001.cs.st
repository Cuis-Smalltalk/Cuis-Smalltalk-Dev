'From Cuis7.1 [latest update: #6433] on 4 June 2024 at 9:51:10 am'!

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:47:19'!
codeFileListMenuSpec
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10		'find class... (f)' 					$f 	window 			findClass)
				#(20 		'-----')
			).
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30		'fileIn' 					nil 	model 			fileIn) )].
	answer := answer, 
			#(
				#(40		'fileOut' 					nil 	model 			fileOut) ).
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50		'remove existing' 					nil 	model 			removeUnmodifiedClasses) )].
	^answer! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 09:49:27'!
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
				menuNshortcutsKey: #methodCategoriesListMenuSpec.

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

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:31:49'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(40		'browse full (b)' 					$b 	window 			browseMethodFull 						editFindReplaceIcon)
				#(50		'class refs (N)' 					$N 	window 			browseClassRefs 						classIcon)
				#(60 		'-----')
				#(70		'fileIn' 					nil 	model 			fileInClass 						updateIcon)
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			) ].! !

!methodRemoval: CodeFileBrowserWindow #codeFileListMenu stamp: 'jmv 6/4/2024 09:49:47'!
CodeFileBrowserWindow removeSelector: #codeFileListMenu!
!methodRemoval: CodeFileBrowserWindow #codeFileListKey:from: stamp: 'jmv 6/4/2024 09:49:30'!
CodeFileBrowserWindow removeSelector: #codeFileListKey:from:!

!CodeFileBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalButtonTuples windowColor)
('menu commands' browseMethodFull browseVersions findClass methodInheritance)
('menu & kbd shortcuts' classListMenuSpec codeFileListMenuSpec messageListMenuSpec methodCategoriesListMenuSpec)
!

