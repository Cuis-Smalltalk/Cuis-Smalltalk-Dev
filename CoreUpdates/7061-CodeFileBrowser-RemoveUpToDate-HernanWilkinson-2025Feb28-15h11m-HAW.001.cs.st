'From Cuis7.3 [latest update: #7060] on 28 February 2025 at 3:11:17 pm'!

!CodeFileBrowser methodsFor: 'as yet unclassified' stamp: 'HAW 2/28/2025 15:09:16'!
removeUpToDate
	
	caseCodeSource classes do: [ :aPseudoClass |
		aPseudoClass removeUpToDateMethods.
		aPseudoClass theMetaClass removeUpToDateMethods ].
	self classListIndex: 0! !


!CodeFileBrowserWindow methodsFor: 'as yet unclassified' stamp: 'HAW 2/28/2025 09:28:35'!
removeUpToDate
	
	model removeUpToDate! !


!PseudoClass methodsFor: 'removing' stamp: 'HAW 2/28/2025 15:09:41'!
removeUpToDateMethods

	self selectors do: [ :aSelector | self removeIfUpToDate: aSelector ].
	self organization removeEmptyCategories.! !

!PseudoClass methodsFor: 'private' stamp: 'HAW 2/28/2025 15:06:18'!
removeIfUpToDate: aSelector

	| realClass pseudoSourceCode realSourceCode |

	realClass := self realClass ifNil: [^self ].
	realSourceCode := realClass sourceCodeAt: aSelector ifAbsent: [ nil ].
	pseudoSourceCode := self sourceCodeAt: aSelector.
	realSourceCode = pseudoSourceCode ifTrue: [ self removeMethod: aSelector ]! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'HAW 2/28/2025 13:17:27'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model systemCategoryList ifNotEmpty: [ 
		model setSelectedSystemCategory: model systemCategoryList first ].
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

!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'HAW 2/28/2025 09:27:08'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (model baseCodeSource isLiveSmalltalkImage and: [ model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples, 
			#((17	'remove up to date'		removeUpToDate 	'remove up to date methods...')) ]
		ifFalse: [
			"For non-standard browser configurations assume most of the default buttons are invalid"
			#(
			) ].! !

