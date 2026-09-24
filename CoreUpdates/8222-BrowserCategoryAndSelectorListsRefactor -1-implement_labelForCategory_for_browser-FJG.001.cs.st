'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 6:12:07 pm'!

!CodeProvider methodsFor: 'private' stamp: 'FJG 6/Apr/2026, 3:18:11 pm (UTC)'!
labelForCategory: aCategorySymbol
	"This sets up a single extension point: we can override labelForCategory: to customize how
categories appear on the message category lists without touching the list-building logic."
	^ aCategorySymbol! !


!Browser methodsFor: 'message category list' stamp: 'FJG 6/Apr/2026, 3:17:37 pm (UTC)'!
messageCategoryItems

	^ self messageCategoryList collect: [:aCategory | self labelForCategory: aCategory]! !


!Browser methodsFor: 'dependency mechanism' stamp: 'FJG 6/Apr/2026, 3:16:55 pm (UTC)'!
changed: aSymbol
	
	super changed: aSymbol.

	aSymbol == #classList ifTrue: [
		self changed: #classItems
	].
	aSymbol == #messageCategoryList ifTrue: [
		self changed: #messageCategoryItems
	]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'FJG 6/Apr/2026, 3:19:30 pm (UTC)'!
buildMorphicMessageCatList

	^(PluggableListMorph
		withModel: model
		listGetter: #messageCategoryItems
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:)
			menuNshortcutsKey: #messageCatListMenuSpec.
! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'FJG 6/Apr/2026, 3:20:35 pm (UTC)'!
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
			listGetter: #messageCategoryItems
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:)
				menuNshortcutsKey: #messageCatListMenuSpec.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


'Migrate existing instances of BrowserWindow'!
BrowserWindow allSubInstances do: [:browserWindow |
	browserWindow allSubmorphsDo: [:aMorph |
		aMorph isMemberOf: PluggableListMorph
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #messageCategoryList ]
			:: ifTrue: [
				aMorph instVarNamed: 'getListSelector' put: #messageCategoryItems
			]
	]
]!

