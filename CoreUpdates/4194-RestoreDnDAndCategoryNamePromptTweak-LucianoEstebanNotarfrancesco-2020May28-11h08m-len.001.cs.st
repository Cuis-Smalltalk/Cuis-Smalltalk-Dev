'From Cuis 5.0 [latest update: #4193] on 28 May 2020 at 11:21:51 am'!

!CodeProvider methodsFor: 'categories' stamp: 'len 5/28/2020 11:20:47'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	|  labels myCategories reject lines newName menuIndex |
	labels _ OrderedCollection with: 'new...'.
	labels addAll: (myCategories _ aClass organization categories asArray copy sort:
		[ :a :b | a asLowercase < b asLowercase ]).
	reject _ myCategories asSet.
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection with: 1 with: (myCategories size + 1).

	aClass allSuperclasses do: [ :cls | | cats |
			cats _ cls organization categories reject: [ :cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: (cats asArray sort: [ :a :b | a asLowercase < b asLowercase]).
				reject addAll: cats]].

	(labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: aPrompt.
		menuIndex = 0 ifTrue: [^ nil].
		menuIndex = 1])
			ifTrue:[
				newName _ self request: 'New category name?' initialAnswer: 'category-name'.
				newName isEmpty ifTrue: [ ^nil ]]
			ifFalse: [ newName _ labels at: menuIndex ].
	^ newName ifNotNil: [ newName asSymbol ]! !


!Browser methodsFor: 'message category functions' stamp: 'len 5/28/2020 11:19:42'!
newMethodCategoryNameIfNone: aNoneBlock

	| labels lines menuIndex newName reject |
	
	labels _ OrderedCollection with: 'new...'.
	reject _ Set new.
	reject
		addAll: self selectedClassOrMetaClass organization categories;
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection new.
	self selectedClassOrMetaClass allSuperclasses do: [:cls | | cats |
		cls = Object ifFalse: [
			cats _ cls organization categories reject:
				 [:cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: cats asArray sort.
				reject addAll: cats]]].
	newName _ (labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: 'Add Category'.
		menuIndex = 0 ifTrue: [^ aNoneBlock value].
		menuIndex = 1])
			ifTrue: [
				self request: 'New category name?'
					initialAnswer: 'category-name']
			ifFalse: [
				labels at: menuIndex].
	
	^ newName isEmpty
		ifTrue: aNoneBlock
		ifFalse: [newName asSymbol]! !


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

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/10/2020 14:38:33'!
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
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

