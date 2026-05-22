Class {
	#name : 'MethodCategoriesPrompter',
	#superclass : 'Object',
	#instVars : [
		'startClass',
		'rejectingFirst',
		'categories',
		'lines',
		'reject'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'instance creation' }
MethodCategoriesPrompter class >> staringFrom: aClass rejectingFirst: rejectingFirst  [
	
	^self new initializeStaringFrom: aClass rejectingFirst: rejectingFirst 

]

{ #category : 'categories initialization - private' }
MethodCategoriesPrompter >> addCategories [

	startClass allSuperclasses do: [ :superclass | self addCategoriesOf: superclass ]
]

{ #category : 'categories initialization - private' }
MethodCategoriesPrompter >> addCategoriesOf: aSuperclass [

	| superclassCategories |
	
	superclassCategories := aSuperclass methodCategoriesAsSortedCollection reject: [ :category | reject includes: category].
	
	superclassCategories isEmpty ifFalse: [
		lines add: categories size.
		categories addAll: superclassCategories.
		reject addAll: superclassCategories]
]

{ #category : 'accessing' }
MethodCategoriesPrompter >> categories [

	^categories
]

{ #category : 'categories initialization - private' }
MethodCategoriesPrompter >> createCategories [

	| classCategories |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	classCategories := startClass methodCategoriesAsSortedCollection.
	
	reject := classCategories asSet.
	reject
		add: ClassOrganizer defaultMethodCategory.
	
	startClass isMeta ifTrue: [ self initializeCategoriesWhenMeta: classCategories].
	rejectingFirst ifFalse: [ categories addAll: classCategories ]
]

{ #category : 'categories initialization - private' }
MethodCategoriesPrompter >> initializeCategories [
	
	self 
		createCategories;
		addCategories
]

{ #category : 'categories initialization - private' }
MethodCategoriesPrompter >> initializeCategoriesWhenMeta: classCategories [

	categories add: Categorizer instanceCreation.
	classCategories remove: Categorizer instanceCreation ifAbsent: nil.
	reject add: Categorizer instanceCreation
]

{ #category : 'initialization' }
MethodCategoriesPrompter >> initializeStaringFrom: aClass rejectingFirst: aRejectingFirst [

	startClass := aClass.
	rejectingFirst := aRejectingFirst.
	
	self initializeCategories 
]

{ #category : 'accessing' }
MethodCategoriesPrompter >> lines [

	^lines 
]

{ #category : 'prompting' }
MethodCategoriesPrompter >> prompt: aPrompt ifNone: aNoneBlock [

	| selectedCategoryIndex categoryName |
	
	selectedCategoryIndex := self promptCategory: aPrompt.
	selectedCategoryIndex = 0 ifTrue: [^ aNoneBlock value].
	
	categoryName := selectedCategoryIndex = 1 ifTrue: [ self requestNewCategory ] ifFalse: [ categories at: selectedCategoryIndex ].
	categoryName isEmpty ifTrue: [ ^aNoneBlock value ].
	
	^categoryName
	
]

{ #category : 'prompting - private' }
MethodCategoriesPrompter >> promptCategory: aPrompt [
	
	| selectedLabelIndex |
	
	selectedLabelIndex := categories size = 1 
		ifTrue: [ 1 ]
	 	ifFalse: [ (PopUpMenu labelArray: categories lines: lines) startUpWithCaption: aPrompt ].
	
	^selectedLabelIndex
]

{ #category : 'prompting - private' }
MethodCategoriesPrompter >> requestNewCategory [
	
	^self request: 'New category name?' initialAnswer: 'category-name'
]
