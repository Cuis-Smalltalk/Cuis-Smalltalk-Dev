Class {
	#name : 'AutoCompleterSelectorsCollector',
	#superclass : 'Object',
	#instVars : [
		'categoriesWithSelectors',
		'addedSelectorsFastSet',
		'prefix',
		'entriesToShow',
		'possibleInvalidSelectors',
		'otherClasses',
		'selectorsLimit',
		'selectorSelectionCondition'
	],
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'category entry' }
AutoCompleterSelectorsCollector class >> categoryEntryHeader [

	^ '-- '
]

{ #category : 'instance creation' }
AutoCompleterSelectorsCollector class >> for: aPrefix [

	^self for: aPrefix withSelectorsLimitedTo: SmalltalkCompleter entriesLimit
]

{ #category : 'instance creation' }
AutoCompleterSelectorsCollector class >> for: aPrefix withSelectorsLimitedTo: aLimit [

	^self new initializeFor: aPrefix withSelectorsLimitedTo: aLimit
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> addCategoriesAndSelectorsOf: aClass [

	| classOrganization |
	
	classOrganization := aClass organization.
	
	classOrganization categories do: [ :aCategory | 
		self hasReachSelectorsLimit ifTrue: [ ^ self ].
		self addCategory: aCategory of: classOrganization ]
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> addCategory: aCategory of: classOrganization [

	| categorySelectors selectedSelectors |

	selectedSelectors := self prefixedSelectorsOf: aCategory in: classOrganization.
	selectedSelectors isEmpty ifFalse: [ 
		categorySelectors := categoriesWithSelectors at: aCategory ifAbsentPut: [ OrderedCollection new ].
		categorySelectors addAll: selectedSelectors.
		self addedSelectors: selectedSelectors.
		self addToPossibleInvalidIfCorrespond: selectedSelectors ]
]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> addCategory: aCategory with: selectors  [

	entriesToShow 
		add: (self categoryEntryFor: aCategory); 
		addAll: selectors
	   
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addKeywordSelectorsOf: aClass  [
	
	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass ]
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addKeywordSelectorsOf: aClass upTo: aSuperclassToExclude [

	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ]
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addKeywordSelectorsOfAll: classes upTo: aSuperclass [

	self selectKeywordSelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclass ]
]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> addLeftCategories [
	
	categoriesWithSelectors keysAndValuesDo: [ :aCategory :selectors | self addCategory: aCategory with: selectors ].
	
]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> addPrioritizedCategories [

	self prioritizedCategories do: [ :aCategory | self addPrioritizedCategory: aCategory ]
]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> addPrioritizedCategory: aCategory [

	categoriesWithSelectors 
		at: aCategory 
		ifPresent: [ :selectors | 
			self addCategory: aCategory with: selectors.
			categoriesWithSelectors removeKey: aCategory ].

]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclassToExclude [

	| currentClass |
	
	currentClass := aClass.
	
	[ currentClass ~= aSuperclassToExclude and: [ currentClass notNil ] and: [ self hasReachSelectorsLimit not ] ] whileTrue: [ 
		self addCategoriesAndSelectorsOf: currentClass.
		currentClass := currentClass superclass].
	 

]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addSelectorsOf: aClass [

	self addSelectorsOf: aClass upTo: nil
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addSelectorsOf: aClass upTo: aSuperclassToExclude [

	self addSelectorsOfAll: (Array with: aClass) upTo: aSuperclassToExclude 
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addSelectorsOfAll: classes upTo: aSuperclass [

	classes do: [ :aClass | 
		otherClasses := classes copyWithout: aClass.
		self addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclass ]
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> addToPossibleInvalidIfCorrespond: selectedSelectors [

	selectedSelectors do: [ :aSelector |
		(otherClasses allSatisfy: [ :otherClass | otherClass canUnderstand: aSelector ]) ifFalse: [ possibleInvalidSelectors add: aSelector ]]
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addUnaryAndBinarySelectorsOf: aClass  [
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass ].
	
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addUnaryAndBinarySelectorsOf: aClass upTo: aSuperclassToExclude [
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ].
	
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addUnaryAndBinarySelectorsOfAll: classes upTo: aSuperclassToExclude [
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclassToExclude ].
	
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> addedSelectors: selectors [

	addedSelectorsFastSet addAll: selectors 
]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> categoryEntryFor: aCategory [
	
	^self class categoryEntryHeader, aCategory 
]

{ #category : 'entries to show' }
AutoCompleterSelectorsCollector >> entriesToShow [
	
	self hasCollectedOnlyOneSelector
		ifTrue: [ entriesToShow := categoriesWithSelectors anyOne ]
		ifFalse: [ 
			entriesToShow := OrderedCollection new.
			self 
				addPrioritizedCategories;
				addLeftCategories ].
						  			
	^entriesToShow 
	
	   
]

{ #category : 'testing' }
AutoCompleterSelectorsCollector >> hasCollectedOnlyOneSelector [
	
	^ categoriesWithSelectors size = 1 and: [ categoriesWithSelectors anyOne size = 1 ]
]

{ #category : 'testing' }
AutoCompleterSelectorsCollector >> hasReachSelectorsLimit [
	
	^addedSelectorsFastSet size >= selectorsLimit
]

{ #category : 'initialization' }
AutoCompleterSelectorsCollector >> initializeFor: aPrefix withSelectorsLimitedTo: aLimit  [

	prefix := aPrefix.
	categoriesWithSelectors := OrderedDictionary new.
	addedSelectorsFastSet := IdentitySet new.
	possibleInvalidSelectors := IdentitySet new.
	selectorsLimit := aLimit.
	selectorSelectionCondition := [ :aSelector | true ]
]

{ #category : 'collecting' }
AutoCompleterSelectorsCollector >> possibleInvalidSelectors [
	
	^possibleInvalidSelectors
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> prefixedSelectorsOf: aCategory in: aClassOrganization [
	
	^ (aClassOrganization listAtCategoryNamed: aCategory) 
		select: [ :aSelector |  
			(aSelector beginsWith: prefix) 
				and: [ (selectorSelectionCondition value: aSelector) 
				and: [ (addedSelectorsFastSet includes: aSelector) not ]]].

]

{ #category : 'entries to show - private' }
AutoCompleterSelectorsCollector >> prioritizedCategories  [
	
	^{Categorizer instanceCreation}
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> selectKeywordSelectorsWhile: aClosure  [

	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword ] while: aClosure 

]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> selectSelectorsThatSatisfy: aSelectorsSelectionCondition while: aClosure [
	
	| currentSelectorsSelectionCondition |
	
	currentSelectorsSelectionCondition := selectorSelectionCondition.
	[ selectorSelectionCondition := aSelectorsSelectionCondition.
	aClosure value ] ensure: [ selectorSelectionCondition := currentSelectorsSelectionCondition ].
]

{ #category : 'collecting - private' }
AutoCompleterSelectorsCollector >> selectUnaryAndBinarySelectorsWhile: aClosure [
	
	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword not ] while: aClosure 

]
