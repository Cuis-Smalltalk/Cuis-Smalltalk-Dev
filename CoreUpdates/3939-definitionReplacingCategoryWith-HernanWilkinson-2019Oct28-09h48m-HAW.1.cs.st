'From Cuis 5.0 [latest update: #3938] on 28 October 2019 at 9:56:04 am'!

!Class methodsFor: 'fileIn/Out' stamp: 'HAW 10/28/2019 09:53:14'!
definitionReplacingCategoryWith: aNewCategory

	| definition categoryDefinitionIndex currentCategoryDefinition definitionWithNewCategory |
	
	definition := self definition.
	"category can be nil, that is why I sent asString to it - Hernan"
	currentCategoryDefinition := 'category: ''', self category asString, ''''.
	categoryDefinitionIndex := definition 
		indexOfSubCollection: currentCategoryDefinition 
		startingAt: 1
		ifAbsent: [ self error: 'Definition of category not found!!' ].
	
	definitionWithNewCategory := definition first: categoryDefinitionIndex - 1.
	definitionWithNewCategory := definitionWithNewCategory, 'category: ''', aNewCategory asString, ''''.
	
	^definitionWithNewCategory		! !

