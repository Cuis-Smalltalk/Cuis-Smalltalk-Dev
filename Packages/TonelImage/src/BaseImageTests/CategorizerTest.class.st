Class {
	#name : 'CategorizerTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Classes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Classes'
}

{ #category : 'assertions' }
CategorizerTest >> assertAddsWithoutBlanks: aCategoryWithBlanks [

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer addCategory: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	
	
	
]

{ #category : 'assertions' }
CategorizerTest >> assertClassifiesWithoutBlanks: aCategoryWithBlanks [

	| categorizer |
	
	categorizer := self createCategorizer.
	
	categorizer classify: 1 under: aCategoryWithBlanks suppressIfDefault: false.
	
	self assert: (categorizer categoryOfElement: 1) equals: aCategoryWithBlanks withBlanksTrimmed 
	
]

{ #category : 'assertions' }
CategorizerTest >> assertRenamesToWithoutBlanks: aCategoryWithBlanks [

	| categorizer categoryToRename |
	
	categorizer := self createCategorizer.
	
	categoryToRename := 'someCategory'.
	categorizer classify: 1 under: categoryToRename suppressIfDefault: false.
	categorizer renameCategory: categoryToRename to: aCategoryWithBlanks.
	
	self assert: (categorizer categories includes: aCategoryWithBlanks withBlanksTrimmed).
	self deny: (categorizer categories includes: aCategoryWithBlanks).
	self deny: (categorizer categories includes: categoryToRename).
	self assert: (categorizer categoryOfElement: 1) equals: aCategoryWithBlanks withBlanksTrimmed.
]

{ #category : 'support' }
CategorizerTest >> categoryWithLeadingBlanks [

	^ ' withLeadingBlanks'
]

{ #category : 'support' }
CategorizerTest >> categoryWithTrailingBlanks [

	^ 'withTrailingBlanks '
]

{ #category : 'support' }
CategorizerTest >> createCategorizer [

	^ Categorizer empty
]

{ #category : 'tests' }
CategorizerTest >> testClassifiesWithoutLeadingBlanks [

	self assertClassifiesWithoutBlanks: self categoryWithLeadingBlanks 
]

{ #category : 'tests' }
CategorizerTest >> testClassifiesWithoutTrailingBlanks [

	self assertClassifiesWithoutBlanks: self categoryWithTrailingBlanks 
]

{ #category : 'tests' }
CategorizerTest >> testRemovesLeadingBlanksWhenAddingCategory [

	self assertAddsWithoutBlanks: self categoryWithLeadingBlanks.
	
	
]

{ #category : 'tests' }
CategorizerTest >> testRemovesTrailingBlanksWhenAddingCategory [

	self assertAddsWithoutBlanks: self categoryWithTrailingBlanks.
	
	
]

{ #category : 'tests' }
CategorizerTest >> testRenamesWithoutLeadingBlanks [

	self assertRenamesToWithoutBlanks: self categoryWithLeadingBlanks 
]

{ #category : 'tests' }
CategorizerTest >> testRenamesWithoutTrailingBlanks [

	self assertRenamesToWithoutBlanks: self categoryWithTrailingBlanks 
]
