Class {
	#name : 'ClassDefinitionNodeAnalyzerTest',
	#superclass : 'TestCase',
	#instVars : [
		'iv1'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test01isAtSuperclassWhenIndexIsInSuperclass [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtSuperclass: 1).
	self assert: (analyzer isAtSuperclass: self class superclass name size)
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test02isNotAtSuperclassWhenIndexIsOutsideSuperclass [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtSuperclass: 0).
	self deny: (analyzer isAtSuperclass: self class superclass name size + 1).
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test03isAtClassNameWhenIndexIsInClassName [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) - 1).
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size - 1).

]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test04isNotAtClassNameWhenIndexIsOutsideClassName [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) - 2).
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size).

]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test05isAtInstanceVariablesWhenIndexIsInInstanceVariablesString [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1')).
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1') + 2).

]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test06isNotAtInstanceVariablesWhenIndexIsOutsideInstanceVariablesString [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''')).
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''') + 4).
	
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test07CanAccessSuperclass [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: analyzer superclass equals: self class superclass.
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test08isAtCategoryWhenIndexIsInCategoryString [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString)).
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size - 1).
	
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test09isNotAtCategoryWhenIndexIsOutsideCategoryString [

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) - 1).
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size).
	
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test10InstanceVariablesIsTheSecondParameter [

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer instanceVariableNamesPositionForClassDefinition ) 
			equals: 'instanceVariableNames:' ]
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> test11CategoryIsTheFifthParameter [

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer categoryPosition) 
			equals: 'category:' ]
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzerTest >> thisClassDefinitionNode [
	
	 ^self class methodNodeFor: self class definition noPattern: true 
]
