Class {
	#name : 'ClassTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Classes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Classes'
}

{ #category : 'definition tests' }
ClassTest >> testDefinitionReplacingCategoryWithReplacesTheCategoryWithTheGivenOne [

	| classDefinition newCategory newCategoryDefinition |
	
	newCategory := 'NewCategory'.
	newCategoryDefinition := 'category: ''', newCategory, ''''.
	classDefinition := self class definitionReplacingCategoryWith: newCategory.
	
	self assert: (classDefinition includesSubString: newCategoryDefinition).
	self deny: (classDefinition includesSubString: self class category)
		


]

{ #category : 'definition tests' }
ClassTest >> testDefinitionReplacingCategoryWithWorksWhenCategoryIsNil [

	| classDefinition newCategory newCategoryDefinition currentCategory |
	
	newCategory := 'NewCategory'.
	newCategoryDefinition := 'category: ''', newCategory, ''''.
	currentCategory := self class category.
	[SystemOrganization removeElement: self class name.
	classDefinition := self class definitionReplacingCategoryWith: newCategory.
	
	self assert: (classDefinition includesSubString: newCategoryDefinition).
	self deny: (classDefinition includesSubString: self class category asString) ] 
		ensure: [
			SystemOrganization classify: self class name under: currentCategory ].

		


]
