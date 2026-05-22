"
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.
"
Class {
	#name : 'ExtractMethod',
	#superclass : 'Refactoring',
	#instVars : [
		'extractMethodNewMethod',
		'collectionOfReplacements'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethod class >> fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments  [

	^self 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage 
			categorizedAs: aCategory 
			withArgumentNames: renamedArguments ) 
		replacements: (Array with: (ExtractMethodReplacement 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage))
	

]

{ #category : 'instance creation' }
ExtractMethod class >> newDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements  [
	
	^self new initializeNewDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements 
]

{ #category : 'applying' }
ExtractMethod >> apply [
	
	self 
		createNewMethod;
		applyReplacements 
]

{ #category : 'private - applying' }
ExtractMethod >> applyMethodReplacements: aMethodReplacements  [
	
	| adjustment sortedReplacements |
	
	adjustment := 0.
	"This is not really necesary because the groupBy: keeps the order, but I do it just in case that is changed - Hernan"
	sortedReplacements := aMethodReplacements sorted: [ :leftReplacement :rightReplacement | leftReplacement isBefore: rightReplacement ].
	sortedReplacements do: [ :aReplacement | 
		aReplacement applyAdjusting: adjustment.
		adjustment := adjustment + aReplacement adjustmentForNextReplacement ]
]

{ #category : 'private - applying' }
ExtractMethod >> applyReplacements [
	
	| replacementsByMethod |
	
	replacementsByMethod := collectionOfReplacements groupBy: [ :aReplacement | aReplacement methodToExtractFrom ].
	replacementsByMethod valuesDo: [ :aMethodReplacements | self applyMethodReplacements: aMethodReplacements ].
	
]

{ #category : 'private - applying' }
ExtractMethod >> createNewMethod [
	
	extractMethodNewMethod apply.

]

{ #category : 'initialization' }
ExtractMethod >> initializeNewDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements  [
	
	extractMethodNewMethod := anExtractMethodNewMethod.
	collectionOfReplacements := aCollectionOfReplacements.
]
