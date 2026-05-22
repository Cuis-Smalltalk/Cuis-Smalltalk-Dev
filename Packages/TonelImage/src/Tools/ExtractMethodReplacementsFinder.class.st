Class {
	#name : 'ExtractMethodReplacementsFinder',
	#superclass : 'Object',
	#instVars : [
		'intervalToExtract',
		'sourceMethod',
		'replacements',
		'newMessage',
		'sourceCodeToExtract',
		'nodesToExtract',
		'findingStrategy'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethodReplacementsFinder class >> ofCodeIn: anIntervalToExtract at: aMethod to: aNewMessage [
	
	^self new initializeOfCodeIn: anIntervalToExtract at: aMethod to: aNewMessage 
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> addReplacementAt: foundIntervalToExtract in: aMethod [
	
	"If ther is an error creating the refactoring, then the found text is not extractable and 
	therefore should not be replaced - Hernan"
	[ replacements add: (self createReplacementAt: foundIntervalToExtract in: aMethod) ] 
		on: RefactoringError 
		do: [ :anError | ].
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> createReplacementAt: foundIntervalToExtract in: aMethod [
	
	^ExtractMethodReplacement 
		fromInterval: foundIntervalToExtract asSourceCodeInterval 
		of: aMethod asMethodReference 
		to: newMessage
		
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> defineFindingStrategy [
	
	| useStringEquality |
	
	"Sadly, finding the right ranges when extracting literal nodes is almost imposible becuase literal nodes can have many
	ranges. An alternative algorithm could be to create an 'expanded AST' without reused nodes and each node with its own
	source range. Nodes of that AST could be a polimorphic proxy of the real node, adding only the source range, something
	to try ...
	For the moment, if the selection to extract has only literal nodes, uses string comparison - Hernan"
	useStringEquality := nodesToExtract allSatisfy: [ :aNode | aNode isLiteralNode ].
	
	^useStringEquality 
		ifTrue: [ [ :aMethod | self findReplacementsUsingStringEqualityIn: aMethod ]]
		ifFalse: [ [ :aMethod | self findReplacementsUsingNodeEquivalenceIn: aMethod ]]
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> findReplacementInHierarchy [

	sourceMethod methodClass withAllSubclassesDo: [ :aClass | self findReplacementsAt: aClass]
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> findReplacementsAt: aClass [

	"I live this implementationt instead of 
	
	aClass methodsDo: findingStrategy
	
	for debugging porpouses. We can do Through with this one - Hernan"
	aClass methodsDo: [ :aMethod | findingStrategy value: aMethod ]
	
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> findReplacementsUsingNodeEquivalenceIn: aMethod [

	| rangesToExtract |
		
	rangesToExtract := sourceMethod compiledMethod = aMethod
		ifTrue: [  aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract keepingSelection: intervalToExtract ]	
		ifFalse: [ aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract ].
		
	rangesToExtract do: [ :sourceRangeToExtract | self addReplacementAt: sourceRangeToExtract in: aMethod ].
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> findReplacementsUsingStringEqualityIn: aMethod [

	| sourceCode foundIntervalToExtract sourceCodeToExtractStart theSizeToExtract |
	
	sourceCode := aMethod sourceCode.
	theSizeToExtract := intervalToExtract size - 1.
	sourceCodeToExtractStart := 1.
	
	[ sourceCodeToExtractStart := sourceCode indexOfSubCollection: sourceCodeToExtract startingAt: sourceCodeToExtractStart.
	sourceCodeToExtractStart ~= 0 ] whileTrue: [ 
		foundIntervalToExtract := sourceCodeToExtractStart to: sourceCodeToExtractStart + theSizeToExtract.
		self addReplacementAt: foundIntervalToExtract in: aMethod.
		sourceCodeToExtractStart := foundIntervalToExtract last + 1 ] 
		
	 
]

{ #category : 'testing' }
ExtractMethodReplacementsFinder >> hasOneReplacement [
	
	^replacements size = 1
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> inClassReplacements [
	
	^replacements select: [ :aReplacement | aReplacement isAt: sourceMethod methodClass ]
]

{ #category : 'initialization' }
ExtractMethodReplacementsFinder >> initializeOfCodeIn: anIntervalToExtract at: aMethod to: aNewMessage [
	
	intervalToExtract := anIntervalToExtract.
	sourceMethod := aMethod.
	newMessage := aNewMessage 
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> methodsToReplace [
	
	^replacements collect: [ :aReplacement | aReplacement methodToExtractFrom ]
]

{ #category : 'private-replacement finding' }
ExtractMethodReplacementsFinder >> nodesToExtract [

	^(NodesInRangeFinder in: sourceMethod for: intervalToExtract) value
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> originalSelectionReplacement [
	
	^ExtractMethodReplacement fromInterval: intervalToExtract of: sourceMethod to: newMessage 
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> removeReplacementAt: anIndex [

	^replacements removeAt: anIndex 
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> replacements [
	
	^replacements 
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> replacementsAt: anIndex ifAbsent: ifAbsentBlock [
 
	^replacements at: anIndex ifAbsent: ifAbsentBlock 
]

{ #category : 'source code' }
ExtractMethodReplacementsFinder >> sourceCodeToExtract [
	
	^sourceCodeToExtract
]

{ #category : 'replacements' }
ExtractMethodReplacementsFinder >> sourceMethodReplacements [
	
	^replacements select: [:aReplacement | aReplacement isOf: sourceMethod ]
]

{ #category : 'evaluating' }
ExtractMethodReplacementsFinder >> value [

	sourceCodeToExtract := sourceMethod sourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	nodesToExtract := self nodesToExtract.
	replacements := OrderedCollection new.
	findingStrategy := self defineFindingStrategy.
	
	self findReplacementInHierarchy.
]
