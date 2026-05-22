Class {
	#name : 'ExtractToTemporaryFinderTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#instVars : [
		'classToRefactor',
		'sourceMethod',
		'finder'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
ExtractToTemporaryFinderTest >> assertFinderIsEmpty [

	self assert: finder replacements isEmpty
]

{ #category : 'assertions' }
ExtractToTemporaryFinderTest >> assertFinderThrowsException [

	self 
		should: [ finder value ] 
		raise:  Error
		withMessageText: NodesInRangeFinder nodesNotFoundErrorDescription.
		
	self assert: finder replacements isNil
]

{ #category : 'class factory' }
ExtractToTemporaryFinderTest >> classToRefactorName [
	
	^#ClassToExtractMethod
]

{ #category : 'excercising' }
ExtractToTemporaryFinderTest >> compile: sourceCode [

	classToRefactor compile: sourceCode.
	sourceMethod := (classToRefactor >> #m1) asMethodReference.
]

{ #category : 'excercising' }
ExtractToTemporaryFinderTest >> createFinderToFindInterval: anInterval [

	finder := ExtractToTemporaryReplacementsFinder 
		named: 'newVar' 
		at: anInterval asSourceCodeInterval 
		from: sourceMethod. 
]

{ #category : 'setup/teardown' }
ExtractToTemporaryFinderTest >> setUp [

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test001_CannotFindIntervalsOfCodeInUnarySelector [

	| methodsToExtractFrom |
	
	self compile: 'm1 1'.
	self createFinderToFindInterval: (4 to: 4).

	finder value.
	
	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 4) equals: methodsToExtractFrom first asSourceCodeInterval.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test002_CannotFindIntervalsOfCodeInKeywordSelector [
	
	classToRefactor compile: 'm1: first m2: second first'.
	sourceMethod := (classToRefactor >> #m1:m2:) asMethodReference.
	self createFinderToFindInterval: (5 to: 9). 

	finder value.
	
	self assertFinderIsEmpty.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test003_CannotFindPartsOfOtherPiecesOfCode [

	| methodsToExtractFrom |
	
	self compile: 'm1 1. 100'.
	self createFinderToFindInterval: (4 to: 4). 

	finder value.
	
	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 4) equals: methodsToExtractFrom first asSourceCodeInterval.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test004_CannotFindReturns [
	
	self compile: 'm1 ^ 1'.
	self createFinderToFindInterval: (4 to: 6). 

	finder value.
	
	self assertFinderIsEmpty
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test005_WhenTryingToFindCodeWithSintaxErrorsThrowsException [
	
	self compile: 'm1 1+2'.
	self createFinderToFindInterval: (4 to: 5).

	self assertFinderThrowsException.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test006_CannotFindMoreThanOneStatement [
	
	self compile: 'm1 1+2. 3+4'.
	self createFinderToFindInterval: (4 to: 11).

	finder value.
	
	self assertFinderIsEmpty.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test007_WhenTryingToFindPartOfAMessageSendThrowsException [
	
	self compile: 'm1 ^ self m2'.
	self createFinderToFindInterval: (11 to: 12).

	self assertFinderThrowsException.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test008_CannotFindLeftSideOfAssignment [

	self compile: 'm1 | a | a := 10'.
	self createFinderToFindInterval: (10 to: 10).

	finder value.
	
	self assertFinderIsEmpty.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test009_WhenTryingToFindEmptyCodeThrowsException [

	self compile: 'm1  10'.
	self createFinderToFindInterval: (3 to: 4).

	self assertFinderThrowsException.
]

{ #category : 'tests' }
ExtractToTemporaryFinderTest >> test010_CanFindAValidPieceOfCode [

	| methodsToExtractFrom |

	self compile: 'm1 10'.
	self createFinderToFindInterval: (4 to: 5).
	
	finder value.

	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 5) equals: methodsToExtractFrom first asSourceCodeInterval.
]
