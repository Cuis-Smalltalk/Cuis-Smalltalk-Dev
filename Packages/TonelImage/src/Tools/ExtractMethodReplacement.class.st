"
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.
"
Class {
	#name : 'ExtractMethodReplacement',
	#superclass : 'Refactoring',
	#instVars : [
		'intervalToExtract',
		'newMessage',
		'methodToExtractFrom',
		'sendCollaboration'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ExtractMethodReplacement class >> assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract [

	ExtractMethodNewMethod assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract

]

{ #category : 'instance creation' }
ExtractMethodReplacement class >> fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage  [

	| trimmedIntervalToExtract |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		
]

{ #category : 'adjustment' }
ExtractMethodReplacement >> adjustmentForNextReplacement [
	
	^sendCollaboration size - intervalToExtract size
]

{ #category : 'applying' }
ExtractMethodReplacement >> apply [

	self sourceClass
		compile: self updatedSourceCodeOfExistingMethod
		classified: methodToExtractFrom category
]

{ #category : 'applying' }
ExtractMethodReplacement >> applyAdjusting: anAdjustment [

	intervalToExtract := (intervalToExtract + anAdjustment) asSourceCodeInterval.
	self apply 
]

{ #category : 'initialization' }
ExtractMethodReplacement >> initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage [

	intervalToExtract := anIntervalToExtract.
	methodToExtractFrom := aMethodToExtractCodeFrom.
	newMessage := 	aNewMessage.
	self initializeSendCollaboration 
]

{ #category : 'initialization' }
ExtractMethodReplacement >> initializeSendCollaboration [

	sendCollaboration := 'self ', self newMessageString.
	self shouldBeEnclosedWithParens ifTrue: [ sendCollaboration := '(' , sendCollaboration , ')' ]
		
]

{ #category : 'accessing' }
ExtractMethodReplacement >> intervalToExtract [
	
	^intervalToExtract
]

{ #category : 'testing' }
ExtractMethodReplacement >> isAt: aClass  [
	
	^methodToExtractFrom methodClass = aClass 
]

{ #category : 'testing' }
ExtractMethodReplacement >> isBefore: anExtractMethodReplacement  [
	
	^anExtractMethodReplacement startsAfter: intervalToExtract first
]

{ #category : 'testing' }
ExtractMethodReplacement >> isOf: aMethod [

	^methodToExtractFrom = aMethod 
]

{ #category : 'accessing' }
ExtractMethodReplacement >> methodToExtractFrom [
	
	^methodToExtractFrom 
]

{ #category : 'private - source code' }
ExtractMethodReplacement >> newMessageString [

	^ newMessage fullName
]

{ #category : 'private - source code' }
ExtractMethodReplacement >> shouldBeEnclosedWithParens [

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode := methodToExtractFrom notOptimizedMethodNode.
	initialNodeAncestors  := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon := initialNodeAncestors intersection: finalNodeAncestors.

	initialNode := (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode := (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions := initialNode isMessageNode and: [ finalNode isMessageNode ].

	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]
]

{ #category : 'private - source code' }
ExtractMethodReplacement >> sourceClass [

	^ methodToExtractFrom methodClass
]

{ #category : 'testing' }
ExtractMethodReplacement >> startsAfter: aPosition [

	^intervalToExtract first > aPosition 
]

{ #category : 'private - source code' }
ExtractMethodReplacement >> updatedSourceCodeOfExistingMethod [

	^ methodToExtractFrom sourceCode
		copyReplaceFrom: intervalToExtract first  
		to: intervalToExtract last 
		with: sendCollaboration
]
