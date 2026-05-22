Class {
	#name : 'ExtractMethodFinderTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#instVars : [
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
ExtractMethodFinderTest >> assert: expected equals: actual [
	"This tests assumes the order of arguments used by Cuis before July 31, 2023,
	and also used by Squeak."

	super assert: actual equals: expected
]

{ #category : 'class names' }
ExtractMethodFinderTest >> classToRefactorName [
	
	^#ClassToExtractMethod
]

{ #category : 'setup/teardown' }
ExtractMethodFinderTest >> setUp [

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName
]

{ #category : 'class names' }
ExtractMethodFinderTest >> subclassToRefactorName [
	
	^#SubclassToExtractMethod
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test01FindsSelectionOnSourceMethod [

	| sourceMethodName finder methodsToExtractFrom sourceMethod |
	
	sourceMethodName := classToRefactor compile: 'm1 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 5) equals: methodsToExtractFrom first asSourceCodeInterval.
	self assert: sourceMethod equals: methodsToExtractFrom first methodToExtractFrom.
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test02FindsRepetitionsAfterSelection [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (8 to: 9) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test03FindsRepetitionsBeforeSelection [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (8 to: 9) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (8 to: 9) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test04DoesNotIncludeComments [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10. "10"'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test05DoesNotIncludeNotExtractableSourceCode [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 100'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test06FindsRepetitionsInOtherClassMethods [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10'.
	classToRefactor compile: 'm1b 100+10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom detect: [ :aReplacement | aReplacement isOf: sourceMethod ].
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	
	replacement := methodsToExtractFrom detect: [ :aReplacement | aReplacement isOf: (classToRefactor >> #m1b) asMethodReference ].
	self assert: (9 to: 10) equals: replacement asSourceCodeInterval.
	
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test07FindsRepetitionsInSubclassesMethods [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement subclassToRefactor |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10'.
	classToRefactor compile: 'm1b 100+10'.
	subclassToRefactor := self createClassNamed: self subclassToRefactorName subclassOf: classToRefactor.
	subclassToRefactor compile: 'm3 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 4 equals: methodsToExtractFrom size.
	replacement := (methodsToExtractFrom select: [ :aReplacement | aReplacement isOf: sourceMethod ]) first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	
	replacement := (methodsToExtractFrom select: [ :aReplacement | aReplacement isOf: sourceMethod ]) second.
	self assert: (8 to: 9) equals: replacement asSourceCodeInterval.
	
	replacement := methodsToExtractFrom detect: [ :aReplacement | aReplacement isOf: (classToRefactor >> #m1b) asMethodReference ].
	self assert: (9 to: 10) equals: replacement asSourceCodeInterval.
	
	replacement := methodsToExtractFrom detect: [ :aReplacement | aReplacement isOf: (subclassToRefactor >> #m3) asMethodReference ].
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert:(replacement isOf: (subclassToRefactor >> #m3) asMethodReference)
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test08FindsRepetitionsWithMultipleSelection [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 10 factorial. true ifTrue: [ 10 factorial. 10 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 29) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (47 to: 72) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (4 to: 29) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test09FindsRepetitionsWithCascade [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 self m1; m2. self m1; m2'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 14) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 14) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (17 to: 27) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test10FindsRepetitionsWithBrace [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 {1 . 2 }. {1 . 2 }'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 11) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 11) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (14 to: 21) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test11FindsRepetitionsWithMoreThanTwoStatementsSelection [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 15 factorial. 20 factorial. true ifTrue: [ 10 factorial. 15 factorial. 20 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 43) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (61 to: 100) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (4 to: 43) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test12FindsRepetitionsOfOneMessageSend [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 10   factorial'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 15) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 15) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (18 to: 31) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test13FindsRepetitionsOfLiteral [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10 + 5'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 5) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 5) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (8 to: 9) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test14FindsRepetitionsOfLiteralVariable [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 Array. Array new'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 8) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 8) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (11 to: 15) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test15FindsRepetitionsOfInstanceVariable [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	classToRefactor addInstVarName: 'iv1'.
	sourceMethodName := classToRefactor compile: 'm1 iv1. iv1 + 2'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 6) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 6) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (9 to: 11) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test16FindsRepetitionsOfParameter [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1: p p+1. p+1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (7 to: 9) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (7 to: 9) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (12 to: 14) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test17KeepsOriginalSelection [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 1 factorial. 1 factorial. 1 factorial. 1 factorial. 1 factorial'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (17 to: 40) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (17 to: 40) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (43 to: 66) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test18FindsMultipleLiteralNodes [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 1. 1. 1. 1. 1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 7) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 7) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (10 to: 13) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test19FindsBlockNodesWithTemporaries [

	| sourceMethodName finder methodsToExtractFrom sourceMethod |
	
	sourceMethodName := classToRefactor compile: 'm1 true and: [ | t1 | false ]'.
	classToRefactor compile: 'm2 true and: [ | t1 | false ]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (16 to: 27) asSourceCodeInterval at: sourceMethod to: (Message selector: #m3). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	self assert: ((methodsToExtractFrom collect: [ :aReplacement | aReplacement methodToExtractFrom selector ]) includesAllOf: #(m1 m2)).
	self assert: (16 to: 27) equals: methodsToExtractFrom first asSourceCodeInterval.
	self assert: (16 to: 27) equals: methodsToExtractFrom second asSourceCodeInterval.
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test20FindsTemporary [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 | t1 | t1. t1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (11 to: 12) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (11 to: 12) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (15 to: 16) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test21FindsTemporaryWhenSelectingDefinition [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 | t1 | t1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 12) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 12) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test22_1_FindsTemporaryWhenSelectingDefinitionInInnerClosure [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'boundsUsing: aBlock 
	
	^self isSingleSelection 
		ifTrue: [ aBlock value: #selected first ]
		ifFalse: [ | points |
	
	points := #selected 
		inject: OrderedCollection new
		into: [ :allPoints :aSelectedWidget | | bounds |
			bounds := aBlock value: aSelectedWidget.
			allPoints add: bounds origin; add: bounds corner.
			allPoints ].
	
	Rectangle encompassing: points ]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (107 to: 370) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2:). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (107 to: 370) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test22FindsTemporaryWhenSelectingDefinitionInInnerClosure [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 | t1 | true and: [ |t2| t1 ]. t1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (23 to: 29) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (23 to: 29) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test23WhenExtractingAllLiteralsUsesStringComparison [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	sourceMethodName := classToRefactor compile: 'm1 1+2. 2+1.'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 4) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
	finder value.
	
	methodsToExtractFrom := finder replacements.
	
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 4) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).
	
	replacement := methodsToExtractFrom second.
	self assert: (11 to: 11) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test24NotWorkingCase [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	self shouldFail: [
		sourceMethodName := classToRefactor compile: 'm1 1+2. 2+1'.
		
		sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
		finder := ExtractMethodReplacementsFinder ofCodeIn: (4 to: 4) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
		finder value.
		
		methodsToExtractFrom := finder replacements.
		
		self assert: 2 equals: methodsToExtractFrom size.
		replacement := methodsToExtractFrom first.
		self assert: (4 to: 4) equals: replacement asSourceCodeInterval.
		self assert: (replacement isOf: sourceMethod).
		
		replacement := methodsToExtractFrom second.
		self assert: (11 to: 11) equals: replacement asSourceCodeInterval.
		self assert: (replacement isOf: sourceMethod).]

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test25NotWorkingCase [

	| sourceMethodName finder methodsToExtractFrom sourceMethod replacement |
	
	self shouldFail: [
		sourceMethodName := classToRefactor compile: 'm1 1. 1. 1. 1.'.
		
		sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
		finder := ExtractMethodReplacementsFinder ofCodeIn: (7 to: 10) asSourceCodeInterval at: sourceMethod to: (Message selector: #m2). 
		finder value.
		
		methodsToExtractFrom := finder replacements.
		
		self assert: 1 equals: methodsToExtractFrom size.
		replacement := methodsToExtractFrom first.
		self assert: (7 to: 10) equals: replacement asSourceCodeInterval.
		self assert: (replacement isOf: sourceMethod).
	]

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test26FindsInsideParentesis [

	| sourceMethodName finder sourceMethod intervalToExtract newMessage methodsToExtractFrom replacement |

	sourceMethodName := classToRefactor compile: 'm1 (self m1)'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (5 to: 11) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	methodsToExtractFrom := finder replacements.
		
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (5 to: 11) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test27FindsWithMoreThanOneStatementAndFirstWithParentesis [

	| sourceMethodName finder sourceMethod intervalToExtract newMessage methodsToExtractFrom replacement |

	sourceMethodName := classToRefactor compile: 'm1 (self m1). self m1'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 21) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	methodsToExtractFrom := finder replacements.
		
	self assert: 1 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 21) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test28FindsRepeatedBlocks [

	| sourceMethodName finder sourceMethod intervalToExtract newMessage methodsToExtractFrom replacement |

	sourceMethodName := classToRefactor compile: 'm1 [self m1]. [self m1]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 12) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	methodsToExtractFrom := finder replacements.
		
	self assert: 2 equals: methodsToExtractFrom size.
	replacement := methodsToExtractFrom first.
	self assert: (4 to: 12) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).

	replacement := methodsToExtractFrom second.
	self assert: (15 to: 23) equals: replacement asSourceCodeInterval.
	self assert: (replacement isOf: sourceMethod).

]

{ #category : 'tests' }
ExtractMethodFinderTest >> test29DoesNotFailWithUnnecesaryDot [

	| sourceMethodName finder sourceMethod intervalToExtract newMessage |

	sourceMethodName := classToRefactor compile: 'm1. self m2'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (5 to: 11) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	
	self 
		shouldnt: [ finder value ] 
		raise: Error
]

{ #category : 'tests' }
ExtractMethodFinderTest >> test30DoesNotFailWhenLookingForABlockEquivalentToAMethod [

	| sourceMethodName finder sourceMethod intervalToExtract newMessage |

	sourceMethodName := classToRefactor compile: 'm1. [10 factorial]'.
	classToRefactor compile: 'm2. 10 factorial'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (5 to: 18) asSourceCodeInterval.
	newMessage := Message selector: #m3.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	
	self 
		shouldnt: [ finder value ] 
		raise: Error
]
