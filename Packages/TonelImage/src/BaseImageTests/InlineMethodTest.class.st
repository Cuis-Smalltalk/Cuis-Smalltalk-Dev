Class {
	#name : 'InlineMethodTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
InlineMethodTest >> anotherSenderClass [

	^#AnotherSenderOfMethodToInlineClass
]

{ #category : 'class factory' }
InlineMethodTest >> implementorClass [

	^#ImplementorOfMethodToInlineClass
]

{ #category : 'refactoring helpers' }
InlineMethodTest >> messageNodeReferenceOf: anImplementor inMethod: aMethodName atIndex: aSourceCodeIndex [

	| nodeToInline methodNode sourceCodeRange nodeAndRange |
	
	methodNode := (anImplementor >> aMethodName asSymbol) methodNode.
	nodeAndRange := (methodNode
		parseNodesPathAt: aSourceCodeIndex ifAbsent: [self fail]) first.
	nodeToInline := nodeAndRange key.
	(nodeToInline isKindOf: MessageNode ) ifFalse: [self failWith: 'There is no message node at the selected
		class, method and index'].
	sourceCodeRange := nodeAndRange value.
	
	^MessageNodeReference messageNode: nodeToInline selector: methodNode selector
		class: anImplementor completeSourceRange: sourceCodeRange .
]

{ #category : 'class factory' }
InlineMethodTest >> senderClass [

	^#SenderOfMethodToInlineClass
]

{ #category : 'tests - successful' }
InlineMethodTest >> test01InlineMethodOfSameClassWithoutReturnValue [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self doSomething.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self doSomething. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test02InlineMethodOfSameClassWithParametersAndWithoutReturnValue [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1:with:' asSymbol.
	methodToInlineSource := 'm1: firstParam with: secondParam
	self use: firstParam. 
	self doSomethingWithParam: firstParam andParam: secondParam.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | self m1: 5 with: ''aString''.
	 ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self use: 5.
self doSomethingWithParam: 5 andParam: ''aString''.
	 ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test03InlineMethodOfSameClassReturningConstantLiteral [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | t := 10. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test04InlineMethodOfSameClassWithMultipleStatementsAndReturn [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self m3.
self m4: 5.
 ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self m3.
self m4: 5.
t := 10. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test05InlineMethodOfSameClassWithUnusedReturn [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self m3. ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 9. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' self m3.
10. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test06InlineMethodOfSameClassThatDeclaresTemporaryVariables [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y z | y := 10.
self m3.
t := y. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test07InlineMethodOfSameClassThatDeclaresTemporaryVariablesAlreadyDeclared [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 22. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
self m3.
t := y1. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test08InlineMethodDeclaresVariablesInClosestScope [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := [self m1]. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 22. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y | t := [| y1 z |
	y1 := 10.
self m3.
y1.]. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test09InlineMethodReturnedInFullClosureKeepsTheReturnSymbol [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := [^self m1]. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 23. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y | t := [| y1 z |
	y1 := 10.
^y1.]. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test10InlineMethodOfAnotherClass [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 expectedNewSourceCode refactoring implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
^y1.'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test11InlineClassMethodOfAnotherClass [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 expectedNewSourceCode refactoring implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 46. 
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
',  implementorClass name, ' m3.
^y1.'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - validations' }
InlineMethodTest >> test12CanNotInlineMethodAccessingPrivateVariablesNotVisibleToSender [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	implementorClass addInstVarName: 'y'.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' y := 10. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.	
	] failsWith: [InlineMethod methodAccessPrivateVariablesNotVisibleToSenderErrorMessage ]
]

{ #category : 'tests - validations' }
InlineMethodTest >> test13CanNotInlineMethodWithMultipleReturns [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. (y > 9) ifTrue: [^y]. ^20.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodHasMultipleReturnsErrorMessage]
]

{ #category : 'tests - validations' }
InlineMethodTest >> test14CanNotInlineMethodWithExplicitAndImplicitReturns [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. (y > 9) ifTrue: [^y]'. "Implicit ^self"
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodHasMultipleReturnsErrorMessage]
]

{ #category : 'tests - successful' }
InlineMethodTest >> test15InlineUsagesOnSameClase [

	| methodToInlineSource methodToInlineSelector nodeToInline1 nodeToInline2 methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | t y | t := self m1. y := self m1. ^20'.  
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 22.
	nodeToInline2 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 35.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1 with: nodeToInline2)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | t y y1 z y2 z1 | y1 := 10.
self m3.
t := y1. y2 := 10.
self m3.
y := y2. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test16InlineUsageAndRemoveMethod [

	| methodToInlineSource methodToInlineSelector nodeToInline1 methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | t y | t := self m1. ^20'.  
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 22.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | t y y1 z | y1 := 10.
self m3.
t := y1. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
	self deny: (implementorClass canUnderstand: methodToInlineSelector).
]

{ #category : 'tests - successful' }
InlineMethodTest >> test17InlineMethodWithASingleReturnLineWhenLiteralResultIsNotAssigned [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^1.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1. ^20'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' 1. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test18InlineMethodUsedInLastStatementOfClosure [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | x y | x := 1. y := 2. ^x + y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | b | b := [1 + self m1]. ^b value'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 26.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | b | b := [| x y |
	x := 1.
y := 2.
1 + (x + y).]. ^b value'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test19InlineMethodIndentation [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | x y | 
		x := 1.
		y := 2.
		^x + y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | b |
		b := [
			1 + self m1
		].
		^b value'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 32.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | b |
		b := [
			| x y |
	x := 1.
			y := 2.
			1 + (x + y).
		].
		^b value'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test20InlineMethodUsedInReturn [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^5'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^3 * self m1'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 14.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^3 * 5'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test21InlineMethodAddsParenthesesWhenNeeded [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, '^5 + 10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^3 * self m1'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 14.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^3 * (5 + 10)'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test22InlineMethodOnSeveralSenders [

	| methodToInlineSource methodToInlineSelector nodeToInline1 nodeToInline2 methodWithUsageSelector1 methodWithUsageSelector2 
		methodWithUsagesSource methodWithUsagesSource2 implementorClass senderClass expectedNewSourceCode1 expectedNewSourceCode2 refactoring  |
	 
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^1.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector1 := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector1, ' ^self m1.'.
	methodWithUsageSelector2 := 'm3' asSymbol.
	methodWithUsagesSource2 := methodWithUsageSelector2, ' ^self m1.'.
	senderClass compile: methodWithUsagesSource.
	senderClass compile: methodWithUsagesSource2.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector1 atIndex: 10.
	nodeToInline2 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector2 atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1 with: nodeToInline2)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode1 := methodWithUsageSelector1,' ^1.'.
	self assert: expectedNewSourceCode1 equals: (senderClass >> methodWithUsageSelector1) sourceCode.
	
	expectedNewSourceCode2 := methodWithUsageSelector2,' ^1.'.
	self assert: expectedNewSourceCode2 equals: (senderClass >> methodWithUsageSelector2) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test23InlineMethodFromSameClassAccessingPrivateVariables [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector  
		methodWithUsagesSource implementorAndSenderClass expectedNewSourceCode refactoring  |
	 
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	implementorAndSenderClass addInstVarName: 'y'.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' y := 1. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector, ' ^self m1.'.
	
	implementorAndSenderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' y := 1.
^y.'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode.
]

{ #category : 'tests - validations' }
InlineMethodTest >> test24CanNotInlineCascadingMessageSend [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector  
		methodWithUsagesSource implementorAndSenderClass |
	 
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm2' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^5'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm3' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector, ' self m1 ; m2.'.
	
	implementorAndSenderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15.
	
	self assertCreation: [
		InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod messageSendCanNotBeCascadeErrorMessage ]
]

{ #category : 'tests - validations' }
InlineMethodTest >> test25CanNotInlineMethodFromAnotherClassReferencingSuper [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. super m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodToInlineReferencesSuperErrorMessage ]
]

{ #category : 'tests - validations' }
InlineMethodTest >> test26CanNotInlineMethodFromAnotherClassReferencingSelfWhenReceiverIsNotAVariable [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodToInlineReferencesSelfErrorMessage]
]

{ #category : 'tests - successful' }
InlineMethodTest >> test27InlineMethodFromAnotherClassReferencingSelfWhenReceiverIsAVariable [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := ', implementorClass name, ' new . ',
	 '^ t ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 58. 
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
		
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector, ' | t y | t := ', implementorClass name, ' new . y := 10.
t m3.
^y.'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test28InlineMethodWithReturnUsedInSenderReturningImplicitSelf [

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^ 2 asString'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' self ' , methodToInlineSelector. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 9. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,'  2 asString'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode
]

{ #category : 'tests - successful' }
InlineMethodTest >> test29InlineMethodWithASingleReturnLineWhenNonLiteralResultIsNotAssigned [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^self doSomething.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1. ^20'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' self doSomething. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test30InlineMethodUsedInBrace [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^self m2: { 2. self m1. 4 }'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^self m2: { 2. 10. 4 }'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test31InlineMethodUsedInBraceAddingParentheses [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10 + 5'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^self m2: { 2. self m1 * 3. 4 }'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^self m2: { 2. (10 + 5) * 3. 4 }'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test32InlineMethodUsedReturningWithDotInMessageNode [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1 + 10. ^20.'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' 10 + 10. ^20.'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]

{ #category : 'tests - successful' }
InlineMethodTest >> test33InlineMethodReturningMessageNodeDoesNotAddParenthesesWhenNotNeeded [

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, '^5 + 10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | res | res := self m1. ^res'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | res | res := 5 + 10. ^res'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
]
