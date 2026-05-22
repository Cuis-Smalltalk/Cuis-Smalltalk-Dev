Class {
	#name : 'CompilerTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#instVars : [
		'sourceCode'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'emulating' }
CompilerTest >> correctFrom: start to: end with: aReplacement [
 
	| newSourceCode |
	
	newSourceCode := sourceCode first: start - 1.
	newSourceCode := newSourceCode, aReplacement.
	newSourceCode := newSourceCode, (sourceCode copyFrom: end + 1 to: sourceCode size).
	
	sourceCode := newSourceCode 
]

{ #category : 'private' }
CompilerTest >> createTestClass [
	
	^self createClassNamed: #CompilerTestDataClass__
]

{ #category : 'emulating' }
CompilerTest >> selectFrom: start to: end  [
	
]

{ #category : 'emulating' }
CompilerTest >> selectInvisiblyFrom: start to: stop  [

]

{ #category : 'emulating' }
CompilerTest >> selectionInterval [
	^ 1 to: 0
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsMoreThanOneUndeclaredVariableCorrectlyInBlock [
	
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ var1 := 1. goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ | goo var1 | var1 := 1. goo := 1. ^goo + var1 ]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableCorrectlyInBlockWhenNoSpaceBetweenLastOneAndPipe [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|var1| var1 := 1.goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [|var1 goo | var1 := 1.goo := 1.^ goo + var1]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableCorrectlyInBlockWhenTheresAnArgumentAlready [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableCorrectlyInBlockWhenTheresArgumentsAndTempDeclarationPipesWithNoTemps [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableCorrectlyInMethodAndInBlock [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle var1 := 1. [ goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle | var1 |', String newLineString, 'var1 := 1. [ | goo | goo := 1. ^goo + var1 ]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableCorrectlyInMethodWhenNoSpaceBetweenLastOneAndPipe [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle |var1| var1 := 1.goo := 1.^ goo + var1'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle |var1 goo | var1 := 1.goo := 1.^ goo + var1'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableInRightBlock [
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|goo| goo := 1. ^goo ]. [goo1:=1.^ goo1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [|goo| goo := 1. ^goo ]. [ | goo1 |goo1:=1.^ goo1]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testAddsUndeclaredVariableWithAnArgumentAndTempAlreadyInBlock [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | foo | foo := 2. goo := 1.^ goo + var1 + foo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | foo goo | foo := 2. goo := 1.^ goo + var1 + foo ]'.
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotDefineVarsInsideBackTick [

	self 
		should: [ self class compile: 'm1 `|v1| v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotHaveMoreThanOneExpressionInsideBackTick [

	self 
		should: [ self class compile: 'm1 `1 factorial. 1 factorial`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'backtick expected') ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotHaveReturnInsideBackTick [

	self 
		should: [ self class compile: 'm1 `^10`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotReferenceSelfInsideBackTick [

	self 
		should: [ self class compile: 'm1 `self`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'self')) ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotReferenceSuperInsideBackTick [

	self 
		should: [ self class compile: 'm1 `super`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'super')) ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotReferenceTempVarsInsideBackTick [

	self 
		should: [ self class compile: 'm1 |v1| `v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'Can not evaluate code') ]
]

{ #category : 'test - backtick' }
CompilerTest >> testCanNotReferenceThisContextInsideBackTick [

	self 
		should: [ self class compile: 'm1 `thisContext`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'thisContext')) ]
]

{ #category : 'tests - exceptions' }
CompilerTest >> testDoesNotConfusesVariablesWithSameNameOfDifferentBlocksWhenRemoving [

	| testClass counter |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]. [ | goo | ]'.
	counter := 0.
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			counter := counter + 1.
			unusedVariable resume: counter = 1 ].
		
	self assert: sourceCode equals: 'griffle [  ]. [ | goo | ]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testDoesNotRemoveUnusedVariableIfSaidSo [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: false ].
		
	self assert: sourceCode equals: 'griffle | goo |'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testKeepsVariableDefinitionWhenThereAreUsedTemporaries [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo a | a := 1. ^a'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle | a | a := 1. ^a'.
]

{ #category : 'tests - limits' }
CompilerTest >> testMaxLiterals [
	"Document the maximum number of literals in a compiled method"

	| bytecodeSetEncoder maxLiterals stringThatCanBeCompiled stringWithOneTooManyLiterals |
	"Why 6?  It's rather implementation dependent.  But the {... construct is compiled as
		(Array braceStream: size)
			nextPut: expr;
			...;
			braceArray
	 where nextPut: is a special selector.  So one each for Array binding, #braceStream, #braceArray and possibly the size,
	 one for the selector and one for the methodClass makes 5 or 6."
	bytecodeSetEncoder := CompiledMethod preferredBytecodeSetEncoderClass new.
	maxLiterals := bytecodeSetEncoder maxNumLiterals - 6.
	"If the bytecode set can push the size of the brace stream with a bytecode (not needing a literal) then we need one more literal."
	(bytecodeSetEncoder isSpecialLiteralForPush: maxLiterals) ifTrue:
		[maxLiterals := maxLiterals + 1].
	stringThatCanBeCompiled := '{ ', (String streamContents: [:strm |
					1 to: maxLiterals do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	stringWithOneTooManyLiterals := '{ ', (String streamContents: [:strm |
					1 to: maxLiterals + 1 do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	self assert: ((1 to: maxLiterals) collect: #printString) equals: (Compiler evaluate: stringThatCanBeCompiled).
	
	"If the following test fails, it means that the limit has been raised or eliminated,
	and this test should be updated to reflect the improvement."
	self should: [Compiler evaluate: stringWithOneTooManyLiterals for: nil logged: false] raise: SyntaxErrorNotification.

]

{ #category : 'tests - limits' }
CompilerTest >> testMaxLiteralsWithClassReferenceInClosure [
	"Document the maximum number of literals in a compiled method. A class
	reference in a closure reduces the maximum literals."

	| bytecodeSetEncoder maxLiterals stringThatCanBeCompiled stringWithOneTooManyLiterals |
	bytecodeSetEncoder := CompiledMethod preferredBytecodeSetEncoderClass new.
	maxLiterals := bytecodeSetEncoder maxNumLiterals
					- 2 "for selector & methodClass"
					- (bytecodeSetEncoder supportsFullBlocks ifTrue: [1] ifFalse: [5 "For this example. See below"])
					- 4 "for the implicit stream processing into which the brace construct is compiled: (Array braceStream: size) braceArray".
	"If the bytecode set can push the size of the brace stream with a bytecode (not needing a literal) then we need one more literal."
	(bytecodeSetEncoder isSpecialLiteralForPush: maxLiterals) ifTrue:
		[maxLiterals := maxLiterals + 1].
	stringThatCanBeCompiled := '[ DateAndTime now. Date today. Time ]. { ',
			(String streamContents: [:strm |
					1 to: maxLiterals do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	stringWithOneTooManyLiterals := '[ DateAndTime now. Date today. Time ]. { ',
			(String streamContents: [:strm |
					1 to: maxLiterals + 1 do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	self assert: maxLiterals equals: (Compiler evaluate: stringThatCanBeCompiled) size.
	
	"If the following test fails, it means that the limit has been raised or eliminated,
	and this test should be updated to reflect the improvement."
	self should: [Compiler evaluate: stringWithOneTooManyLiterals for: nil logged: false] raise: SyntaxErrorNotification.

]

{ #category : 'tests - exceptions' }
CompilerTest >> testRemovesUnusedVariablesFromBlocks [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle [  ]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testRemovesVariableDefinitionWhenNoMoreUnusedTemporaries [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle '.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testReusesBlockTempDeclarationWhenEmptyForUndeclaredVariable [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ || goo := 1.^ goo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ | goo | goo := 1.^ goo ]'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testReusesMethodTempDeclarationWhenEmptyForUndeclaredVariable [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle || goo := 1.^ goo'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareMethodTempAction ].
		
	self assert: sourceCode equals: 'griffle | goo | goo := 1.^ goo'.
]

{ #category : 'tests - exceptions' }
CompilerTest >> testUndeclaredVariableIsSignaledForUndeclaredVariables [

	| testClass |
	
	testClass := self createTestClass.
	
	self 
		should:  [ testClass compile: 'griffle ^ goo' notifying: self ]
		raise: UndeclaredVariable
		withExceptionDo: [ :anUndeclaredVariable |
			self assert: anUndeclaredVariable name equals: 'goo' ].
]

{ #category : 'tests - exceptions' }
CompilerTest >> testUndefinedVariableIsSignaledForVariablesDefinedButNotInitialized [

	| testClass |
	
	testClass := self createTestClass.

	self 
		should: [testClass compile: 'griffle | goo | ^ goo' notifying: self]
		raise: UndefinedVariable
		withExceptionDo: [ :anUndefinedVariable |
			self assert: anUndefinedVariable name equals: 'goo' ].
]

{ #category : 'tests - exceptions' }
CompilerTest >> testUnknownSelectorIsSignalForMessagesSendNotImplemented [

	| testClass unknowSelector |
	
	testClass := self createTestClass.
	unknowSelector := 'reallyHopeThisIsntImplementedAnywhere'.
	
	self 
		should: [ testClass
				compile: 'griffle self ', unknowSelector
				notifying: self]
		raise: UnknownSelector
		withExceptionDo: [ :anUnknowSelector |
			self assert: anUnknowSelector name equals: unknowSelector ].
]

{ #category : 'tests - exceptions' }
CompilerTest >> testUnusedVariableIsSignaledForUnusedVariables [

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	self 
		should: [testClass compile: sourceCode notifying: self]
		raise: UnusedVariable
		withExceptionDo: [ :unusedVariable |
			self assert: unusedVariable name equals: 'goo' ].
]

{ #category : 'emulating' }
CompilerTest >> text [

	^ sourceCode 
]

{ #category : 'private' }
CompilerTest >> unusedVariableSource [
	^ 'griffle 
		| goo |
		^ nil'
]
