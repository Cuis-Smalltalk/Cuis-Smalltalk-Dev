Class {
	#name : 'InlineTemporaryVariableTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'newVariable'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
InlineTemporaryVariableTest >> classToRefactor [

	^#ClassWithMethodToInlineTemporaryVariable
]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test01VariableToInlineCanNotBeEmpty [

	|classToRefactor originalSource methodToRefactorName tempVarToInline |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := ' '.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName , ' |', tempVarToInline,'| ^nil.'.
	classToRefactor compile: originalSource.
	
	self assertCreation: [InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.]
	 failsWith: [InlineTemporaryVariable selectionIsNotATemporaryVariableErrorMessage].
]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test02VariableToInlineMustExistInSelector [

	|classToRefactor originalSource methodToRefactorName tempVarToInline |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | |  ^nil.'.
	classToRefactor compile: originalSource.
	
	self assertCreation: [InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.]
	 failsWith: [InlineTemporaryVariable selectionIsNotATemporaryVariableErrorMessage].
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test03RefactoringRemovesUnusedTemporaryVariableDeclaration [

	|classToRefactor originalSource methodToRefactorName refactoring tempVarToInline expectedSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName , ' |', tempVarToInline,'| ^nil.'.
	classToRefactor compile: originalSource.
	expectedSourceCode := methodToRefactorName, ' ^nil.'.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	self assert: expectedSourceCode
		equals: ((classToRefactor >> methodToRefactorName asSymbol)) sourceCode.
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test04AssignmentToVariableToInlineIsDeleted [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 2. ^nil.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^nil.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test05RefactoringScopeIsBetweenAssignments [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. t1 := 2. t1 := 3. ^nil.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (20 to: 21)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | t1 | t1 := 1. t1 := 3. ^nil.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test06InlinedVariableUsagesAreReplacedWithTheVariablesValue [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m1: 1. self m2: t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' self m2: self m1: 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test07RefactoringDoesNotAddParenthesesWhenTempIsLiteralAndReferenceIsReceiverOfUnaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. ^t1 abs'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 abs'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test08RefactoringDoesNotAddParenthesesWhenTempIsLiteralAndReferenceIsArgumentOfBinaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. ^5 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^5 * 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test09RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsArgumentOfBinaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^5 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^5 * (1 + 2)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test10RefactoringDoesNotAddParenthesesWhenTempIsBinaryAndReferenceIsReceiverOfBinaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^t1 * 5'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 + 2 * 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test11RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsReceiverOfUnaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 - 2. ^t1 abs'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(1 - 2) abs'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test12RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsPartOfMessageWhereReceiverIsAnotherReferenceToTemp [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 * 5. ^t1 + t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 * 5 + (1 * 5) + (1 * 5)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test13RefactoringDoesNotAddParenthesesWhenTempIsBinaryAndReferenceIsArgumentOfKeywordMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^self m2: t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^self m2: 1 + 2'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test14RefactoringAddsParenthesesWhenTempIsKeywordMessageAndReferenceIsReceiverOfBinaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 * 2'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) * 2'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test15RefactoringDoesNotAddParenthesesWhenTempIsKeywordMessageAndReferenceIsArgumentOfBinaryMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^2 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^2 * self m2: 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test16RefactoringAddsParenthesesWhenTempIsKeywordAndReferenceIsPartOfMessageWhereArgumentIsAnotherReferenceToTemp [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 + t1 + 2 + t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) + (self m2: 5) + 2 + (self m2: 5) + self m2: 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test17RefactoringDoesNotAddParenthesesWhenBothValueAndReferenceAlreadyHaveThem [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := (1 + 2). ^t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(1 + 2) + (1 + 2)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test18RefactoringInlinesUsageOnNextAssignment [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 t2 | t1 := 2 * 3. t2 := 5 * t1. t1 := 3 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (14 to: 15)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | t1 t2 | t2 := 5 * (2 * 3). t1 := 3 + (2 * 3)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode 
]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test19CanNotRefactorTemporaryWhenAssignedAfterBeingReferencedInBlock [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b| t1 := 2. b := [5 * t1]. t1 := 3. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (12 to: 13)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable temporaryAssignedAfterUsedInBlockErrorMessage 
			equals: error messageText ]
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test20RefactoringBlockTemporaryDoesNotChangeFollowingBlocksDeclaringTemporaryWithSameName [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (23 to: 24)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ ^2 * 2]. b2 := [ | t | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test21RefactoringBlockTemporaryDoesNotChangePreviousBlocksDeclaringTemporaryWithSameName [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (58 to: 59)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ ^10+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test22RefactoringNestedBlockTemporaryDoesNotChangeFollowingBlocksDeclaringTemporaryWithSameName [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | b3 | b3 := [ | t | t := 2. ^t * 2]. ^b3 ]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (38 to: 39)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | b3 | b3 := [ ^2 * 2]. ^b3 ]. b2 := [ | t | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test23RefactoringBlockTemporaryFromDeclarationDoesNotChangePreviousBlocksDeclaringTemporaryWithSameName [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (54 to: 55)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ ^10+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test24RefactoringOnlyRemovesDeclarationOfInlinedTemporary [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't2'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t t2 t3 | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (56 to: 57)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t t3 | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	

]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test25CanNotRefactorTemporaryFromDeclarationWhenThereAreMultipleAssignments [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b| t1 := 2. b := 5 * t1. t1 := 3. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 7)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable moreThanOneAssignmentErrorMessage 
			equals: error messageText ]
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test26RefactoringAddsParenthesesWhenTempIsKeywordMessageAndReferenceIsArgumentOfKeywordMessage [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 do: 10'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) do: 10'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test27RefactoringIsAllowedWhenVariableIsUsedInBlockButThereAreNoMoreAssignments [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b | t1 := 2. b := [5 * t1]. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (13 to: 14)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b | b := [5 * 2]. ^b value.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	

]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test28CanNotRefactorTemporaryFromDeclarationWhenVariableIsReferencedButNotAssigned [

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b | b := 5 * t1. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 7)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable assignmentNotFoundErrorMessage 
			equals: error messageText ]
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test29InlineReferenceAtEndOfMethodWithoutStatementEndingCharacter [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. ^self printOn: a'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^self printOn: 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test30InlineFromReferenceLocatedAfterAssignment [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. self printOn: a. ^a'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (36 to: 36)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' self printOn: 1. ^1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test31InlineTemporaryUsedAfterAssignmentInBlock [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b t1 | b := [ t1 := 30. ^t1]. ^t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (21 to: 22)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b t1 | b := [ ^30]. ^t1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	

]

{ #category : 'tests - successful' }
InlineTemporaryVariableTest >> test32InlineTemporaryUsedAfterBlockWhenNotReferencedByIt [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't2'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b t1 t2 | b := [ t1 := 30. 15.]. t2 := 20. ^b2 value: 10 + t2'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (40 to: 41)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b t1 | b := [ t1 := 30. 15.]. ^b2 value: 10 + 20'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

]

{ #category : 'tests - validations' }
InlineTemporaryVariableTest >> test33InlineTemporaryFromReferenceAfterAssignmentInBlock [

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. 10 > 9 ifTrue: [a := 10]. ^a.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (45 to: 45)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable temporaryIsAssignedInBlockBeforeReferenceErrorMessage 
			equals: error messageText ]
	
	

]
