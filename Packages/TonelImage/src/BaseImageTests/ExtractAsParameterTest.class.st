Class {
	#name : 'ExtractAsParameterTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCanExtractAsParameter: toExtract [

	self assertCanExtractFrom: toExtract size: toExtract size introducing: toExtract
]

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCanExtractFrom: sourceCode interval: interval introducing: toIntroduced  [

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor. 
	classToRefactor compile: oldSelector asString, ' ', sourceCode.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: interval + 3
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', (sourceCode first: interval first - 1), newParameter, (sourceCode last: sourceCode size - interval last) equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector asString, ' self ', newSelector asString, ' ', toIntroduced equals: senderMethod sourceCode
]

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCanExtractFrom: sourceCode size: toExtractSize introducing: toIntroduced  [

	self assertCanExtractFrom: sourceCode interval: (1 to: toExtractSize) introducing: toIntroduced 
]

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCannotExtractAsParameter: newParameterValue withErrorMessage: anErrorMessage [

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' ', newParameterValue.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 3+newParameterValue size) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: anErrorMessage 
]

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCannotExtractInvalidNodeAsParameter: newParameterValue [

	self 
		assertCannotExtractAsParameter: newParameterValue 
		withErrorMessage: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter
]

{ #category : 'assertions' }
ExtractAsParameterTest >> assertCannotExtractInvalidSelectionAsParameter: newParameterValue [

	self 
		assertCannotExtractAsParameter: newParameterValue 
		withErrorMessage: ExtractAsParameter errorMessageForInvalidSelection 
]

{ #category : 'class factory' }
ExtractAsParameterTest >> classToRefactorName [
	
	^#ClassToIntroduceParameter
]

{ #category : 'class factory' }
ExtractAsParameterTest >> classVarName [
	
	^'ClassVar1'
]

{ #category : 'class factory' }
ExtractAsParameterTest >> createDefaultClassToRefactor [

	^ self 
		createClassNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		instanceVariableNames: self instVarName 
		classVariableNames: self classVarName
]

{ #category : 'class factory' }
ExtractAsParameterTest >> instVarName [
	
	^'iv1'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test01CanExtractLiterals [

	self assertCanExtractAsParameter: '1'

]

{ #category : 'tests' }
ExtractAsParameterTest >> test02CannotExtract_self [

	self assertCannotExtractInvalidNodeAsParameter: 'self'.
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test03CannotExtract_super [

	self assertCannotExtractInvalidNodeAsParameter: 'super'.
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test04CannotExtract_thisContext [

	self assertCannotExtractInvalidNodeAsParameter: 'thisContext'.
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test05CanExtractMessageSendToLiteralWithLiterals [

	self assertCanExtractAsParameter: '1 + 1'.
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test06CanExtract_true [

	self assertCanExtractAsParameter: 'true'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test07CannotExtract_comment [

	self assertCannotExtractInvalidSelectionAsParameter: '"comment"'.
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test08CanExtractSelectionWithSeparators [

	self assertCanExtractFrom: ' 10 ' size: 4 introducing: '10'
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test09RemovesTrailingDotsFromSelection [

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '1.'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' ', toExtract, ' self printString'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (4 to: 5) 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', newParameter, '. self printString' equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector asString, ' self ', newSelector asString, ' 1' equals: senderMethod sourceCode
]

{ #category : 'tests' }
ExtractAsParameterTest >> test10CanExtractWithParenthesis [

	self assertCanExtractFrom: '(1)' size: 3 introducing: '1' 
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test11CanExtract_symbol [

	self assertCanExtractAsParameter: '#assert:'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test12CanExtract_false [

	self assertCanExtractAsParameter: 'false'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test13CanExtract_nil [

	self assertCanExtractAsParameter: 'nil'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test14CanExtactFromKeywordSelector [

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract newKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector, newKeyword) asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '1.'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector, ' p1 ', toExtract, ' self printString'.
	classToRefactor compile: senderSelector, ' self ', oldSelector, ' #x '.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (8 to: 9) 
		at: 2
		newKeyword: newKeyword
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: oldSelector, ' p1 ', newKeyword, ' ', newParameter, '  ', newParameter, '. self printString' equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector, ' self ', oldSelector, ' #x ', newKeyword, ' 1 ' equals: senderMethod sourceCode
]

{ #category : 'tests' }
ExtractAsParameterTest >> test15CanExtractMessageSendToLiteral [

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '10 factorial'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector, ' ', toExtract.
	classToRefactor compile: senderSelector, ' self ', oldSelector.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (4 to: 15) 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', newParameter equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector, ' self ', newSelector, ' ', toExtract equals: senderMethod sourceCode
]

{ #category : 'tests' }
ExtractAsParameterTest >> test16CannotExtractMessageSendWithInvalidNode [

	self assertCannotExtractInvalidNodeAsParameter: '10 + self'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test17CannotExtractMessageSendToInvalidNode [

	self assertCannotExtractInvalidNodeAsParameter: 'self + 10'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test18CanExtractManyMessageSends [

	self assertCanExtractAsParameter: '10 + 10 + 10'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test19CannotExtractManyMessageSendsWithInvalidNode [

	self assertCannotExtractInvalidNodeAsParameter: '10 + 10 + self'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test20CanExtractPartOfAValidExpression [

	self assertCanExtractFrom: '10 + 10' size: 2 introducing: '10'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test21CanExtractABlock [

	self assertCanExtractAsParameter: '[10]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test22CannotExtractBlockWithInvalidNode [

	self assertCannotExtractInvalidNodeAsParameter: '[self]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test23CannotExtractBlockWithMessageSendWithInvalidNode [

	self assertCannotExtractInvalidNodeAsParameter: '[1 + self]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test24CanExtractBlockWithManyExpressions [

	self assertCanExtractAsParameter: '[1 + 1. 2 + 2]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test25CanExtractBlockWithParameters [

	self assertCanExtractAsParameter: '[:p1 | 1 + p1]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test26CannotExtractBlockThatReferencesMethodVariable [

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' | var1 | [ 1 + var1 ]'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (13 to: 24) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 


]

{ #category : 'tests' }
ExtractAsParameterTest >> test27CanExtractBlockWithParameterAndLocalVariable [

	self assertCanExtractAsParameter: '[:p1 | | v1 | v1 := 1. v1 + p1]'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test28CanExtractBlockEvaluation [

	| toExtract |
	
	toExtract := '[:p1 | p1] value: 1'.
	self assertCanExtractFrom: toExtract size: toExtract size introducing: '(', toExtract ,')'
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test29CannotExtractInvalidInterval [

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (2 to: 1) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractMethodNewMethod noSelectionErrorMessage 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test30CannotExtractIntervalOutOfBounds [

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (2 to: 20) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractMethodNewMethod outOfBoundsSelectionErrorMessage 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test31CannotExtractIfOriginalMethodIsNotInImplementors [

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 4) 
			at: classToRefactor>>oldSelector  
			implementors: {} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForOrigialMethodMustBeInImplementorsToChange
]

{ #category : 'tests' }
ExtractAsParameterTest >> test32CannotExtractMoreThanOneExpression [

	self assertCannotExtractInvalidSelectionAsParameter: '1+1. 2+2' 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test33WhenAddingToUnaryMessageSelectorMustBeUnary [
	
	| classToRefactor oldSelector  |
	
	oldSelector := 'm1:' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' p1 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (8 to: 8) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: AddParameter selectorMustBeUnaryErrorMessage 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test34WhenAddingToKeywordMessageSelectorMustBeKeyword [
	
	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 4) 
			at: 1
			newKeyword: #m2:
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: AddParameter selectorMustBeKeywordErrorMessage 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test35CanExtractManyIntervals [
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 10'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 5). (9 to: 10) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test36CannotExtractIfAnyIntervalIsNotValid [
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + self'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { (4 to: 5). (9 to: 12) } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 
		
	
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test37CannotExtractIfSourceToExtractAreNotEqualInAllIntervals [
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 11'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { (4 to: 5). (9 to: 10) } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageNotAllExpressionsToExtractAreEqual
		
	
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test38CanExtractManyBlocksIfAreEqual [
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' [10] + [10]'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 7). (11 to: 14) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test39CanExtractManyBlocksIfSourceCodeNotEqualButSameAst [
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' [10] + [ 10 ]'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 7). (11 to: 16) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test40CannotExtractWithoutIntervals [
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 11'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: {} 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForNoExpressionToExtract
		
	
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test41DetectsAllRangesForLiteral [
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 + 10'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 5).

	self assert: { (4 to: 5). (9 to: 10) } equals: intervals asArray.
]

{ #category : 'tests' }
ExtractAsParameterTest >> test42DoesNotMixesRangesForDifferentLiterals [
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 + 100'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 5).

	self assert: { (4 to: 5) } equals: intervals asArray.
]

{ #category : 'tests' }
ExtractAsParameterTest >> test43DetectsRangesForEquivalentBlocks [
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' [10] + [ 10 ]'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 7).

	self assert: { (4 to: 7). (11 to: 16) } equals: intervals asArray.
]

{ #category : 'tests' }
ExtractAsParameterTest >> test44DetectsRangesForEquivalentMessageSends [
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 factorial + 10    factorial'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 15).

	self assert: { (4 to: 15). (19 to: 33) } equals: intervals asArray.
]

{ #category : 'tests' }
ExtractAsParameterTest >> test45DoesNotDetectEquiallySemanticBlocks [
	
	| classToRefactor selector intervals |
	
	"This is something to improve - Hernan"
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' [:p1 | 1 ] + [:p2 | 1]'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 13).

	self assert: { (4 to: 13) } equals: intervals asArray.
]

{ #category : 'tests' }
ExtractAsParameterTest >> test46CanExtractMessageSendToClassWithValidParameters [

	| toExtract |
	
	toExtract := 'Array with: 1'.
	
	self assertCanExtractFrom: toExtract size: toExtract size introducing: '(', toExtract, ')'
	
	
]

{ #category : 'tests' }
ExtractAsParameterTest >> test47CanExtractEmptyBraceArray [

	self assertCanExtractAsParameter: '{}'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test48CanExtractBraceArrayWithExtractableElements [

	self assertCanExtractAsParameter: '{1. {}. Array with: 1}'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test49CannotExtractBraceArrayWithNoExtractableElements [

	self assertCannotExtractInvalidNodeAsParameter: '{1. self m2 }'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test50CanExtractReferenceToGlobal [

	self assertCanExtractAsParameter: 'Smalltalk'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test51CannotExtractNotExistingGlobalReference [

	self assertCannotExtractInvalidNodeAsParameter: '__Smalltalk__'
]

{ #category : 'tests' }
ExtractAsParameterTest >> test52CanExtractReferenceToInstVar [

	self assertCanExtractAsParameter: self instVarName 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test53CanExtractReferenceToClassVar [

	self assertCanExtractAsParameter: self classVarName 
]

{ #category : 'tests' }
ExtractAsParameterTest >> test54CannotExtractInstVarWhenInLeftPartOfAssigment [
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor.
	classToRefactor compile: oldSelector asString, ' ', self instVarName, ':= 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { oldSelector size + 2 to: oldSelector size + 2 + self instVarName size - 1 } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 
		
	
		
]

{ #category : 'tests' }
ExtractAsParameterTest >> test55CanExtractInstVarEvenIsLeftPartOfAssigmentOfOtherNode [
	
	| classToRefactor oldSelector sourceCode |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor.
	sourceCode := oldSelector asString, ' ', self instVarName, ':= 1. ', self instVarName.
	classToRefactor compile: sourceCode.
	
	self 
		shouldnt: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { sourceCode size - self instVarName size + 1 to: sourceCode size } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		raise: RefactoringError 
		
	
		
]
