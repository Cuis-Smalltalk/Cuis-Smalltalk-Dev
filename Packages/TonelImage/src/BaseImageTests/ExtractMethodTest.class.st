Class {
	#name : 'ExtractMethodTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
ExtractMethodTest >> assertClassHasDefined: aSelector withBody: newSourceCode [

	self
		assert: newSourceCode
		equals: (classToRefactor >> aSelector) sourceCode
]

{ #category : 'assertions' }
ExtractMethodTest >> assertClassHasDefined: aSelector withBody: newSourceCode inCategory: aCategory [

	self assertClassHasDefined: aSelector withBody: newSourceCode.
	self assert: aCategory equals: (classToRefactor >> aSelector) category
]

{ #category : 'assertions' }
ExtractMethodTest >> assertExtracting: codeToExtract from: originalCode named: newMessage defines: newCode andUpdates: updatedCode [

	| originalSelector category |
	category := #category.
	originalSelector := classToRefactor compile: originalCode classified: category.

	(ExtractMethod
		fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
		of: classToRefactor >> originalSelector
		to: newMessage
		categorizedAs: category
		withArgumentNames: Dictionary new) apply.

	self
		assertClassHasDefined: originalSelector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newCode inCategory: category
]

{ #category : 'assertions' }
ExtractMethodTest >> assertExtracting: codeToExtract from: originalCode named: newMessage defines: newMethodCodeWithRenamedArg andUpdates: updatedCode withRenamedArguments: renamedArgs [

	| category originalSelector |
	category := #category.
	originalSelector := classToRefactor compile: originalCode classified: category.
	
	(ExtractMethod
		fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
		of: classToRefactor >> originalSelector
		to: newMessage
		categorizedAs: category
		withArgumentNames: renamedArgs) apply.

	self
		assertClassHasDefined: originalSelector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCodeWithRenamedArg inCategory: category
]

{ #category : 'class factory' }
ExtractMethodTest >> classToRefactorName [

	^ #ClassToExtractMethod
]

{ #category : 'method and source code helpers' }
ExtractMethodTest >> compileMethodToExtractCodeFrom [

	classToRefactor compile: self sourceCodeOfMethodToExtractCodeFrom
]

{ #category : 'assertions' }
ExtractMethodTest >> creationWithMessage: aMessage onInterval: anIntervalToExtract ofMethod: methodToExtractCodeFrom failsWith: aRefactoringExceptionMessageEvaluationBlock errorCondition: anExceptionHandlingCondition [

	self
		should: [
			ExtractMethod  
				fromInterval: anIntervalToExtract
				of: methodToExtractCodeFrom
				to: aMessage
				categorizedAs: ClassOrganizer defaultMethodCategory 
				withArgumentNames: aMessage arguments]
		raise: anExceptionHandlingCondition
		withMessageText: aRefactoringExceptionMessageEvaluationBlock
]

{ #category : 'assertions' }
ExtractMethodTest >> creationWithSelectorNamed: aSelectorName onInterval: anIntervalToExtract ofMethod: aMethodToRefactor failsWith: aRefactoringErrorMessageEvaluationBlock errorCondition: anExceptionHandlingCondition  [

	self
		creationWithMessage: (Message selector: aSelectorName)
		onInterval: anIntervalToExtract
		ofMethod: aMethodToRefactor
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: anExceptionHandlingCondition
]

{ #category : 'refactoring helpers' }
ExtractMethodTest >> intervalOf: aPieceOfSourceCode locatedIn: sourceCode [

	| interval |
	interval _ sourceCode intervalOfSubCollection: aPieceOfSourceCode.
	^ (interval first to: interval last - 1) asSourceCodeInterval
]

{ #category : 'method and source code helpers' }
ExtractMethodTest >> methodToExtractCodeFrom [

	^ classToRefactor >> self selectorOfMethodToExtractCodeFrom
]

{ #category : 'method and source code helpers' }
ExtractMethodTest >> selectorOfMethodToExtractCodeFrom [

	^ #mExtractMethodExample:
]

{ #category : 'set up' }
ExtractMethodTest >> setUp [

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName.
	
]

{ #category : 'method and source code helpers' }
ExtractMethodTest >> sourceCodeOfMethodToExtractCodeFrom [

	^ self selectorOfMethodToExtractCodeFrom, ' anArgument

	<anAnnoyingPragma>
	| localVar |
	<anotherAnnoyingPragma>
	self m1.
	self m2: ''hey true''.
	self m3: anArgument , ''^ 21''.
	localVar _ Set with: ''hello''.
	localVar := #($x $y $z) size and: [ (9) * 3 ].
	^ localVar + ((4))'
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test01NewSelectorShouldNotBeEmpty [

	self
		tryingToExtractWithSelectorNamed: #''
		failsWith: [ NewSelectorPrecondition newSelectorCanNotBeEmptyErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test02NewSelectorShouldNotContainSeparators [

	self
		tryingToExtractWithSelectorNamed: #'my selector'
		failsWith: [ NewSelectorPrecondition newSelectorCanNotContainSeparatorsErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test03ExtractingToSelectorAlreadyDefinedInTheClassRaisesAWarning [

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractWithSelectorNamed: self selectorOfMethodToExtractCodeFrom
		raisesWarning: [
			NewSelectorPrecondition
				warningMessageFor: self selectorOfMethodToExtractCodeFrom
				isAlreadyDefinedIn: classToRefactor ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test04NewSelectorShouldNotStartWithANumber [

	self
		tryingToExtractWithSelectorNamed: #'2selector'
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test05NewUnarySelectorShouldNotStartWithASymbol [

	self
		tryingToExtractWithSelectorNamed: #-
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test06IntervalToExtractIsNotBeforeMethodSourceCodeBoundaries [

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (-1 to: 2) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod outOfBoundsSelectionErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test07IntervalToExtractIsNotAfterMethodSourceCodeBoundaries [

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (1 to: self sourceCodeOfMethodToExtractCodeFrom size + 2) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod outOfBoundsSelectionErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test08IntervalToExtractShouldNotContainAReturnExpression [

	self
		tryingToExtract: '^ localVar + ((4))'
		from: 'm1 ^ localVar + ((4))'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeIncludingAReturnStatement ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test09ExtractingALiteralCreatesANewMethodAndChangesExistingCodeToCallThatNewMethod [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '4'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test10IntervalToExtractDoesNotContainsPartOfTheOriginalSelector [

	self
		tryingToExtract: 'm1 ^ 8'
		from: 'm1 ^ 8'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test11IntervalToExtractDoesNotContainsPartOfLocalVariablesDefinition [

	self
		tryingToExtract: '| localVar |'
		from: 'm1 | localVar | ^ localVar + 3'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test12IntervalToExtractDoesNotContainsPartOfPragmasDefinition [

	self
		tryingToExtract: '<annoyingPragma>'
		from: 'm1 <annoyingPragma>'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test13IntervalToExtractShouldBeAValidSmalltalkExpression [

	self
		tryingToExtract: '+ ((4))'
		from: 'm1 ^ 3 + ((4))'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test14ItIsNotPossibleToExtractTheLeftSideOfAnAssignment [

	| firstOccurrence secondOccurrence intervalToExtract |
	
	self compileMethodToExtractCodeFrom.
	firstOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: 1.
	secondOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: firstOccurrence + 1.
	intervalToExtract := secondOccurrence toSelfPlus: 'localVar' size.
	self
		tryingToExtractOnInterval: intervalToExtract asSourceCodeInterval
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingLeftSideOfAssignment ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test15ExtractingAListOfStatementsCreatesANewMethodWithoutReturn [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'self m3.
	self m4: 5'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	', codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test16CanNotExtractPartOfALiteral [

	self
		tryingToExtract: 'true'
		from: 'm1 ^ ''hey true'''
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test17AStringContainingTheReturnCharacterCanBeExtracted [
	"this is to make sure the return is found using AST logic, not by text"

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '''^ 3'' size'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test18ExtractingAListOfStatementsIncludingDotsRemovesThemAndContinuesWithMethodExtraction [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '
	self m3.
	self m4: 5.'.
	originalCode _ 'm1
	' , codeToExtract.
	newMethodCode _ 'm2

	self m3.
	self m4: 5'.
	updatedCode _ 'm1
	
	self m2.'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test19ASelectionContainingAdditionalParenthesesAtTheBeginningAndOrEndCanBeExtracted [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '((42))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 42'.
	updatedCode _ 'm1 ^ ((self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test20ASelectionContainingALocalVariableIsParameterizedOnTheExtractedMessage [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'local + 3'.
	originalCode _ 'm1 | local | ^ ' , codeToExtract.
	newMethodCode _ 'm2: local

	^ ' , codeToExtract.
	updatedCode _ 'm1 | local | ^ self m2: local'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('local'))
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test21ASelectionContainingAdditionalParenthesesOnOneSideCanBeExtractedAndItIsNotTrimmed [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(9) * 3'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test22ItIsPossibleToExtractTheLastExpressionOfAMethod [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'localVar + ((4))'.
	originalCode _ 'm1 | localVar | ^ ' , codeToExtract.
	newMethodCode _ 'm2: localVar

	^ ' , codeToExtract.
	updatedCode _ 'm1 | localVar | ^ self m2: localVar'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('localVar'))
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test23CanNotExtractWithAnInvalidInterval [

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (10 to: 9) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod noSelectionErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test24ItIsNotPossibleToExtractAnIntervalIncludingATemporaryAssignmentExpression [

	self
		tryingToExtract: 'localVar _ 8'
		from: 'm1 | localVar | localVar _ 8'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test25NewUnarySelectorShouldNotContainInvalidCharacters [

	self
		tryingToExtractWithSelectorNamed: 'hola)' asSymbol
		failsWith: [ NewSelectorPrecondition invalidCharacterInsideNewSelectorErrorMessage ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test26ItIsPossibleToExtractAnEntireBlockIncludingALocalAssignment [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '[ |something| something _ 3 ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test27ItIsPossibleToExtractExpressionsWithOptimizedSelectors [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 ifNil: [ #boo ] ifNotNil: [ #yay ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test28ItIsNotPossibleToExtractALocalVariableInsideOfItsDeclaration [

	self
		tryingToExtract: 'localVar'
		from: 'm1 | localVar | ^ #foo'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test29ExtractingABinaryExpressionToAKeywordMessageIntroducesParenthesesToNotBreakOriginalPrecedence [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 + arg'.
	originalCode _ 'm1: arg ^ ' , codeToExtract , ' * 3'.
	newMethodCode _ 'm2: arg

	^ ' , codeToExtract.
	updatedCode _ 'm1: arg ^ (self m2: arg) * 3'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('arg'))
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test30ItIsPossibleToExtractAQuotedExpression [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '`3 + 4`'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test31ItIsPossibleToExtractExpressionsWithOptimizedSelectorsWhereTheReceiverIsNotASimpleLiteral [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 factorial ifNotNil: [ #boo ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test32ItIsPossibleToExtractATemporaryDeclarationOfABlockIfItIsNotUsedOutsideOfSelectionInterval [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ^ [ ' , codeToExtract , ' ]'.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 ^ [ self m2 ]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test33ItIsPossibleToExtractATemporaryDeclarationIfItIsNotUsedOutsideOfSelectionInterval [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test34TryingToExtractAMethodWithLessArgumentsThanNeededFails [

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1: arguments: #('localVar1'))
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test35TryingToExtractAMethodWithMoreArgumentsThanNeededFails [

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1:m2:m3: arguments: #('localVar1' 'localVar2' 'localVar3'))
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test36ItIsPossibleToExtractACascadeExpression [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(3 + 2) factorial; yourself'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test37ItIsPossibleATempDeclarationAlongWithABlock [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| temp | []'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test38ItIsPossibleToExtractCodeThatContainsMultipleParenthesisWithSpacesBetweenThem [

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(  (3 + 4))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 3 + 4'.
	updatedCode _ 'm1 ^ (  (self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test39TryingToExtractToAMethodThatExistsInASuperclassRaisesAWarning [

	self
		tryingToExtract: '42'
		from: 'm1 ^ 42'
		using: (Message selector: #yourself)
		raisesWarning: [ NewSelectorPrecondition warningMessageFor: #yourself isAlreadyDefinedIn: Object ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test40CanNotExtractTemporaryAssignmentWithDeclarationIfTheVariableIsUsedOutsideOfSelectionInterval [

	self
		tryingToExtract: '|var| var _ 42.'
		from: 'm1 |var| var _ 42. ^ var'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval ]
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test41CannotExtractPartOfMethodSignature [

	self
		tryingToExtract: 'arg'
		from: 'm1: arg ^ self'
		using: (Message selector: #m2: arguments: #('arg'))
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]
]

{ #category : 'tests - many repetitions' }
ExtractMethodTest >> test42ExtractsManyRepetitionsOnSameMethod [

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 5) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	^ 10' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. self m2' equals: (classToRefactor >> #m1) sourceCode.
	
]

{ #category : 'tests - many repetitions' }
ExtractMethodTest >> test43ExtractsRepetitionsOnMoreThanOneMethod [

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10'.
	classToRefactor compile: 'm1b 100+10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 5) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 'm1 self m2' equals: (classToRefactor >> #m1) sourceCode.
	self assert: 'm1b 100+self m2' equals: (classToRefactor >> #m1b) sourceCode.
	
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test44CanExtractManyStatementsInsideABlock [

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract _ 'self m1.
	self m2'.
	originalCode _ 'm1 true ifTrue: [' , codeToExtract, ']'.
	newMethodCode _ 'm2

	self m1.
	self m2'.
	updatedCode _ 'm1 true ifTrue: [self m2]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test45CanExtractCodeWithMoreThanOneBlockAndABlockAtTheEndInsideAnotherBlock [

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract _ 'true ifTrue: [ self m1 ].
	true ifTrue: [ self m2 ].'.
	originalCode _ 'm1 [true] whileTrue: [ ' , codeToExtract, '
	self m3]'.
	newMethodCode _ 'm4

	true ifTrue: [ self m1 ].
	true ifTrue: [ self m2 ]'.
	updatedCode _ 'm1 [true] whileTrue: [ self m4.
	self m3]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m4)
		defines: newMethodCode andUpdates: updatedCode
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test46CanModifyArgumentsNamesInExtractedMethod [

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'true ifTrue: [ arg ].'.
	originalCode := 'm1: arg
	[true] whileTrue: [ ' , codeToExtract, ']'.
	newMethodCodeWithRenamedArg := 'm3: argModified

	^ true ifTrue: [ argModified ]'.
	updatedCode := 'm1: arg
	[true] whileTrue: [ self m3: arg.]'.
	
	newMessage := Message selector: #m3: argument: 'arg'. 
	
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
		withRenamedArguments: (Dictionary with:'arg'->'argModified' ). 
	
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test47OnlyModifiedArgumentsNamesAreModifiedInExtractedMethod [

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'arg1 ifTrue: [ arg2 ].'.
	originalCode := 'm1: arg1 m2: arg2
	[true] whileTrue: [ ' , codeToExtract, ']'.
	newMethodCodeWithRenamedArg := 'm3: arg1Modified m4: arg2

	^ arg1Modified ifTrue: [ arg2 ]'.
	updatedCode := 'm1: arg1 m2: arg2
	[true] whileTrue: [ self m3: arg1 m4: arg2.]'.
	
	newMessage := Message selector: #m3:m4: arguments: #('arg1' 'arg2'). 
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
		withRenamedArguments: (Dictionary with:'arg1'->'arg1Modified'). 
	

]

{ #category : 'tests - validations' }
ExtractMethodTest >> test48NewSelectorMustBeUnaryWhenNoArguments [

	self
		tryingToExtract: '10'
		from: 'm1 10'
		using: (Message selector: #m2: arguments: #())
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]
]

{ #category : 'tests - successful' }
ExtractMethodTest >> test49CanExtractFromOptimizedMessageSends [

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'x + 1'.
	originalCode := 'm1 1 to: 2 do: [ :x | x + 1]'.
	newMethodCodeWithRenamedArg := 'm2: x

	^ x + 1'.
	updatedCode := 'm1 1 to: 2 do: [ :x | self m2: x]'.
	
	newMessage := Message selector: #m2: argument: 'x'. 
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
	

]

{ #category : 'tests - many repetitions' }
ExtractMethodTest >> test50ExtractsManyRepetitionsWithMultipleStatementsOnSameMethod [

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 10 factorial. true ifTrue: [ 10 factorial. 10 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 29) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	10 factorial. 10 factorial' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. true ifTrue: [ self m2]' equals: (classToRefactor >> #m1) sourceCode.
	
]

{ #category : 'tests - many repetitions' }
ExtractMethodTest >> test51ExtractsManyRepetitionsWithMoreThanTwoStatementsOnSameMethod [

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 15 factorial. 20 factorial. true ifTrue: [ 10 factorial. 15 factorial. 20 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 43) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	10 factorial. 15 factorial. 20 factorial' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. true ifTrue: [ self m2]' equals: (classToRefactor >> #m1) sourceCode.
	
]

{ #category : 'tests - validations' }
ExtractMethodTest >> test53NotWorkingCase_IncompleteCascadeMessage [

	self shouldFail: [
		self
			tryingToExtract: 'self m1;m2'
			from: 'm1 self m1;m2;m3'
			failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
	]
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtract: someCode from: anExistingSourceCode failsWith: aRefactoringErrorMessageEvaluationBlock [

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtract: someCode from: anExistingSourceCode using: aMessage failsWith: aRefactoringErrorMessageEvaluationBlock [

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtract: someCode from: anExistingSourceCode using: aMessage raisesWarning: aRefactoringWarningMessageEvaluationBlock [

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtractOnInterval: anIntervalToExtract failsWith: aRefactoringErrorMessageEvaluationBlock [

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: anIntervalToExtract
		ofMethod: self methodToExtractCodeFrom
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtractWithSelectorNamed: aSelectorName failsWith: aRefactoringErrorMessageEvaluationBlock [

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError
]

{ #category : 'assertions' }
ExtractMethodTest >> tryingToExtractWithSelectorNamed: aSelectorName raisesWarning: aRefactoringWarningMessageEvaluationBlock [

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning
		
]
