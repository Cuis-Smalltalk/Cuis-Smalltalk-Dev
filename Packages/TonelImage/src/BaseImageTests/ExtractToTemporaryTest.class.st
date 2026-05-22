Class {
	#name : 'ExtractToTemporaryTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'test helpers' }
ExtractToTemporaryTest >> allIntervalsOf: aPieceOfSourceCode locatedIn: sourceCode [

	| intervals subsequenceSize |
	
	intervals := OrderedCollection new.
	subsequenceSize := aPieceOfSourceCode size.

	1 to: (sourceCode size - subsequenceSize + 1) do: [ :index |
		(sourceCode findString: aPieceOfSourceCode startingAt: index) = index
			ifTrue: [ intervals add: (index toSelfPlus: subsequenceSize - 1) asSourceCodeInterval ] ].
	
	^ intervals
]

{ #category : 'assertions' }
ExtractToTemporaryTest >> assertExtracting: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring [

	| intervalToExtract methodToRefactor actualSourceCodeAfterRefactoring |
	classToRefactor compile: sourceCodeOfMethodToRefactor.
	intervalToExtract := self intervalOf: codeToExtract locatedIn: sourceCodeOfMethodToRefactor.
	methodToRefactor := classToRefactor >> #m1.

	actualSourceCodeAfterRefactoring := (ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring
]

{ #category : 'assertions' }
ExtractToTemporaryTest >> assertExtractingAll: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring [

	| intervalsToExtract |

	intervalsToExtract := self allIntervalsOf: codeToExtract locatedIn: sourceCodeOfMethodToRefactor.
	
	self 
		assertExtractingIntervals: intervalsToExtract 
		from: sourceCodeOfMethodToRefactor 
		toVariableNamed: newVariable 
		updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'assertions' }
ExtractToTemporaryTest >> assertExtractingIntervals: intervalsToExtract  [
	from: sourceCodeOfMethodToRefactor 
	toVariableNamed: newVariable 
	updatesTo: sourceCodeAfterRefactoring

	| methodToRefactor |

	classToRefactor compile: sourceCodeOfMethodToRefactor.
	methodToRefactor := classToRefactor >> #m1.

	(ExtractToTemporary named: newVariable intervals: intervalsToExtract from: methodToRefactor) apply.

	self assert: sourceCodeAfterRefactoring equals: (classToRefactor >> #m1) sourceCode
]

{ #category : 'class factory' }
ExtractToTemporaryTest >> classToRefactorName [

	^ #ClassToDoExtractTemporary
]

{ #category : 'test helpers' }
ExtractToTemporaryTest >> intervalOf: aPieceOfSourceCode locatedIn: sourceCode [

	| interval |
	interval _ sourceCode intervalOfSubCollection: aPieceOfSourceCode.
	^ (interval first to: interval last - 1) asSourceCodeInterval
]

{ #category : 'set up' }
ExtractToTemporaryTest >> setUp [

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test01NewVariableCanNotBeEmpty [

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ ''.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ 2'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test02NewVariableHasToBeValid [

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ 'a b'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test03NewVariableNameCanNotBeDefinedInMethod [

	| newVariable sourceCodeToExtract sourceCode intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 | ', newVariable, ' | ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodToRefactor methodNode ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test04FailsIfNewTemporaryIsEqualToInstanceVariableInClass [

	| newVariable sourceCode sourceCodeToExtract methodToRefactor intervalToExtract |
	newVariable _ 'new'.
	classToRefactor addInstVarName: newVariable.
	sourceCodeToExtract _ '78'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test05FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass [

	| newVariable sourceCode sourceCodeToExtract methodToRefactor superclassToRefactor intervalToExtract |
	newVariable := 'new'.
	superclassToRefactor := self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.

	"classToRefactor superclass: superclassToRefactor."
	classToRefactor instVarNamed: 'superclass' put: superclassToRefactor.
	Object flushCache.

	sourceCodeToExtract := '78'.
	sourceCode := 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test06SourceCodeToExtractCanNotIncludeReturn [

	| newVariable sourceCode sourceCodeToExtract  intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '^ 1'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeIncludingAReturnStatement ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test07SourceCodeToExtractCanNotBeEmpty [

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ ' '.
	sourceCode _ 'm1 ^ 1'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeToExtractCanNotBeEmpty]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test08SourceCodeToExtractCanNotHaveSyntaxErrors [

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor expectedSyntaxErrorMessage |
	newVariable _ 'new'.
	sourceCodeToExtract _ '1 +'.
	sourceCode _ 'm1 ^ 1 + 3'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	 
	[ Parser parse: sourceCodeToExtract class: classToRefactor noPattern: true.
		self fail. ]
		on: SyntaxErrorNotification 
		do: [ :anError | expectedSyntaxErrorMessage _ anError messageText].
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasSyntaxError: expectedSyntaxErrorMessage ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test09SourceCodeToExtractHasToBeOneStatement [

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '1+2. 3+4'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasToBeOneStatement ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test10FailsIfIntervalToExtractIsBeforeMethodSourceCodeBoundaries [

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (-1 to: 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test11FailsIfIntervalToExtractIsAfterMethodSourceCodeBoundaries [

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (sourceCode size - 1 to: sourceCode size + 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test12ItIsPossibleToExtractASingleLiteralFromAMethodWithoutAnyTempsOrArguments [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test13ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAlreadyOtherTemporary [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	| four |
	four := 4.
	^ four + 2'.
	sourceCodeAfterRefactoring := 'm1

	| four two |
	four := 4.
	two := 2.
	^ four + two'.

	self assertExtracting: '2' from: sourceCode toVariableNamed: 'two' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test14ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAnEmptyTemporariesDeclaration [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	| |
	^ 3 factorial'.
	sourceCodeAfterRefactoring := 'm1

	| f |
	f := 3 factorial.
	^ f'.

	self assertExtracting: '3 factorial' from: sourceCode toVariableNamed: 'f' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test15ItIsPossibleToExtractCodeAndGenerateAssignmentUsingAnsiAssignmentStyle [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test16ItIsPossibleToExtractASingleLiteralFromABlockWithoutAnyTempsOrArguments [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ [ self m2 ]'.
	sourceCodeAfterRefactoring := 'm1

	^ [ | new |
	new := self m2 ]'.

	self assertExtracting: 'self m2' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test17CannotExtractPartOfMethodName [

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm1'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test18CannotExtractPartOfMessageSend [

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ self m2'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm2'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test19ItIsPossibleToExtractAnIntervalWithSomeExtraSpaces [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: ' 42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test20ItIsPossibleToExtractAnEmptyBlock [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ []'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := [].
	^ new'.

	self assertExtracting: '[]' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test21CannotExtractLeftSideOfAssignment [

	| newVariable sourceCode methodToRefactor intervalToExtract |
	newVariable := 'new'.
	sourceCode := 'm1 | existing | existing := 2'.
	classToRefactor compile: sourceCode.
	intervalToExtract := (17 to: 24) asSourceCodeInterval. "second occurrence of 'existing' variable "
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test22ItIsPossibleToExtractACascadeExpression [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 3 factorial; yourself'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 3 factorial; yourself.
	^ new'.

	self assertExtracting: '3 factorial; yourself' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test23CannotUseAReservedNameAsTheNewTemporaryVariable [

	| intervalToExtract methodToRefactor newVariable sourceCode |

	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		sourceCode := 'm1 ^ 2'.
		classToRefactor compile: sourceCode.
		intervalToExtract := self intervalOf: '2' locatedIn: sourceCode.
		methodToRefactor := classToRefactor >> #m1.

		self 
			assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test24WhenExtractingAnEntireStatementItOnlyGeneratesTheVariableAssignment [

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	self run'.
	sourceCodeAfterRefactoring := 'm1

	| action |
	action := self run'.

	self assertExtracting: 'self run' from: sourceCode toVariableNamed: 'action' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test25CannotExtractArgumentNamesFromMethodSignature [

	| intervalToExtract methodToRefactor newVariable sourceCode |
	newVariable := 'new'.
	sourceCode := 'm1: arg ^ self'.
	classToRefactor compile: sourceCode.
	intervalToExtract := self intervalOf: 'arg' locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1:.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test26ExtractsCorrectlyWithParentesisInsideParentesis [

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1 ^((self m2) + (self m3)) m1.'.
	sourceCodeAfterRefactoring := 'm1 | action |
	action := (self m2) + (self m3).
	^(action) m1.'.

	self assertExtracting: '((self m2) + (self m3))' from: sourceCode toVariableNamed: 'action' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test27ExtractRightNodeWhenDuplicates [

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1
self m2.
^self m2.'.
	sourceCodeAfterRefactoring := 'm1
| a |
	a := self m2.
^self m2.'.

	self assertExtracting: 'self m2' from: sourceCode toVariableNamed: 'a' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test28ExtractRightNodeWhenDuplicatesAndSelectedLastOne [

	| sourceCode sourceCodeAfterRefactoring intervalToExtract methodToRefactor actualSourceCodeAfterRefactoring |
	
	sourceCode := 'm1
self m2.
^self m2.'.
	sourceCodeAfterRefactoring := 'm1
| a |
	self m2.
a := self m2.
	^a.'.

	classToRefactor compile: sourceCode.
	intervalToExtract := (14 to: 20) asSourceCodeInterval.
	methodToRefactor := classToRefactor >> #m1.
	actualSourceCodeAfterRefactoring := (ExtractToTemporary named: 'a' at: intervalToExtract from: methodToRefactor) apply.
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test29ExctractsCorrectlyWhenTempVariablesDeclarationDoesNotHaveSpaces [

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1
|var|
var := 10.
^var + 50.'.
	sourceCodeAfterRefactoring := 'm1
|var anotherVar|
var := 10.
anotherVar := var + 50.
	^anotherVar.'.

	self assertExtracting: 'var + 50' from: sourceCode toVariableNamed: 'anotherVar' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test30ItsPossibleToExtractMoreThanOneIntervalOfSameCode [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	10. 
	10.'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := 10.
	var. 
	var.'.
	
	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test31ItsPossibleToExtractIntervalsThatItsScopesAreNested [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10. [ 10. ] ]'.
	sourceCodeAfterRefactoring := 'm1 
	[ | var |
	var := 10.
	var. [ var. ] ]'.
	
	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test32ItsPossibleToExtractSameValuesFromDifferentsScopes [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10 ]. 
	[ 10 ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := 10.
	[ var ]. 
	[ var ]'.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test33ItsPossibleToExtractMoreThanOneIntervalOfSameCode [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10 ]. 
	[ 10 ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := [ 10 ].
	var. 
	var'.
	
	self assertExtractingAll: '[ 10 ]' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test34CannotExtractWhenIntervalsAreFromDifferentPiecesOfCode [

	| sourceCode methodToRefactor |

	sourceCode := 'm1 10. 99.'.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ 
			ExtractToTemporary 
				named: 'var' 
				intervals: { (4 to: 5) asSourceCodeInterval . (8 to: 9) asSourceCodeInterval } 
				from: methodToRefactor ]
		failsWith: [ ExtractToTemporary extractingDifferentPiecesOfCodeErrorMessage ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test35ExtractAllSemanticallyEquivalentCodePieces [

	| sourceCode sourceCodeAfterRefactoring methodToRefactor intervalsToExtract |

	sourceCode := 'm1 
	[10] . 
	[   10 ] . 
	[   10   ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := [10].
	var . 
	var . 
	var'.
	intervalsToExtract := { 
		(6 to: 9) asSourceCodeInterval . 
		(15 to: 22) asSourceCodeInterval .
		(28 to: 37) asSourceCodeInterval }.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	(ExtractToTemporary named: 'var' intervals: intervalsToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: (classToRefactor >> #m1) sourceCode
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test36CanExtractMultiplePiecesOfCodeHavingTemporaryVariables [

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 
	| foo | 
	foo := 50 . 
	foo . 
	10. 
	10. 
	10'.
	sourceCodeAfterRefactoring := 'm1 
	| foo var | 
	foo := 50 . 
	foo . 
	var := 10.
	var. 
	var. 
	var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test37CanExtractMultiplePiecesOfCodeHavingTemporaryVariablesWithDeclarationWithoutSpaces [

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 
	|foo| 
	foo := 50 . 
	foo . 
	10. 
	10. 
	10'.
	sourceCodeAfterRefactoring := 'm1 
	|foo var| 
	foo := 50 . 
	foo . 
	var := 10.
	var. 
	var. 
	var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test38CanExtractMultiplePiecesOfCodeFromABlockHavingTemporaryVariables [

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 [ | foo | foo := 50 . foo . 10. 10. 10 ]'.
	sourceCodeAfterRefactoring := 'm1 [ | foo var | foo := 50 . foo . var := 10.
	var. var. var ]'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test39CanExtractMultiplePiecesOfCodeHavingTemporaryVariablesAndALotOfSpaceBeforeFirstSentence [

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 | foo |         foo := 50 . foo . 10. 10. 10'.
	sourceCodeAfterRefactoring := 'm1 | foo var |         foo := 50 . foo . var := 10.
	var. var. var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test40CannotExtractAnyPieceOfCodeWithoutIntervals [

	| methodToRefactor |

	classToRefactor compile: 'm1 10. 10.'.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: 'var' intervals: {} from: methodToRefactor ]
		failsWith: [ ExtractToTemporary cannotExtractTemporaryWithoutIntervalsErrorMessage ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test41WhenThereIsOnlyOneIntervalUsesTheSingleExtractToTemporaryClass [

	| methodToRefactor refactor |

	classToRefactor compile: 'm1 10. 10.'.
	methodToRefactor := classToRefactor >> #m1.

	refactor := ExtractToTemporary 
		named: 'var' 
		intervals: { (4 to: 6) asSourceCodeInterval } 
		from: methodToRefactor.
	
	self assert: refactor class equals: ExtractToTemporary
]

{ #category : 'tests - validations' }
ExtractToTemporaryTest >> test42AppliesPreconditionsWhenTryingToExtractMultiplePiecesOfCode [

	classToRefactor compile: 'm1 10. 10'.
	
	self 
		assertCreation: [ ExtractToTemporary 
				named: '' 
				intervals: { (4 to: 5) asSourceCodeInterval . (8 to: 9) asSourceCodeInterval } 
				from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test43ExctractsOnlyOneIntervalOfRepeatedIntervals [

	| sourceCode expectedSourceCodeAfterRefactoring actualCodeAfterRefactoring |

	sourceCode := 'm1 10. 10.'.
	expectedSourceCodeAfterRefactoring := 'm1 | var |
	var := 10. 10.'.
	classToRefactor compile: sourceCode.

	actualCodeAfterRefactoring := (
		ExtractToTemporary 
			named: 'var' 
			intervals: { (4 to: 5) asSourceCodeInterval . (4 to: 5) asSourceCodeInterval } 
			from: classToRefactor >> #m1) apply.

	self assert: expectedSourceCodeAfterRefactoring equals: actualCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test44CanExtractMultipleBlocksOfCodeInsideAKeywordCollaboration [

	| sourceCode sourceCodeAfterRefactoring actualSourceCodeAfterRefactoring intervalsToExtract methodToRefactor |

	sourceCode := 'm1 
	
	1 isNumber
		ifTrue: [ |a| a := 10. a+1 ]
		ifFalse: [    |a|    a := 10    . a+1     ]'.
	sourceCodeAfterRefactoring := 'm1 
	
	| var |
	var := [ |a| a := 10. a+1 ].
	1 isNumber
		ifTrue: var
		ifFalse: var'.

	classToRefactor compile: sourceCode.
	intervalsToExtract := { 
		self intervalOf: '[ |a| a := 10. a+1 ]' locatedIn: sourceCode .
		self intervalOf: '[    |a|    a := 10    . a+1     ]' locatedIn: sourceCode }.
	methodToRefactor := classToRefactor >> #m1.
	
	actualSourceCodeAfterRefactoring := 
		(ExtractToTemporary named: 'var' intervals: intervalsToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test45CanExtractMultipleWhenThereAreMoreThanOneCommonScope [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 
		[ 
			[ [ [ 10 ] ] ]. 
			[ [ [ 10 ] ] ] 
		] 
	]'.
	sourceCodeAfterRefactoring := 'm1 
	[ 
		[ 
			| var |
	var := 10.
	[ [ [ var ] ] ]. 
			[ [ [ var ] ] ] 
		] 
	]'.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]

{ #category : 'tests - successful' }
ExtractToTemporaryTest >> test46ChoosesClosestNodeWhenExtractingSingleCodePieceWithMultiplePossibleScopes [

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 
		[ 
			[ [ [ 10 ] ] ].
		] 
	]'.
	sourceCodeAfterRefactoring := 'm1 
	[ 
		[ 
			[ [ [ | var |
	var := 10 ] ] ].
		] 
	]'.

	self assertExtracting: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring
]
