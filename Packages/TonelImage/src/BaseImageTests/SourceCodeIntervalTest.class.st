Class {
	#name : 'SourceCodeIntervalTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test01TryingToTrimAnAlreadyTrimmedIntervalThatRepresentsASmalltalkExpressionDoesNotChangeTheOriginalInterval [
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '3+4'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: originalInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test02StartingAndEndingSeparatorsAreTrimmed [
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '	3+4 '.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 4) asSourceCodeInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test03StartingAndEndingDotsAreTrimmed [
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '...3+4..'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (4 to: 6) asSourceCodeInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test04GroupsOfParenthesesAreTrimmed [
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '((3+4))'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (3 to: 5) asSourceCodeInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test05GroupsOfParenthesesIncludingSeparatorsAreTrimmed [
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := ' (	(3+4)
	)'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (5 to: 7) asSourceCodeInterval.
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test06AnIntervalThatDoesNotHaveCharactersToExpandRemainsTheSame [

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3+4 factorial'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: originalInterval.
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test07AnIntervalEnclosedByParenthesesCanBeExpanded [

	| sourceCode expandedInterval originalInterval |
	sourceCode := '(3+4 factorial)'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test08AnIntervalWithParenthesesOnJustOneSideCannotBeExpanded [

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3 + 4 factorial)'.
	originalInterval := (1 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: originalInterval.
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test09AnIntervalEnclosedByBackticksCanBeExpanded [

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`3 + 4 factorial`'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test10AnIntervalEnclosedMultipleBackticksAndParenthesesCanBeExpanded [

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`((3 + 4 factorial))`'.
	originalInterval := (4 to: sourceCode size - 3) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).
]

{ #category : 'tests - equality' }
SourceCodeIntervalTest >> test11AsSourceCodeIntervalMessageDoesNotCreateANewSourceCodeIntervalInstance [

	| interval |
	interval := (1 to: 10) asSourceCodeInterval.
	
	self assert: interval == interval asSourceCodeInterval
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test12AnIntervalEndingOnDotExpandsUntilStartOfNextStatementOnSameLine [

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 6) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to: 7) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test13AnIntervalEndingBeforeDotExpandsUntilStartOfNextStatementOnSameLine [

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 5) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to: 7) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test14AnIntervalEndingOnEndOfTemporariesDeclarationExpandsUntilStartOfNextStatement [

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '| a  b | 
		1 + 2. 3 + 4'.
	interval := (1 to: 8) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to:12) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test15AnIntervalEndingOnLastStatemenWithoutDottIsNotExpanded [

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 
		^3 + 4'.
	interval := (1 to: 16) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test16AnIntervalEndingOnLastStatemenWithDotIsNotExpanded [

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 
		^3 + 4.'.
	interval := (1 to: 17) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test17AnIntervalEndingBeforeStatementEndCannotBeExpanded [

	| interval sourceCode |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 3) asSourceCodeInterval.
	self should: [interval expandUntilStartOfNextStatementOn: sourceCode.]
		raise: Error 
		withMessageText: [SourceCodeInterval canNotExpandIncompleteStatementError].
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test18AnIntervalEndingAfterLastStatementCannotBeExpanded [

	| interval sourceCode |
	
	sourceCode := '| a  b | 
		1 + 2. 3 + 4'.
	interval := (1 to: 20) asSourceCodeInterval.
	self should: [interval expandUntilStartOfNextStatementOn: sourceCode.]
		raise: Error 
		withMessageText: [SourceCodeInterval canNotExpandIncompleteStatementError].
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test19AnIntervalEndingAfterSourceCodeEndIsNotExpanded [

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 1000) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test20AnIntervalEndingExpandsUntilStartOfNextStatementOnNextLine [

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2.
		3 + 4'.
	interval := (1 to: 6) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedInterval := (1 to: 9) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test21ExpandIntervalToEndOfStatement [

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2. ^2'.
	interval := (1 to: 2) asSourceCodeInterval.
	expandedInterval := interval expandUntilStatementEndOn: sourceCode.
	expectedInterval := (1 to: 6) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.
]

{ #category : 'tests - testing' }
SourceCodeIntervalTest >> test22ExpandIntervalToEndOfStatementWhenNoDotIsPresent [

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2'.
	interval := (1 to: 2) asSourceCodeInterval.
	expandedInterval := interval expandUntilStatementEndOn: sourceCode.
	expectedInterval := (1 to: 5) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test23TrimmsExternalParentesisWhenMoreParentesisAtDifferentCollaborations [
	
	| originalInterval trimmedInterval sourceCode |
	
	sourceCode := '((self m2) + (self m3))'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 22) asSourceCodeInterval.
]

{ #category : 'tests - trimming' }
SourceCodeIntervalTest >> test24TrimmsExternalParentesisWhenMoreParentesisAtDifferentCollaborationsWithOtherSeparators [
	
	| originalInterval trimmedInterval sourceCode |
	
	sourceCode := '((self m2) + (self m3)
	)'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 22) asSourceCodeInterval.
]

{ #category : 'tests - expanding' }
SourceCodeIntervalTest >> test25CanExpandMultipleParentesisWithSeparatorsInTheMiddle [

	| sourceCode expandedInterval originalInterval |
	
	sourceCode := '` ( ( 3 + 4 factorial)     )     `'.
	originalInterval := (7 to: 21) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).
]
