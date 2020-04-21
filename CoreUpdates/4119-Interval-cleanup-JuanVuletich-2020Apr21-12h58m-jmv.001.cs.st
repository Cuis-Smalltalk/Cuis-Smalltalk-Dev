'From Cuis 5.0 [latest update: #4118] on 21 April 2020 at 1:03:06 pm'!

!Refactoring class methodsFor: 'services - source code' stamp: 'jmv 4/21/2020 12:53:58'!
shouldTrimToMatchExpressionOn: sourceCode atIndex: currentIndex

	| currentChar |
	currentChar := sourceCode at: currentIndex ifAbsent: [ ^ false ].
	^ currentChar isSeparator or: [ currentChar = $. ]
! !

!Refactoring class methodsFor: 'services - source code' stamp: 'jmv 4/21/2020 12:53:25'!
trim: anInterval matchingParenthesesOn: aSourceCode

	| trimmedInterval |
	trimmedInterval := anInterval.
	[
		| initialChar endingChar |
		initialChar := aSourceCode at: trimmedInterval first ifAbsent: [ nil ].
		endingChar := aSourceCode at: trimmedInterval last ifAbsent: [ nil ].
		initialChar = $( and: [ endingChar = $) ]
	 ]
		whileTrue: [ trimmedInterval _ trimmedInterval first + 1 to: trimmedInterval last - 1 ].
	^ trimmedInterval! !

!Refactoring class methodsFor: 'services - source code' stamp: 'jmv 4/21/2020 12:59:54'!
trim: anInterval toMatchExpressionOn: aSourceCode

	 | trimmedInterval |
	trimmedInterval := anInterval.
	[ self shouldTrimToMatchExpressionOn: aSourceCode atIndex: trimmedInterval first ]
		whileTrue: [ trimmedInterval := trimmedInterval first + 1 to: trimmedInterval last ].
	[ self shouldTrimToMatchExpressionOn: aSourceCode atIndex: trimmedInterval last ]
		whileTrue: [ trimmedInterval := trimmedInterval first to: trimmedInterval last - 1 ].

	^ self trim: trimmedInterval matchingParenthesesOn: aSourceCode! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'jmv 4/21/2020 12:55:38'!
trimmed: anInterval

	^ Refactoring trim: anInterval toMatchExpressionOn: sourceCode! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'jmv 4/21/2020 12:55:14'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract := Refactoring trim: anIntervalToExtract toMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assert: trimmedIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'jmv 4/21/2020 12:56:09'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor methodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := Refactoring trim: anIntervalToExtract toMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		declaringTempIn: codeNodeForNewVariable! !

!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'jmv 4/21/2020 12:55:58'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	|  trimmedSourceCodeToExtract methodNodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := Refactoring trim: anIntervalToExtract toMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	methodNodeToExtract := self tryToParse: trimmedSourceCodeToExtract on: aMethodToRefactor methodClass.
	self assertHasOneStatement: methodNodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract.
	self assert: trimmedIntervalToExtract enclosesAValidExpressionOn: aMethodToRefactor.! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'jmv 4/21/2020 12:55:47'!
anySourceRangeOf: aMethodToRefactor matches: anIntervalToExtract

	| sourceRangeCollections allSourceRanges sourceCode |
	sourceRangeCollections := aMethodToRefactor methodNode completeSourceRanges values.
	allSourceRanges := sourceRangeCollections
		inject: Set new
		into: [ :sourceRanges :sourceRangeCollection | sourceRanges addAll: sourceRangeCollection; yourself ].
	sourceCode := aMethodToRefactor sourceCode.
	^ allSourceRanges anySatisfy: [ :sourceRange |
		(Refactoring trim: sourceRange toMatchExpressionOn: sourceCode) = anIntervalToExtract ]! !


!ExtractMethodApplier class methodsFor: 'instance creation' stamp: 'jmv 4/21/2020 12:55:26'!
for: anIntervalToExtract of: aMethodToExtractCodeFrom

	| trimmedIntervalToExtract sourceCode |
	sourceCode := aMethodToExtractCodeFrom sourceCode.
	trimmedIntervalToExtract := Refactoring trim: anIntervalToExtract toMatchExpressionOn: sourceCode.

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new initializeFor: trimmedIntervalToExtract of: aMethodToExtractCodeFrom! !

!methodRemoval: Interval #trimToMatchExpressionOn: stamp: 'jmv 4/21/2020 13:02:35'!
Interval removeSelector: #trimToMatchExpressionOn:!
!methodRemoval: Interval #shouldTrimToMatchExpressionOn:atIndex: stamp: 'jmv 4/21/2020 13:02:43'!
Interval removeSelector: #shouldTrimToMatchExpressionOn:atIndex:!
!methodRemoval: Interval #trimMatchingParenthesesOn: stamp: 'jmv 4/21/2020 13:02:45'!
Interval removeSelector: #trimMatchingParenthesesOn:!
