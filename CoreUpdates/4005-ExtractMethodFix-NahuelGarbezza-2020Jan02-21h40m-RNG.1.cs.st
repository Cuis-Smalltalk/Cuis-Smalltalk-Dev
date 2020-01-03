'From Cuis 5.0 [latest update: #4003] on 2 January 2020 at 10:01:34 pm'!
"Change Set:		4004-CuisCore-AuthorName-2020Jan02-21h40m
Date:			2 January 2020
Author:			Nahuel Garbezza

Fixes to Extract Method refactoring:

* bug: extra arguments were selected for the extracted message when extracting blocks
* change :: usage to facilitate interoperability
* rename 'ensure' by 'assert' to have consistency with other refactorings"!


!ExtractMethod class methodsFor: 'validations' stamp: 'RNG 1/2/2020 21:50:31'!
assert: aSelector canBeDefinedIn: aClass 

	ExtractMethodNewSelectorPrecondition valueFor: aSelector on: aClass! !

!ExtractMethod class methodsFor: 'validations' stamp: 'RNG 1/2/2020 21:51:30'!
assert: anIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom 

	(self isNotEmpty: anIntervalToExtract)
		ifFalse: [ self signalNoSelectedCodeError ].
	(self is: anIntervalToExtract withinBoundsOf: aMethodToExtractCodeFrom sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ].
	(self method: aMethodToExtractCodeFrom containsAValidExpressionOn: anIntervalToExtract)
		ifFalse: [ self signalSelectedCodeIsInvalidForExtractError ].! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 1/2/2020 21:55:44'!
containsValidNodes

	^ self isNotLeftSideOfAssignment
		and: [ self thereAreNoLocalVariableAssignments ]
		and: [ self thereAreNoReturnExpressions ]
		and: [ self isNotATempDeclaration ]! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 1/2/2020 21:56:11'!
isLeftSideOfAssignment

	^ initialNodeAncestors size > 1
		and: [ initialNodeAncestors second key isAssignmentNode ]
		and: [ initialNodeAncestors second key variable = initialNode key ]! !

!ExtractMethodExpressionValidation methodsFor: 'validation' stamp: 'RNG 1/2/2020 21:56:53'!
passed

	methodNode _ method methodNode.
	sourceCode _ method sourceCode.
	initialNodeAncestors  _ methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors _ methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	initialNode _ initialNodeAncestors first.
	finalNode _ finalNodeAncestors first.

	^ self intervalCoversCompleteAstNodes
		and: [ self containsValidNodes ]
		and: [ self startAndEndParseNodesAreTheSame
			or: [ self startAndEndNodesShareAParentNode ]
			or: [ self intervalMatchesBeginningOfStatement and: [ self intervalMatchesEndOfStatement ] ] ]! !


!ExtractMethod methodsFor: 'private - source code' stamp: 'RNG 1/2/2020 21:58:40'!
shouldBeEnclosedWithParens

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode _ existingMethod methodNode.
	initialNodeAncestors  _ methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors _ methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon _ initialNodeAncestors intersection: finalNodeAncestors.

	initialNode _ (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode _ (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions _ initialNode isMessageNode and: [ finalNode isMessageNode ].
	
	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'RNG 1/2/2020 21:51:11'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory 

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract _ ExtractMethodIntervalTrimmer
		trim: anIntervalToExtract locatedIn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assert: trimmedIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 1/2/2020 21:53:15'!
blockNodesEnclosingIntervalToExtract

	| nodes |
	nodes _ Set new.
	methodToExtractCodeFrom methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode and: [ self intervalToExtractIsCoveredByAnyOf: sourceRanges ])
			ifTrue: [ nodes add: parseNode ] ].
	^ nodes! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 1/2/2020 21:47:50'!
definedInOuterScope: parseNode

	^ self nodesThatAddVariablesToScope
		anySatisfy: [ :node | node hasLocallyArgumentOrTemporaryNamed: parseNode name ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 1/2/2020 21:45:39'!
intervalToExtractIsCoveredByAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first < intervalToExtract first
			and: [ sourceRange last > intervalToExtract last ] ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 1/2/2020 21:53:56'!
saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize

	self saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer.
	newSelector _ ('' join: (self selectorTokensOf: userAnswer)) asSymbol.! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 1/2/2020 21:59:32'!
shouldBeParameterized: parseNode appearingIn: sourceRanges

	^ (sourceRanges anySatisfy: [ :sourceRange | intervalToExtract includes: sourceRange first ])
		and: [ parseNode isTempOrArg ]
		and: [ self definedInOuterScope: parseNode ]! !


!ExtractMethodApplier class methodsFor: 'pre-conditions' stamp: 'RNG 1/2/2020 21:51:11'!
assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract

	ExtractMethod
		assert: anIntervalToExtract
		isValidIntervalOn: aMethodToExtractCodeFrom! !

!methodRemoval: ExtractMethod class #ensure:isValidIntervalOn: stamp: 'RNG 1/2/2020 21:51:11'!
ExtractMethod class removeSelector: #ensure:isValidIntervalOn:!
!methodRemoval: ExtractMethod class #ensure:canBeDefinedIn: stamp: 'RNG 1/2/2020 21:50:31'!
ExtractMethod class removeSelector: #ensure:canBeDefinedIn:!

!ExtractMethod class reorganize!
('validations' assert:canBeDefinedIn: assert:isValidIntervalOn:)
('validations - private' is:withinBoundsOf: isNotEmpty: method:containsAValidExpressionOn:)
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage selectedCodeInvalidForExtractErrorMessage)
('exceptions' signalNoSelectedCodeError signalOutOfBoundsIntervalError signalSelectedCodeIsInvalidForExtractError)
('instance creation' fromInterval:of:to:categorizedAs:)
!


!ExtractMethodApplier class reorganize!
('instance creation' for:of:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
!

