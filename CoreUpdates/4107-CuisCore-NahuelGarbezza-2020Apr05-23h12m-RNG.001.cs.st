'From Cuis 5.0 [latest update: #4106] on 6 April 2020 at 12:00:48 am'!
"Change Set:		4107-CuisCore-AuthorName-2020Apr05-23h12m
Date:			5 April 2020
Author:			Nahuel Garbezza

Allow extract method refactoring to extract declaration of temp variables if they are not used outside of the selection interval. Also refactored parse nodes to parameterize detection logic outside of the applier"!

!classDefinition: #ExtractMethodParametersDetector category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodParametersDetector
	instanceVariableNames: 'intervalToExtract methodNodeToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodParametersDetector commentStamp: '<historical>' prior: 0!
I am responsible for returning the parse nodes we need to parameterize before performing an extract method refactoring.!


!CodeNode methodsFor: 'accessing' stamp: 'RNG 4/5/2020 19:45:20'!
temporariesDeclaration

	self subclassResponsibility! !

!CodeNode methodsFor: 'accessing' stamp: 'RNG 4/5/2020 19:45:28'!
temporariesDeclaration: aTemporariesDeclarationNode

	self subclassResponsibility! !

!CodeNode methodsFor: 'testing' stamp: 'RNG 4/5/2020 19:45:40'!
hasTemporaryVariables

	^ self temporariesDeclaration declaresAnyVariable! !


!MethodNode methodsFor: 'accessing' stamp: 'RNG 4/5/2020 19:45:58'!
numberOfStatements

	^ block statements size! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/5/2020 19:46:24'!
findSourceRangeOfNextStatementIn: listOfAncestors

	^ listOfAncestors
		detect: [ :assoc | assoc key isBlockNode or: [ assoc key class = LeafNode ] ]
		ifFound: [ :assoc |
			(listOfAncestors at: (listOfAncestors indexOf: assoc) - 1) value ]
		ifNone: [ listOfAncestors last value ]! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/5/2020 19:46:34'!
isNotInsideATempDeclaration

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isTemporariesDeclaration
			and: [ sourceRanges anySatisfy: [ :sourceRange |
				sourceRange first < intervalToExtract first and: [ sourceRange last > intervalToExtract last ] ] ])
			ifTrue: [ ^ false ] ].
	^ true! !


!ExtractMethodParametersDetector methodsFor: 'evaluating' stamp: 'RNG 4/5/2020 22:44:44'!
value

	| parseNodesFound |
	parseNodesFound := OrderedCollection new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ parseNodesFound add: parseNode ]
	].
	^ parseNodesFound! !

!ExtractMethodParametersDetector methodsFor: 'initialization' stamp: 'RNG 4/5/2020 22:48:06'!
initializeFor: aMethodNodeToRefactor at: anIntervalToExtract

	methodNodeToRefactor := aMethodNodeToRefactor.
	intervalToExtract := anIntervalToExtract! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:45:20'!
blockNodesEnclosingIntervalToExtract

	| nodes |
	nodes := Set new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode and: [ self intervalToExtractIsCoveredByAnyOf: sourceRanges ])
			ifTrue: [ nodes add: parseNode ] ].
	^ nodes! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:35:53'!
definedInOuterScope: parseNode

	^ self nodesThatAddVariablesToScope
		anySatisfy: [ :node | node hasLocallyArgumentOrTemporaryNamed: parseNode name ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:46:47'!
intervalToExtractIncludesAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		intervalToExtract includes: sourceRange first ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:38:04'!
intervalToExtractIsCoveredByAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first < intervalToExtract first
			and: [ sourceRange last > intervalToExtract last ] ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:47:03'!
isNotExtractedAlongWithItsDeclaration: parseNode

	^ (methodNodeToRefactor
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNodeInInterval |
			parseNodeInInterval isTemporariesDeclaration
				and: [ parseNode isVariableNode ]
				and: [ parseNodeInInterval declaresVariable: parseNode ] ]) not! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:47:08'!
nodesThatAddVariablesToScope

	^ (Set with: methodNodeToRefactor)
		addAll: self blockNodesEnclosingIntervalToExtract;
		yourself! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'RNG 4/5/2020 22:46:30'!
shouldBeParameterized: parseNode appearingIn: sourceRanges

	^ (self intervalToExtractIncludesAnyOf: sourceRanges)
		and: [ parseNode isTempOrArg ]
		and: [ self definedInOuterScope: parseNode ]
		and: [ self isNotExtractedAlongWithItsDeclaration: parseNode ]! !


!ExtractMethodParametersDetector class methodsFor: 'instance creation' stamp: 'RNG 4/5/2020 22:47:48'!
for: aMethodNodeToRefactor at: anIntervalToExtract

	^ self new initializeFor: aMethodNodeToRefactor at: anIntervalToExtract! !

!ExtractMethodParametersDetector class methodsFor: 'evaluating' stamp: 'RNG 4/5/2020 22:47:55'!
valueFor: aMethodNodeToRefactor at: anIntervalToExtract

	^ (self for: aMethodNodeToRefactor at: anIntervalToExtract) value! !


!ExtractMethod class methodsFor: 'validations' stamp: 'RNG 4/5/2020 23:42:05'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor

	| parseNodesToParameterize |
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract.
	newMessage arguments size = parseNodesToParameterize size
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !

!ExtractMethod class methodsFor: 'error messages' stamp: 'RNG 4/5/2020 22:53:43'!
wrongNumberOrArgumentsGivenErrorMessage

	^ 'The number of arguments in the given selector is not correct'! !

!ExtractMethod class methodsFor: 'exceptions' stamp: 'RNG 4/5/2020 23:02:46'!
signalExtractMethodWithWrongNumberOfArgumentsError

	self refactoringError: self wrongNumberOrArgumentsGivenErrorMessage! !


!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'RNG 4/5/2020 23:40:59'!
refactoringClass

	^ ExtractMethod! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/5/2020 19:46:51'!
containsValidNodes

	^ self isNotLeftSideOfAssignment
		and: [ self thereAreNoLocalVariableAssignmentsWithoutDeclaration ]
		and: [ self thereAreNoReturnExpressions ]
		and: [ self isNotATempDeclarationWithUsagesOutOfIntervalToExtract ]
		and: [ self isNotInsideATempDeclaration ]! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/5/2020 19:47:16'!
intervalMatchesBeginningOfStatement 

	^ (self findSourceRangeOfNextStatementIn: initialNodeAncestors) value first = intervalToExtract first! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/5/2020 19:47:25'!
intervalMatchesEndOfStatement

	^ (self findSourceRangeOfNextStatementIn: finalNodeAncestors) last = intervalToExtract last! !


!ExtractMethod methodsFor: 'private - source code' stamp: 'RNG 4/5/2020 19:47:58'!
returnCharacterIfNeeded

	| extractedMethodNode |
	extractedMethodNode _ Parser parse: extractedSourceCode class: self sourceClass noPattern: true.
	^ (extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ])
		ifTrue: [ '' ] ifFalse: [ '^ ' ]! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'RNG 4/5/2020 23:42:05'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory 

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract := ExtractMethodIntervalTrimmer
		trim: anIntervalToExtract locatedIn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assert: trimmedIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 4/5/2020 22:48:26'!
parseNodesToParameterize

	^ ExtractMethodParametersDetector
		valueFor: methodToExtractCodeFrom methodNode
		at: intervalToExtract! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'RNG 4/5/2020 23:40:39'!
validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords

	newSelectorKeywords size = parseNodesToParameterize size
		ifFalse: [ self refactoringClass signalExtractMethodWithWrongNumberOfArgumentsError ]! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'RNG 4/5/2020 23:41:07'!
createRefactoring

	^ self refactoringClass
		fromInterval: intervalToExtract
		of: methodToExtractCodeFrom
		to: self buildNewMessage
		categorizedAs: methodToExtractCodeFrom category! !

!methodRemoval: ExtractMethodApplier #shouldBeParameterized:appearingIn: stamp: 'RNG 4/5/2020 23:17:54'!
ExtractMethodApplier removeSelector: #shouldBeParameterized:appearingIn:!
!methodRemoval: ExtractMethodApplier #intervalToExtractIsCoveredByAnyOf: stamp: 'RNG 4/5/2020 23:17:54'!
ExtractMethodApplier removeSelector: #intervalToExtractIsCoveredByAnyOf:!
!methodRemoval: ExtractMethodApplier #nodesThatAddVariablesToScope stamp: 'RNG 4/5/2020 23:17:54'!
ExtractMethodApplier removeSelector: #nodesThatAddVariablesToScope!
!methodRemoval: ExtractMethodApplier #signalExtractMethodWithWrongNumberOfArgumentsError stamp: 'RNG 4/5/2020 23:34:19'!
ExtractMethodApplier removeSelector: #signalExtractMethodWithWrongNumberOfArgumentsError!
!methodRemoval: ExtractMethodApplier #blockNodesEnclosingIntervalToExtract stamp: 'RNG 4/5/2020 23:17:54'!
ExtractMethodApplier removeSelector: #blockNodesEnclosingIntervalToExtract!
!methodRemoval: ExtractMethodApplier #definedInOuterScope: stamp: 'RNG 4/5/2020 23:17:54'!
ExtractMethodApplier removeSelector: #definedInOuterScope:!
!methodRemoval: ExtractMethodExpressionValidation #isDeclaredWithinIntervalToExtract: stamp: 'RNG 4/5/2020 23:14:46'!
ExtractMethodExpressionValidation removeSelector: #isDeclaredWithinIntervalToExtract:!
!methodRemoval: ExtractMethodExpressionValidation #isNotATempDeclaration stamp: 'RNG 4/5/2020 23:14:46'!
ExtractMethodExpressionValidation removeSelector: #isNotATempDeclaration!

!ExtractMethodParametersDetector reorganize!
('evaluating' value)
('initialization' initializeFor:at:)
('private' blockNodesEnclosingIntervalToExtract definedInOuterScope: intervalToExtractIncludesAnyOf: intervalToExtractIsCoveredByAnyOf: isNotExtractedAlongWithItsDeclaration: nodesThatAddVariablesToScope shouldBeParameterized:appearingIn:)
!


!ExtractMethodParametersDetector class reorganize!
('instance creation' for:at:)
('evaluating' valueFor:at:)
!


!ExtractMethod class reorganize!
('validations' assert:canBeDefinedIn: assert:hasValidParametersForExtracting:from: assert:isValidIntervalOn:)
('validations - private' is:withinBoundsOf: isNotEmpty: method:containsAValidExpressionOn:)
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage selectedCodeInvalidForExtractErrorMessage wrongNumberOrArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalNoSelectedCodeError signalOutOfBoundsIntervalError signalSelectedCodeIsInvalidForExtractError)
('instance creation' fromInterval:of:to:categorizedAs:)
!

