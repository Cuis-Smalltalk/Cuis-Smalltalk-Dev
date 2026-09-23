'From Cuis7.9 [latest update: #8184] on 22 September 2026 at 9:01:24 pm'!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer parameterizableNodes parameterSharing '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage parameterizableReplacements parameterSharing '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodNewMethodSourceCode methodsFor: 'initialization' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withArgumentNames: renamedArguments  withParameterizableNodes: aCollectionOfParameterizableNodes sharedAs: aParameterSharing

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage copy.
	newArgumentNames := renamedArguments.
	parameterizableNodes := aCollectionOfParameterizableNodes.
	parameterSharing := aParameterSharing.
	
	extractMethodProgrammer := ExtractMethodProgrammer 
		from: intervalToExtract 
		of: existingMethod 
		to: newMessage
		withParameterizableNodes: aCollectionOfParameterizableNodes! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
parameterNameOfEachParameterizableNode

	"The nodes sharing a parameter are written with the name of the first of them"

	| distinctIndexes |

	distinctIndexes := parameterSharing asSet asSortedCollection asArray.

	^parameterSharing collect: [ :firstIndex | newMessage arguments at: (distinctIndexes indexOf: firstIndex) ]! !


!ExtractMethodNewMethodSourceCode class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes sharedAs: aParameterSharing

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes
		sharedAs: aParameterSharing! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes sharedAs: aParameterSharing

	| trimmedIntervalToExtract newSourceCodeGenerator |

	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage 
			hasValidParametersForExtracting: anIntervalToExtract 
			from: aMethodToExtractCodeFrom notOptimizedMethodNode
			with: parameterizableNodes
			sharedAs: aParameterSharing;
		assert: parameterizableNodes areValidAt: trimmedIntervalToExtract in: aMethodToExtractCodeFrom.

	newSourceCodeGenerator := (ExtractMethodNewMethodSourceCode
		fromInterval: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes
		sharedAs: aParameterSharing).

	^ self new
		initializeOf: aMethodToExtractCodeFrom
		in: aCategory
		newSourceCodeFrom: newSourceCodeGenerator! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor with: parameterizableNodes sharedAs: aParameterSharing

	| parseNodesToParameterize parameterCount |
	
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract
		with: parameterizableNodes.
	parameterCount := parseNodesToParameterize size - parameterizableNodes size + aParameterSharing asSet size.
	
	(newMessage hasRightNumberOfArguments and: [ newMessage arguments size = parameterCount ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !


!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 22/Sep/2026, 10:18:46 pm (UTC)'!
argumentsByReplacementUsing: nodesToBeParameterized

	^ parameterizableReplacements collect: [ :paramReplacement |
		paramReplacement allArgumentsToParameterizeInOrderUsing: nodesToBeParameterized ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
distinctAmong: arguments

	^ (1 to: arguments size)
		select: [ :index | (parameterSharing at: index) = index ]
		thenCollect: [ :index | arguments at: index ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
parameterSharingAmong: argumentsByReplacement

	"For each argument, the index of the first one that is the same expression as it in every piece,
	so that two places holding the same customer in every piece are one parameter, and two places
	that any piece fills differently stay two"

	| argumentCount |

	argumentCount := argumentsByReplacement first size.
	(argumentsByReplacement allSatisfy: [ :arguments | arguments size = argumentCount ])
		ifFalse: [ ^ (1 to: argumentCount) asArray ].

	^ (1 to: argumentCount) collect: [ :index |
		(1 to: index) detect: [ :earlierIndex |
			argumentsByReplacement allSatisfy: [ :arguments |
				(arguments at: earlierIndex) key equivalentTo: (arguments at: index) key ]]]! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'HAW 22/Sep/2026, 10:18:46 pm (UTC)'!
distinctParametersOf: aParameterizableReplacement using: nodesToBeParameterized

	"The arguments of the piece that are a parameter of their own, the ones repeated alike in every
	piece sharing one"

	parameterSharing := self parameterSharingAmong: (self argumentsByReplacementUsing: nodesToBeParameterized).

	^ self distinctAmong: (aParameterizableReplacement allArgumentsToParameterizeInOrderUsing: nodesToBeParameterized)! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
parameterSharing

	^ parameterSharing! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 22/Sep/2026, 10:01:48 pm (UTC)'!
initializeSourceCodeToExtract

	| parameterizableNodesToReplace parameterNames |

	sourceCodeToExtract := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last.
	
	parameterNames := self parameterNameOfEachParameterizableNode.
	parameterizableNodesToReplace := parameterizableNodes with: parameterNames collect: [ :nodeWithInterval :parameterName | 
		(nodeWithInterval value - intervalToExtract first + 1) -> parameterName ].
	
	sourceCodeToExtract := sourceCodeToExtract copyReplacing: parameterizableNodesToReplace.
	
	((sourceCodeToExtract endsWith: '.') not and: [ extractMethodProgrammer shouldReturnAnyVariable ])
		ifTrue: [ sourceCodeToExtract := sourceCodeToExtract , '.' ].! !


!ExtractMethodNewMethodSourceCode class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes

	^ self
		fromInterval: anIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes
		sharedAs: (1 to: parameterizableNodes size) asArray! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes 

	^ self
		fromInterval: anIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		categorizedAs: aCategory
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes
		sharedAs: (1 to: parameterizableNodes size) asArray! !


!ExtractMethodApplier methodsFor: 'private - parse nodes to parameterized' stamp: 'HAW 22/Sep/2026, 10:18:46 pm (UTC)'!
parseAndParameterizableNodesToParameterizeInOrder

	| paramReplacementForNaming |
	
	paramReplacementForNaming := finder parameterizableReplacements
		detect: [ :paramReplacement | paramReplacement canBeUsedForNaming ]
		ifFound: [ :paramReplacement | paramReplacement ]
		ifNone: [ finder parameterizableReplacements first ].
	
	^ (finder distinctParametersOf: paramReplacementForNaming using: self parseNodesToParameterize)
		collect: [ :argParseNode | argParseNode key ]! !

!ExtractMethodApplier methodsFor: 'refactoring - private' stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
createExtractMethodNewMethodFor: newMessage using: sourceCodeParameterizableReplacement 

	| parameterizableNodes replacementIntervalToExtract replacementMethodToExtractFrom parameterSharing |

	"The code selected to extract could be removed from the replacements with the ExtractRepeatedCodeWindow.
	That's why the interval and method used here from the replacement."

	parameterizableNodes := sourceCodeParameterizableReplacement parameterizableNodesWithInterval.
	replacementIntervalToExtract := sourceCodeParameterizableReplacement intervalToExtract.
	replacementMethodToExtractFrom := sourceCodeParameterizableReplacement methodToExtractFrom methodReference.
	
	self notParameterizableParseNodesToParameterize do: [ :node |
		| sourceRanges |
		
		sourceRanges := replacementMethodToExtractFrom methodNode 
			completeSourceRangesOf: node 
			ifAbsent: [ self shouldNotHappen ].
		
		parameterizableNodes addAll: (sourceRanges 
			select: [ :range | 
				(replacementIntervalToExtract includesAllOf: range) and: [ 
				parameterizableNodes noneSatisfy: [ :paramNode | paramNode value includesAllOf: range ] ] ]
			thenCollect: [ :range | node -> range ]) ].
	parameterSharing := finder parameterSharing , (finder parameterSharing size + 1 to: parameterizableNodes size) asArray.

	^ newMethodRefactoring := ExtractMethodNewMethod 
		fromInterval: replacementIntervalToExtract
		of: replacementMethodToExtractFrom
		to: newMessage
		categorizedAs: replacementMethodToExtractFrom category 
		withArgumentNames: renamedNewMessageArguments
		withParameterizableNodes: parameterizableNodes
		sharedAs: parameterSharing! !


!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 22/Sep/2026, 10:18:46 pm (UTC)'!
acceptParamReplacementsWithNodesToParameterized: nodesToBeParameterized to: aNewMessage 

	| argumentsByReplacement |

	argumentsByReplacement := self argumentsByReplacementUsing: nodesToBeParameterized.
	parameterSharing := self parameterSharingAmong: argumentsByReplacement.

	replacements := OrderedCollection new.
	parameterizableReplacements with: argumentsByReplacement do: [ :paramReplacement :arguments |
		| distinctArguments |

		distinctArguments := self distinctAmong: arguments.
		self assertArgumentsAreValid: distinctArguments.
		self 
			addReplacementAt: paramReplacement intervalToExtract 
			in: paramReplacement methodToExtractFrom
			with: (Message selector: aNewMessage selector arguments: (distinctArguments collect: [ :argument | argument value ]))
			paramNodes: paramReplacement parameterizableNodesSortedByInterval ]! !

!methodRemoval: ExtractMethodReplacementsFinder class #repetitionShapeOf: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodReplacementsFinder class removeSelector: #repetitionShapeOf:!
!methodRemoval: ExtractMethodReplacementsFinder #stringsOfDistinctArgumentsAmong: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodReplacementsFinder removeSelector: #stringsOfDistinctArgumentsAmong:!
!methodRemoval: ExtractMethodReplacementsFinder #assertRepeatedArgumentsAreRepeatedAlikeIn: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodReplacementsFinder removeSelector: #assertRepeatedArgumentsAreRepeatedAlikeIn:!
!methodRemoval: ExtractMethodNewMethod class #assert:hasValidParametersForExtracting:from:with: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodNewMethod class removeSelector: #assert:hasValidParametersForExtracting:from:with:!
!methodRemoval: ExtractMethodNewMethod class #countOfDistinct: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodNewMethod class removeSelector: #countOfDistinct:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initializeFrom:of:to:withArgumentNames:withParameterizableNodes: stamp: 'HAW 22/Sep/2026, 10:06:27 pm (UTC)'!
ExtractMethodNewMethodSourceCode removeSelector: #initializeFrom:of:to:withArgumentNames:withParameterizableNodes:!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer parameterizableNodes parameterSharing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodApplier reorganize!
('initialization' initializeNewMessageArguments initializeOn:for:of: initializeRenamedNewMessageArguments)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacements requestNewMessage requestNewMessageFor:usingForSaving: requestNewMessageWithAllParameterizedValues requestRefactoringParameters saveSelectorFrom:for: selectorCaption:)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueAcceptingOnlyEquivalents valueAcceptingOnlyEquivalentsHandlingExceptions valueAcceptingParameterizableCode valueAcceptingParameterizableCodeHandlingExceptions valueApplyingRefactoringWith: valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' assertHasRequiredParameters: assertValidMethodNameFrom: assertValidUserMethodName:from: buildInitialKeywordSelectorAnswer: buildInitialSelectorAnswer: calculateNewMessageActualArguments: calculateRenamedNewMessageArguments: extractKewordsAndArgumentsNamesFrom: formatAsKeyword: formatAsMethodArgument: initialSelectorKeyword initializeNewSelector keywordsAndArgumentsOf: parseNodesToParameterize reconstrucMethodName saveBinaryOrKeywordSelector:withArguments: sourceCodeToExtract textFor:withAttribute:)
('private - reordering parameters' hasSameActualArgumentsAsTheProposedOnes hasToReorderActualArguments reorderActualArgumentsBasedOn: reorderActualArgumentsIfNecessaryBasedOn:)
('private - parse nodes to parameterized' notParameterizableParseNodesToParameterize parseAndParameterizableNodesToParameterizeInOrder)
('accessing' intervalToExtract methodToExtractCodeFrom)
('refactoring - private' createExtractMethodNewMethodFor:using:)
!

!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage parameterizableReplacements parameterSharing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
