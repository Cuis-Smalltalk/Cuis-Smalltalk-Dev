'From Cuis 6.0 [latest update: #5933] on 19 July 2023 at 7:07:08 pm'!
!classDefinition: #QQ category: 'qq'!
Object subclass: #QQ
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'qq'!

!ExtractMethodApplier methodsFor: 'private - reordering parameters' stamp: 'HAW 7/19/2023 18:29:16'!
hasSameActualArgumentsAsTheProposedOnes

	^ self newMessageArgumentNames asSet = userArgumentNames asSet! !

!ExtractMethodApplier methodsFor: 'private - reordering parameters' stamp: 'HAW 7/19/2023 18:31:30'!
hasToReorderActualArguments

	^ renamedNewMessageArguments notEmpty and:  [ self hasSameActualArgumentsAsTheProposedOnes ]! !

!ExtractMethodApplier methodsFor: 'private - reordering parameters' stamp: 'HAW 7/19/2023 19:06:30'!
reorderActualArgumentsBasedOn: parseNodesToParameterize

	newSelectorKeywords with: userArgumentNames do: [ :keyword :userArgumentName | 
			newMessageArguments 
				at: keyword 
				put: (parseNodesToParameterize detect: [:aParseNode | aParseNode name = userArgumentName ])].
	renamedNewMessageArguments := Dictionary new! !

!ExtractMethodApplier methodsFor: 'private - reordering parameters' stamp: 'HAW 7/19/2023 18:31:36'!
reorderActualArgumentsIfNecessaryBasedOn: parseNodesToParameterize 
		
	self hasToReorderActualArguments ifTrue: [ self reorderActualArgumentsBasedOn: parseNodesToParameterize ].
	! !


!QQ methodsFor: 'as yet unclassified' stamp: 'HAW 7/19/2023 19:04:00'!
m1

	| a b |
	
	a := 0.
	b := 1.
	
	     a/b! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'HAW 7/19/2023 18:56:00'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	newMessageArguments := OrderedDictionary new.
	renamedNewMessageArguments := Dictionary new! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/19/2023 19:04:33'!
requestNewMessage

	| parseNodesToParameterize initialAnswer userAnswer  caption |
	
	parseNodesToParameterize := self parseNodesToParameterize.
	initialAnswer := self buildInitialSelectorAnswer: parseNodesToParameterize.
	caption := `'New method name:' , String newLineString, 'Reorder parameters with same names or rename them with new names'`.
	userAnswer := self request: caption initialAnswer: initialAnswer.

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ].
	
	^self buildNewMessage.
	! !

!ExtractMethodApplier methodsFor: 'private - new message' stamp: 'HAW 7/19/2023 18:04:47'!
saveUnarySelector: userAnswer

	newSelector := userAnswer asSymbol! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 18:04:30'!
initializeNewSelector

	newSelector := ('' join: newSelectorKeywords) asSymbol! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:06:45'!
saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize
	
	self 
		initializeKewordsAndArgumentsNamesFrom: userAnswer;
		assertValidUserMethodName: parseNodesToParameterize from: userAnswer;
		initializeNewMessageArguments: parseNodesToParameterize;
		initializeRenamedNewMessageArguments: parseNodesToParameterize;
		reorderActualArgumentsIfNecessaryBasedOn: parseNodesToParameterize;
		initializeNewSelector.! !

!methodRemoval: QQ #m1a:m2b: stamp: 'HAW 7/19/2023 18:28:10'!
QQ removeSelector: #m1a:m2b:!
!methodRemoval: QQ #m1:m2: stamp: 'HAW 7/19/2023 19:03:29'!
QQ removeSelector: #m1:m2:!
!methodRemoval: QQ #m11:m22: stamp: 'HAW 7/19/2023 18:27:48'!
QQ removeSelector: #m11:m22:!
!methodRemoval: QQ #b:a: stamp: 'HAW 7/19/2023 19:04:00'!
QQ removeSelector: #b:a:!
!methodRemoval: QQ #a:b: stamp: 'HAW 7/19/2023 19:02:50'!
QQ removeSelector: #a:b:!
!methodRemoval: ExtractMethodApplier #reorderActualArguments stamp: 'HAW 7/19/2023 18:27:21'!
ExtractMethodApplier removeSelector: #reorderActualArguments!
!methodRemoval: ExtractMethodApplier #reorderActualArguments: stamp: 'HAW 7/19/2023 18:30:28'!
ExtractMethodApplier removeSelector: #reorderActualArguments:!

!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacementsWith: requestNewMessage requestRefactoringParameters)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' argumentNamesOf: assertHasRequiredParameters: assertValidMethodNameFrom: assertValidUserMethodName:from: buildInitialSelectorAnswer: formatAsKeyword: formatAsMethodArgument: initializeKewordsAndArgumentsNamesFrom: initializeNewMessageArguments: initializeNewSelector initializeRenamedNewMessageArguments: keywordsAndArgumentsOf: parseNodesToParameterize reconstrucMethodName saveBinaryOrKeywordSelector:withArguments: selectorKeywordsOf: sourceCodeToExtract)
('private - reordering parameters' hasSameActualArgumentsAsTheProposedOnes hasToReorderActualArguments reorderActualArgumentsBasedOn: reorderActualArgumentsIfNecessaryBasedOn:)
!

