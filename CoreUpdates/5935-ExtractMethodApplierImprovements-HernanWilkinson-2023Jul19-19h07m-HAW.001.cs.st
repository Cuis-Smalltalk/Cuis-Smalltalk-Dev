'From Cuis 6.0 [latest update: #5933] on 19 July 2023 at 7:26:58 pm'!

!ExtractMethodApplier methodsFor: 'initialization' stamp: 'HAW 7/19/2023 19:12:28'!
initializeRenamedNewMessageArguments

	renamedNewMessageArguments := Dictionary new! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:23:03'!
calculateNewMessageActualArguments: parseNodesToParameterize

	parseNodesToParameterize 
		with: newSelectorKeywords 
		do: [ :parseNode :keyword | newMessageArguments at: keyword put: parseNode ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:23:32'!
calculateRenamedNewMessageArguments: parseNodesToParameterize

	userArgumentNames 
		with: parseNodesToParameterize 
		do: [ :userArgumentName :actualArgument |  
			userArgumentName  ~= actualArgument name
				ifTrue: [ renamedNewMessageArguments at: actualArgument name put: userArgumentName]]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:22:23'!
extractKewordsAndArgumentsNamesFrom: userAnswer

	newSelectorKeywords := OrderedCollection new.
	userArgumentNames := OrderedCollection new.
	(self keywordsAndArgumentsOf: userAnswer) pairsDo: [ :keyword :argument | 
		newSelectorKeywords add: keyword, ':'.
		userArgumentNames add: argument ]! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'HAW 7/19/2023 19:11:59'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	newMessageArguments := OrderedDictionary new.
	self initializeRenamedNewMessageArguments! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:10:58'!
reconstrucMethodName

	^ String streamContents: [ :stream |
		newSelectorKeywords 
			with: userArgumentNames 
			do: [ :keyword :argumentName | stream nextPutAll: keyword; nextPutAll: argumentName ]]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:23:03'!
saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize
	
	self 
		extractKewordsAndArgumentsNamesFrom: userAnswer;
		assertValidUserMethodName: parseNodesToParameterize from: userAnswer;
		calculateNewMessageActualArguments: parseNodesToParameterize;
		calculateRenamedNewMessageArguments: parseNodesToParameterize;
		reorderActualArgumentsIfNecessaryBasedOn: parseNodesToParameterize;
		initializeNewSelector.! !

!ExtractMethodApplier methodsFor: 'private - reordering parameters' stamp: 'HAW 7/19/2023 19:11:59'!
reorderActualArgumentsBasedOn: parseNodesToParameterize

	newSelectorKeywords 
		with: userArgumentNames 
		do: [ :keyword :userArgumentName | 
			newMessageArguments 
				at: keyword 
				put: (parseNodesToParameterize detect: [:aParseNode | aParseNode name = userArgumentName ])].
	
	self initializeRenamedNewMessageArguments! !

!methodRemoval: ExtractMethodApplier #initializeRenamedNewMessageArguments: stamp: 'HAW 7/19/2023 19:17:30'!
ExtractMethodApplier removeSelector: #initializeRenamedNewMessageArguments:!
!methodRemoval: ExtractMethodApplier #initializeNewMessageArguments: stamp: 'HAW 7/19/2023 19:19:23'!
ExtractMethodApplier removeSelector: #initializeNewMessageArguments:!
!methodRemoval: ExtractMethodApplier #argumentNamesOf: stamp: 'HAW 7/19/2023 19:09:19'!
ExtractMethodApplier removeSelector: #argumentNamesOf:!
!methodRemoval: ExtractMethodApplier #calculateKewordsAndArgumentsNamesFrom: stamp: 'HAW 7/19/2023 19:22:23'!
ExtractMethodApplier removeSelector: #calculateKewordsAndArgumentsNamesFrom:!
!methodRemoval: ExtractMethodApplier #selectorKeywordsOf: stamp: 'HAW 7/19/2023 19:09:05'!
ExtractMethodApplier removeSelector: #selectorKeywordsOf:!
!methodRemoval: ExtractMethodApplier #initializeKewordsAndArgumentsNamesFrom: stamp: 'HAW 7/19/2023 19:19:08'!
ExtractMethodApplier removeSelector: #initializeKewordsAndArgumentsNamesFrom:!
!methodRemoval: ExtractMethodApplier #calculateNewMessageArguments: stamp: 'HAW 7/19/2023 19:23:03'!
ExtractMethodApplier removeSelector: #calculateNewMessageArguments:!

!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of: initializeRenamedNewMessageArguments)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacementsWith: requestNewMessage requestRefactoringParameters)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' assertHasRequiredParameters: assertValidMethodNameFrom: assertValidUserMethodName:from: buildInitialSelectorAnswer: calculateNewMessageActualArguments: calculateRenamedNewMessageArguments: extractKewordsAndArgumentsNamesFrom: formatAsKeyword: formatAsMethodArgument: initializeNewSelector keywordsAndArgumentsOf: parseNodesToParameterize reconstrucMethodName saveBinaryOrKeywordSelector:withArguments: sourceCodeToExtract)
('private - reordering parameters' hasSameActualArgumentsAsTheProposedOnes hasToReorderActualArguments reorderActualArgumentsBasedOn: reorderActualArgumentsIfNecessaryBasedOn:)
!

