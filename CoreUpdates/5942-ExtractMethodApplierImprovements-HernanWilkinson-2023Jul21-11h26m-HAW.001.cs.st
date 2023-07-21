'From Cuis 6.0 [latest update: #5941] on 21 July 2023 at 11:40:15 am'!

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/21/2023 11:35:58'!
saveSelectorFrom: userAnswer for: parseNodesToParameterize

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ]! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/21/2023 11:33:23'!
selectorCaption: parseNodesToParameterize
	
	| caption |
	
	caption := 'New method name:'.
	parseNodesToParameterize ifNotEmpty: [
		caption := caption, `String newLineString, '(Reorder parameters with same names or rename them with new names)'`].
	
	^caption! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:27:06'!
initialSelectorKeyword

	^ 'keyword'! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:38:14'!
textFor: aString withAttribute: anAttribute 

	^ Text
		string: aString
		attributes: (SHTextStylerST80 attributesFor: anAttribute)! !


!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/21/2023 11:36:10'!
requestNewMessage

	| parseNodesToParameterize initialAnswer userAnswer  caption |

	parseNodesToParameterize := self parseNodesToParameterize.
	initialAnswer := self buildInitialSelectorAnswer: parseNodesToParameterize.
	caption := self selectorCaption: parseNodesToParameterize.
	userAnswer := self request: caption initialAnswer: initialAnswer.
	self saveSelectorFrom: userAnswer for: parseNodesToParameterize.

	^self buildNewMessage.
	! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:27:25'!
buildInitialKeywordSelectorAnswer: parseNodesToParameterize

	^ Text streamContents: [ :stream |
		parseNodesToParameterize 
			withIndexDo: [ :parseNode :index | 
				stream
					nextPutAll: (self formatAsKeyword: self initialSelectorKeyword, index printString, ': ');
					nextPutAll: (self formatAsMethodArgument: parseNode name) ]
			separatedBy: [ stream newLine ]]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:31:20'!
buildInitialSelectorAnswer: parseNodesToParameterize
	
	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: self initialSelectorKeyword ]
		ifNotEmpty: [ self buildInitialKeywordSelectorAnswer: parseNodesToParameterize]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:38:54'!
formatAsKeyword: aKeyword

	^ self textFor: aKeyword withAttribute: #patternKeyword! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/21/2023 11:38:14'!
formatAsMethodArgument: aMethodArgumentName

	^ self textFor: aMethodArgumentName withAttribute: #methodArg! !

!methodRemoval: ExtractMethodApplier #saveSelectorFor:for: stamp: 'HAW 7/21/2023 11:35:58'!
ExtractMethodApplier removeSelector: #saveSelectorFor:for:!
!methodRemoval: ExtractMethodApplier #textFor: stamp: 'HAW 7/21/2023 11:38:14'!
ExtractMethodApplier removeSelector: #textFor:!

!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of: initializeRenamedNewMessageArguments)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacementsWith: requestNewMessage requestRefactoringParameters saveSelectorFrom:for: selectorCaption:)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' assertHasRequiredParameters: assertValidMethodNameFrom: assertValidUserMethodName:from: buildInitialKeywordSelectorAnswer: buildInitialSelectorAnswer: calculateNewMessageActualArguments: calculateRenamedNewMessageArguments: extractKewordsAndArgumentsNamesFrom: formatAsKeyword: formatAsMethodArgument: initialSelectorKeyword initializeNewSelector keywordsAndArgumentsOf: parseNodesToParameterize reconstrucMethodName saveBinaryOrKeywordSelector:withArguments: sourceCodeToExtract textFor:withAttribute:)
('private - reordering parameters' hasSameActualArgumentsAsTheProposedOnes hasToReorderActualArguments reorderActualArgumentsBasedOn: reorderActualArgumentsIfNecessaryBasedOn:)
!

