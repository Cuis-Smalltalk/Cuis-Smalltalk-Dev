Class {
	#name : 'ExtractMethodApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'intervalToExtract',
		'methodToExtractCodeFrom',
		'newSelector',
		'newMessageArguments',
		'codeProvider',
		'sourceCodeToExtract',
		'newMethodRefactoring',
		'finder',
		'renamedNewMessageArguments',
		'newSelectorKeywords',
		'userArgumentNames'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ExtractMethodApplier class >> assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract [

	ExtractMethodNewMethod 
		assertCanApplyRefactoringOn: aMethodToRefactor
		at: anIntervalToExtract
]

{ #category : 'instance creation' }
ExtractMethodApplier class >> on: aCodeProvider for: anIntervalToExtract of: aMethodToRefactor [

	| trimmedIntervalToExtract sourceCode |
	
	sourceCode := aMethodToRefactor sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode..

	self assertCanApplyRefactoringOn: aMethodToRefactor at: trimmedIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: trimmedIntervalToExtract of: aMethodToRefactor
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> assertHasRequiredParameters: parseNodesToParameterize  [

	newSelectorKeywords size = parseNodesToParameterize size
		ifFalse: [ ExtractMethodNewMethod signalExtractMethodWithWrongNumberOfArgumentsError ]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> assertValidMethodNameFrom: userAnswer [

	| userAnswerWithoutSeparators |
	
	userAnswerWithoutSeparators := userAnswer reject: [:aChar | aChar isSeparator ].
	userAnswerWithoutSeparators = self reconstrucMethodName 
		ifFalse: [  ExtractMethodNewMethod signalInvalidMethodNameError ]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> assertValidUserMethodName: parseNodesToParameterize from: userAnswer  [

	self assertValidMethodNameFrom: userAnswer.
	self assertHasRequiredParameters: parseNodesToParameterize
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> buildInitialKeywordSelectorAnswer: parseNodesToParameterize [

	^ Text streamContents: [ :stream |
		parseNodesToParameterize 
			withIndexDo: [ :parseNode :index | 
				stream
					nextPutAll: (self formatAsKeyword: self initialSelectorKeyword, index printString, ': ');
					nextPutAll: (self formatAsMethodArgument: parseNode name) ]
			separatedBy: [ stream newLine ]]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> buildInitialSelectorAnswer: parseNodesToParameterize [
	
	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: self initialSelectorKeyword ]
		ifNotEmpty: [ self buildInitialKeywordSelectorAnswer: parseNodesToParameterize]
]

{ #category : 'private - new message' }
ExtractMethodApplier >> buildNewMessage [

	^ Message
		selector: newSelector
		arguments: self newMessageArgumentNames
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> calculateNewMessageActualArguments: parseNodesToParameterize [

	parseNodesToParameterize 
		with: newSelectorKeywords 
		do: [ :parseNode :keyword | newMessageArguments at: keyword put: parseNode ]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> calculateRenamedNewMessageArguments: parseNodesToParameterize [

	userArgumentNames 
		with: parseNodesToParameterize 
		do: [ :userArgumentName :actualArgument |  
			userArgumentName  ~= actualArgument name
				ifTrue: [ renamedNewMessageArguments at: actualArgument name put: userArgumentName]]
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> createExtractMethodNewMethodFor: newMessage [

	^ newMethodRefactoring := ExtractMethodNewMethod 
		fromInterval: intervalToExtract
		of: methodToExtractCodeFrom
		to: newMessage
		categorizedAs: methodToExtractCodeFrom category 
		withArgumentNames: renamedNewMessageArguments
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> createRefactoring [
	
	^ self shouldNotImplement
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> createRefactoringForMethodsInClass [
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder inClassReplacements 
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> createRefactoringForOriginalSelection [
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements:  { finder originalSelectionReplacement }
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> createRefactoringForSourceMethod [
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder sourceMethodReplacements 
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> createRefactoringWithAllReplacements [
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder replacements 
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> extractKewordsAndArgumentsNamesFrom: userAnswer [

	newSelectorKeywords := OrderedCollection new.
	userArgumentNames := OrderedCollection new.
	(self keywordsAndArgumentsOf: userAnswer) pairsDo: [ :keyword :argument | 
		newSelectorKeywords add: keyword, ':'.
		userArgumentNames add: argument ]
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> findReplacementsWith: newMessage [

	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: methodToExtractCodeFrom to: newMessage.
	finder value
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> formatAsKeyword: aKeyword [

	^ self textFor: aKeyword withAttribute: #patternKeyword
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> formatAsMethodArgument: aMethodArgumentName [

	^ self textFor: aMethodArgumentName withAttribute: #methodArg
]

{ #category : 'private - reordering parameters' }
ExtractMethodApplier >> hasSameActualArgumentsAsTheProposedOnes [

	^ self newMessageArgumentNames asSet = userArgumentNames asSet
]

{ #category : 'private - reordering parameters' }
ExtractMethodApplier >> hasToReorderActualArguments [

	^ renamedNewMessageArguments notEmpty and:  [ self hasSameActualArgumentsAsTheProposedOnes ]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> initialSelectorKeyword [

	^ 'keyword'
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> initializeNewSelector [

	newSelector := ('' join: newSelectorKeywords) asSymbol
]

{ #category : 'initialization' }
ExtractMethodApplier >> initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom [

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	newMessageArguments := OrderedDictionary new.
	self initializeRenamedNewMessageArguments
]

{ #category : 'initialization' }
ExtractMethodApplier >> initializeRenamedNewMessageArguments [

	renamedNewMessageArguments := Dictionary new
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> keywordsAndArgumentsOf: userAnswer [

	^ userAnswer findTokens: `': ', String newLineString`
]

{ #category : 'private - new message' }
ExtractMethodApplier >> newMessageArgumentNames [

	^ newMessageArguments values collect: [ :parseNode | parseNode name ]
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> parseNodesToParameterize [

	^ ExtractMethodParametersDetector
		valueFor: methodToExtractCodeFrom notOptimizedMethodNode
		at: intervalToExtract
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> reconstrucMethodName [

	^ String streamContents: [ :stream |
		newSelectorKeywords 
			with: userArgumentNames 
			do: [ :keyword :argumentName | stream nextPutAll: keyword; nextPutAll: argumentName ]]
]

{ #category : 'refactoring - creation' }
ExtractMethodApplier >> refactoringClass [

	^ ExtractMethod
]

{ #category : 'private - reordering parameters' }
ExtractMethodApplier >> reorderActualArgumentsBasedOn: parseNodesToParameterize [

	newSelectorKeywords 
		with: userArgumentNames 
		do: [ :keyword :userArgumentName | 
			newMessageArguments 
				at: keyword 
				put: (parseNodesToParameterize detect: [:aParseNode | aParseNode name = userArgumentName ])].
	
	self initializeRenamedNewMessageArguments
]

{ #category : 'private - reordering parameters' }
ExtractMethodApplier >> reorderActualArgumentsIfNecessaryBasedOn: parseNodesToParameterize [

	self hasToReorderActualArguments ifTrue: [ self reorderActualArgumentsBasedOn: parseNodesToParameterize ].
	
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> requestNewMessage [

	| parseNodesToParameterize initialAnswer userAnswer  caption |

	parseNodesToParameterize := self parseNodesToParameterize.
	initialAnswer := self buildInitialSelectorAnswer: parseNodesToParameterize.
	caption := self selectorCaption: parseNodesToParameterize.
	userAnswer := self request: caption initialAnswer: initialAnswer.
	self saveSelectorFrom: userAnswer for: parseNodesToParameterize.

	^self buildNewMessage.
	
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> requestRefactoringParameters [

	| newMessage |
	
	newMessage := self requestNewMessage.
	self createExtractMethodNewMethodFor: newMessage.
	self findReplacementsWith: newMessage.
	
	finder hasOneReplacement 
		ifTrue: [ self valueWithAllReplacements ]
		ifFalse: [ ExtractMethodReplacementsWindow openFrom: self with: finder ]
		
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize [
	
	self 
		extractKewordsAndArgumentsNamesFrom: userAnswer;
		assertValidUserMethodName: parseNodesToParameterize from: userAnswer;
		calculateNewMessageActualArguments: parseNodesToParameterize;
		calculateRenamedNewMessageArguments: parseNodesToParameterize;
		reorderActualArgumentsIfNecessaryBasedOn: parseNodesToParameterize;
		initializeNewSelector.
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> saveSelectorFrom: userAnswer for: parseNodesToParameterize [

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ]
]

{ #category : 'private - new message' }
ExtractMethodApplier >> saveUnarySelector: userAnswer [

	newSelector := userAnswer asSymbol
]

{ #category : 'refactoring - parameters request' }
ExtractMethodApplier >> selectorCaption: parseNodesToParameterize [
	
	| caption |
	
	caption := 'New method name:'.
	parseNodesToParameterize ifNotEmpty: [
		caption := caption, `String newLineString, '(Reorder parameters with same names or rename them with new names)'`].
	
	^caption
]

{ #category : 'refactoring - changes' }
ExtractMethodApplier >> showChanges [

	codeProvider currentMethodRefactored
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> sourceCodeToExtract [
	
	^sourceCodeToExtract
]

{ #category : 'private - parameterizing' }
ExtractMethodApplier >> textFor: aString withAttribute: anAttribute  [

	^ Text
		string: aString
		attributes: (SHTextStylerST80 attributesFor: anAttribute)
]

{ #category : 'value' }
ExtractMethodApplier >> value [

	requestExitBlock := [ ^self ].
	
	self requestRefactoringParametersHandlingRefactoringExceptions

]

{ #category : 'value' }
ExtractMethodApplier >> valueCreatingWith: aRefactoringCreationBlock [

	self 
		createAndSetRefactoringHandlingRefactoringExceptions: aRefactoringCreationBlock;
		applyRefactoring;
		showChanges
	
	
]

{ #category : 'value' }
ExtractMethodApplier >> valueWithAllReplacements [

	self valueCreatingWith: [ self createRefactoringWithAllReplacements ]
	
]

{ #category : 'value' }
ExtractMethodApplier >> valueWithMethodsInClass [
	
	self valueCreatingWith: [ self createRefactoringForMethodsInClass ]
]

{ #category : 'value' }
ExtractMethodApplier >> valueWithOriginalSelection [
	
	self valueCreatingWith: [ self createRefactoringForOriginalSelection ]
]

{ #category : 'value' }
ExtractMethodApplier >> valueWithSourceMethod [
	
	self valueCreatingWith: [ self createRefactoringForSourceMethod ]
]
