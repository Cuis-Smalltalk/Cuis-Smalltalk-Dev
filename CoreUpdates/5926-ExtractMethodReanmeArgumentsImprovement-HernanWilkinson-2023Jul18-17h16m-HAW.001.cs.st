'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 12:36:52 pm'!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodNewMethodSourceCode commentStamp: '<historical>' prior: 0!
I responsible of creating the new source code for the extracted method!

!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'intervalToExtract categoryOfNewSelector newMessage extractedSourceCode existingMethod newArgumentNames newSourceCode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider sourceCodeToExtract newMethodRefactoring finder renamedNewMessageArguments newSelectorKeywords userArgumentNames '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Message methodsFor: 'accessing' stamp: 'HAW 7/19/2023 12:01:52'!
keywords
	
	^selector keywords ! !

!Message methodsFor: 'testing' stamp: 'HAW 7/19/2023 12:22:10'!
hasRightNumberOfArguments
	
	selector isUnary ifTrue: [ ^args isEmpty ].
	^args size = selector keywords size! !


!ExtractMethodNewMethodSourceCode methodsFor: 'value' stamp: 'HAW 7/19/2023 11:29:45'!
value

	self 
		initializeSourceCodeToExtract;
		initiliazeNewSourceCode;
		renameArguments. 
	
	^newSourceCode ! !

!ExtractMethodNewMethodSourceCode methodsFor: 'initialization' stamp: 'HAW 7/19/2023 11:29:14'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withArgumentNames: renamedArguments  

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage.
	newArgumentNames := renamedArguments .
	! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:20:55'!
initializeSourceCodeToExtract

	sourceCodeToExtract := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:21:21'!
initiliazeNewSourceCode

	newSourceCode := String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation;
			nextPutAll: self returnCharacterIfNeeded;
			nextPutAll: sourceCodeToExtract ]! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:23:29'!
needsExplicitReturn: extractedMethodNode

	^ extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ]! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 10:42:29'!
newMessageString

	^ newMessage fullName! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:26:37'!
renameArgumentFrom: currentArgumentName to: newArgumentName

	| methodNode | 
	
	methodNode := self sourceClass methodNodeFor: newSourceCode. 
	newSourceCode := (RenameTemporary from: currentArgumentName to: newArgumentName in: methodNode) apply! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:24:40'!
renameArguments

	newArgumentNames keysAndValuesDo: [ :currentArgumentName :newArgumentName | 
		self renameArgumentFrom: currentArgumentName to: newArgumentName ].
! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:23:35'!
returnCharacterIfNeeded

	| extractedMethodNode |
	
	extractedMethodNode := Parser parse: sourceCodeToExtract class: self sourceClass noPattern: true.
	
	^ (self needsExplicitReturn: extractedMethodNode) ifTrue: [ '' ] ifFalse: [ '^ ' ]! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 10:42:29'!
sourceClass

	^ existingMethod methodClass! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 7/19/2023 11:30:32'!
startingMethodIdentation

	^ `String lfString , String lfString , String tab`! !


!ExtractMethodNewMethodSourceCode class methodsFor: 'instance creation' stamp: 'HAW 7/19/2023 11:32:27'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withArgumentNames: renamedArguments 

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments! !


!ExtractMethodNewMethod methodsFor: 'initialization' stamp: 'HAW 7/19/2023 11:45:44'!
initializeOf: aMethodToExtractCodeFrom in: aCategory newSourceCode: aNewSourceCode   

	existingMethod := aMethodToExtractCodeFrom.
	categoryOfNewSelector := aCategory.
	newSourceCode := aNewSourceCode ! !


!ExtractMethodNewMethod class methodsFor: 'error messages' stamp: 'HAW 7/19/2023 09:29:31'!
invalidMethodNameError

	^ 'Invalid method name'! !

!ExtractMethodNewMethod class methodsFor: 'exceptions' stamp: 'HAW 7/19/2023 09:29:18'!
signalInvalidMethodNameError

	self refactoringError: self invalidMethodNameError! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/18/2023 17:59:41'!
argumentNamesOf: userAnswer

	^(self keywordsAndArgumentsOf: userAnswer) pairsCollect: [ :keyword :argument | argument ].! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 09:24:44'!
assertHasRequiredParameters: parseNodesToParameterize 

	newSelectorKeywords size = parseNodesToParameterize size
		ifFalse: [ ExtractMethodNewMethod signalExtractMethodWithWrongNumberOfArgumentsError ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 09:28:57'!
assertValidMethodNameFrom: userAnswer

	| userAnswerWithoutSeparators |
	
	userAnswerWithoutSeparators := userAnswer reject: [:aChar | aChar isSeparator ].
	userAnswerWithoutSeparators = self reconstrucMethodName 
		ifFalse: [  ExtractMethodNewMethod signalInvalidMethodNameError ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 09:24:48'!
assertValidUserMethodName: parseNodesToParameterize from: userAnswer 

	self assertValidMethodNameFrom: userAnswer.
	self assertHasRequiredParameters: parseNodesToParameterize! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 09:09:16'!
initializeKewordsAndArgumentsNamesFrom: userAnswer

	newSelectorKeywords := OrderedCollection new.
	userArgumentNames := OrderedCollection new.
	(self keywordsAndArgumentsOf: userAnswer) pairsDo: [ :keyword :argument | 
		newSelectorKeywords add: keyword, ':'.
		userArgumentNames add: argument ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 10:09:35'!
initializeNewMessageArguments: parseNodesToParameterize

	^ parseNodesToParameterize with: newSelectorKeywords do: [ :parseNode :keyword | newMessageArguments at: keyword put: parseNode ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 10:11:00'!
initializeNewSelector

	^ newSelector := ('' join: newSelectorKeywords) asSymbol! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 10:10:48'!
initializeRenamedNewMessageArguments: parseNodesToParameterize

	^ userArgumentNames with: parseNodesToParameterize do: [ :userArgumentName :currentArgument |  
		userArgumentName  ~= currentArgument name
			ifTrue: [ renamedNewMessageArguments at: currentArgument name put: userArgumentName]]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/18/2023 17:55:55'!
keywordsAndArgumentsOf: userAnswer

	^ userAnswer findTokens: `': ', String newLineString`! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 09:26:13'!
reconstrucMethodName

	^ String streamContents: [ :stream |
		newSelectorKeywords with: userArgumentNames do: [ :keyword :argumentName |
			stream nextPutAll: keyword; nextPutAll: argumentName ]]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/18/2023 17:51:36'!
selectorKeywordsOf: userAnswer
	"this selects the pieces of strings before each $:"

	^ (self keywordsAndArgumentsOf: userAnswer) pairsCollect: [ :keyword :argument | keyword, ':' ]! !


!Message methodsFor: 'accessing' stamp: 'HAW 7/19/2023 10:30:46'!
fullName

	^ String streamContents: [ :messageStream |
		args
			ifEmpty: [ messageStream nextPutAll: selector ]
			ifNotEmpty: [ 
				selector keywords with: args 
					do: [ :keyword :argumentName |
						messageStream
							nextPutAll: keyword;
							nextPut:  Character space;
							nextPutAll: argumentName  ]
					separatedBy: [ messageStream nextPut: Character space ]]]! !


!ExtractMethodParametersDetector methodsFor: 'evaluating' stamp: 'HAW 7/18/2023 17:24:03'!
value

	| parseNodesFound |
	
	parseNodesFound := OrderedCollection new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ parseNodesFound add: parseNode ]].
		
	^ parseNodesFound! !


!ExtractMethodNewMethod methodsFor: 'applying' stamp: 'HAW 7/19/2023 11:44:06'!
apply

	existingMethod methodClass
		compile: newSourceCode 
		classified: categoryOfNewSelector.
	
	! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'HAW 7/19/2023 12:07:25'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments 

	| trimmedIntervalToExtract newSourceCode |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.
	
	"With the renamed arguments functionality, I want to signal errors at creation time, not at applying time. 
	So I need to cheeck that renaming arguments is valid, and to do that I need to rename them all, that is why
	now the new source code is created at instantiation time and not applying time - Hernan."
	newSourceCode := (ExtractMethodNewMethodSourceCode 
		fromInterval: trimmedIntervalToExtract 
		of: aMethodToExtractCodeFrom 
		to: newMessage
		withArgumentNames: renamedArguments) value.

	^ self new
		initializeOf: aMethodToExtractCodeFrom
		in: aCategory 
		newSourceCode: newSourceCode! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 7/19/2023 12:26:33'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor

	| parseNodesToParameterize |
	
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract.
	
	(newMessage hasRightNumberOfArguments and: [ newMessage arguments size = parseNodesToParameterize size ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 10:11:34'!
saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize
	
	self 
		initializeKewordsAndArgumentsNamesFrom: userAnswer;
		assertValidUserMethodName: parseNodesToParameterize from: userAnswer;
		initializeNewMessageArguments: parseNodesToParameterize;
		initializeRenamedNewMessageArguments: parseNodesToParameterize;
		initializeNewSelector.! !

!methodRemoval: ExtractMethodApplier #keywordOf: stamp: 'HAW 7/18/2023 17:51:13'!
ExtractMethodApplier removeSelector: #keywordOf:!
!methodRemoval: ExtractMethodApplier #validateRequiredParameters:haveACorrespondingKeywordIn: stamp: 'HAW 7/18/2023 18:21:18'!
ExtractMethodApplier removeSelector: #validateRequiredParameters:haveACorrespondingKeywordIn:!
!methodRemoval: ExtractMethodApplier #assertValidUserMessageName: stamp: 'HAW 7/19/2023 09:17:15'!
ExtractMethodApplier removeSelector: #assertValidUserMessageName:!
!methodRemoval: ExtractMethodApplier #assertValidUserMessageName:from: stamp: 'HAW 7/19/2023 09:24:01'!
ExtractMethodApplier removeSelector: #assertValidUserMessageName:from:!
!methodRemoval: ExtractMethodApplier #saveMessageArgumentsForEach:using: stamp: 'HAW 7/18/2023 18:12:59'!
ExtractMethodApplier removeSelector: #saveMessageArgumentsForEach:using:!
!methodRemoval: ExtractMethodApplier #splitArgumentAndKeywordFrom: stamp: 'HAW 7/18/2023 17:52:35'!
ExtractMethodApplier removeSelector: #splitArgumentAndKeywordFrom:!
!methodRemoval: ExtractMethodApplier #selectorTokensOf: stamp: 'HAW 7/18/2023 17:21:07'!
ExtractMethodApplier removeSelector: #selectorTokensOf:!
!methodRemoval: ExtractMethodApplier #argumentOf: stamp: 'HAW 7/18/2023 17:52:38'!
ExtractMethodApplier removeSelector: #argumentOf:!
!methodRemoval: ExtractMethodApplier #argumentsOf: stamp: 'HAW 7/18/2023 17:59:41'!
ExtractMethodApplier removeSelector: #argumentsOf:!
!methodRemoval: ExtractMethodApplier #validateRequiredParameters: stamp: 'HAW 7/19/2023 09:24:44'!
ExtractMethodApplier removeSelector: #validateRequiredParameters:!
!methodRemoval: ExtractMethodApplier #argumentsTokensOf: stamp: 'HAW 7/18/2023 17:58:51'!
ExtractMethodApplier removeSelector: #argumentsTokensOf:!
!methodRemoval: ExtractMethodNewMethod #compileMethodWithRenameArg: stamp: 'HAW 7/19/2023 11:38:58'!
ExtractMethodNewMethod removeSelector: #compileMethodWithRenameArg:!
!methodRemoval: ExtractMethodNewMethod #renameArguments stamp: 'HAW 7/19/2023 11:38:58'!
ExtractMethodNewMethod removeSelector: #renameArguments!
!methodRemoval: ExtractMethodNewMethod #from:detectNodeNamed: stamp: 'HAW 7/19/2023 11:38:58'!
ExtractMethodNewMethod removeSelector: #from:detectNodeNamed:!
!methodRemoval: ExtractMethodNewMethod #newMethodNode stamp: 'HAW 7/19/2023 11:38:58'!
ExtractMethodNewMethod removeSelector: #newMethodNode!
!methodRemoval: ExtractMethodNewMethod #renameArgumentFrom:to:in: stamp: 'HAW 7/19/2023 11:38:58'!
ExtractMethodNewMethod removeSelector: #renameArgumentFrom:to:in:!
!methodRemoval: ExtractMethodNewMethod #startingMethodIdentation stamp: 'HAW 7/19/2023 11:43:12'!
ExtractMethodNewMethod removeSelector: #startingMethodIdentation!
!methodRemoval: ExtractMethodNewMethod #initializeFrom:of:in:withArgumentNames:newSourceCode: stamp: 'HAW 7/19/2023 11:45:20'!
ExtractMethodNewMethod removeSelector: #initializeFrom:of:in:withArgumentNames:newSourceCode:!
!methodRemoval: ExtractMethodNewMethod #returnCharacterIfNeeded stamp: 'HAW 7/19/2023 11:43:20'!
ExtractMethodNewMethod removeSelector: #returnCharacterIfNeeded!
!methodRemoval: ExtractMethodNewMethod #sourceClass stamp: 'HAW 7/19/2023 11:43:58'!
ExtractMethodNewMethod removeSelector: #sourceClass!
!methodRemoval: ExtractMethodNewMethod #initializeFrom:of:to:in:withArgumentNames:newSourceCode: stamp: 'HAW 7/19/2023 11:45:12'!
ExtractMethodNewMethod removeSelector: #initializeFrom:of:to:in:withArgumentNames:newSourceCode:!
!methodRemoval: ExtractMethodNewMethod #initializeFrom:of:in:newSourceCode: stamp: 'HAW 7/19/2023 11:45:32'!
ExtractMethodNewMethod removeSelector: #initializeFrom:of:in:newSourceCode:!
!methodRemoval: ExtractMethodNewMethod #initializeFrom:of:to:in:withArgumentNames: stamp: 'HAW 7/19/2023 11:12:04'!
ExtractMethodNewMethod removeSelector: #initializeFrom:of:to:in:withArgumentNames:!
!methodRemoval: ExtractMethodNewMethod #initializeExtractedSourceCode stamp: 'HAW 7/19/2023 11:42:33'!
ExtractMethodNewMethod removeSelector: #initializeExtractedSourceCode!
!methodRemoval: ExtractMethodNewMethod #newMessageString stamp: 'HAW 7/19/2023 11:43:31'!
ExtractMethodNewMethod removeSelector: #newMessageString!
!methodRemoval: ExtractMethodNewMethod #of:in:newSourceCode: stamp: 'HAW 7/19/2023 11:45:44'!
ExtractMethodNewMethod removeSelector: #of:in:newSourceCode:!
!methodRemoval: ExtractMethodNewMethod #newMethodSourceCode stamp: 'HAW 7/19/2023 11:41:32'!
ExtractMethodNewMethod removeSelector: #newMethodSourceCode!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assert:canBeDefinedIn: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #assert:canBeDefinedIn:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assert:hasValidParametersForExtracting:from: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #assert:hasValidParametersForExtracting:from:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assert:isWithinBoundsOf: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #assert:isWithinBoundsOf:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assertIntervalToExtractIsNotEmpty: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #assertIntervalToExtractIsNotEmpty:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #is:withinBoundsOf: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #is:withinBoundsOf:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #isNotEmpty: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #isNotEmpty:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #noSelectionErrorMessage stamp: 'HAW 7/19/2023 11:32:53'!
ExtractMethodNewMethodSourceCode class removeSelector: #noSelectionErrorMessage!
!methodRemoval: ExtractMethodNewMethodSourceCode class #signalNoSelectedCodeError stamp: 'HAW 7/19/2023 11:32:55'!
ExtractMethodNewMethodSourceCode class removeSelector: #signalNoSelectedCodeError!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assertCanApplyRefactoringOn:at: stamp: 'HAW 7/19/2023 11:32:58'!
ExtractMethodNewMethodSourceCode class removeSelector: #assertCanApplyRefactoringOn:at:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #signalExtractMethodWithWrongNumberOfArgumentsError stamp: 'HAW 7/19/2023 11:32:55'!
ExtractMethodNewMethodSourceCode class removeSelector: #signalExtractMethodWithWrongNumberOfArgumentsError!
!methodRemoval: ExtractMethodNewMethodSourceCode class #invalidMethodNameError stamp: 'HAW 7/19/2023 11:32:53'!
ExtractMethodNewMethodSourceCode class removeSelector: #invalidMethodNameError!
!methodRemoval: ExtractMethodNewMethodSourceCode class #signalInvalidMethodNameError stamp: 'HAW 7/19/2023 11:32:55'!
ExtractMethodNewMethodSourceCode class removeSelector: #signalInvalidMethodNameError!
!methodRemoval: ExtractMethodNewMethodSourceCode class #wrongNumberOfArgumentsGivenErrorMessage stamp: 'HAW 7/19/2023 11:32:53'!
ExtractMethodNewMethodSourceCode class removeSelector: #wrongNumberOfArgumentsGivenErrorMessage!
!methodRemoval: ExtractMethodNewMethodSourceCode class #outOfBoundsSelectionErrorMessage stamp: 'HAW 7/19/2023 11:32:53'!
ExtractMethodNewMethodSourceCode class removeSelector: #outOfBoundsSelectionErrorMessage!
!methodRemoval: ExtractMethodNewMethodSourceCode class #signalOutOfBoundsIntervalError stamp: 'HAW 7/19/2023 11:32:55'!
ExtractMethodNewMethodSourceCode class removeSelector: #signalOutOfBoundsIntervalError!
!methodRemoval: ExtractMethodNewMethodSourceCode class #assert:containsValidCodeToBeExtractedAt: stamp: 'HAW 7/19/2023 11:33:00'!
ExtractMethodNewMethodSourceCode class removeSelector: #assert:containsValidCodeToBeExtractedAt:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #fromInterval:of:to:categorizedAs:withArgumentNames: stamp: 'HAW 7/19/2023 11:32:27'!
ExtractMethodNewMethodSourceCode class removeSelector: #fromInterval:of:to:categorizedAs:withArgumentNames:!
!methodRemoval: ExtractMethodNewMethodSourceCode #compileMethodWithRenameArg: stamp: 'HAW 7/19/2023 11:15:38'!
ExtractMethodNewMethodSourceCode removeSelector: #compileMethodWithRenameArg:!
!methodRemoval: ExtractMethodNewMethodSourceCode #newMethodNode stamp: 'HAW 7/19/2023 11:15:33'!
ExtractMethodNewMethodSourceCode removeSelector: #newMethodNode!
!methodRemoval: ExtractMethodNewMethodSourceCode #apply stamp: 'HAW 7/19/2023 10:45:10'!
ExtractMethodNewMethodSourceCode removeSelector: #apply!
!methodRemoval: ExtractMethodNewMethodSourceCode #renameArgumentFrom:to:in: stamp: 'HAW 7/19/2023 11:15:30'!
ExtractMethodNewMethodSourceCode removeSelector: #renameArgumentFrom:to:in:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initiliazeNewMethodSourceCode stamp: 'HAW 7/19/2023 11:21:21'!
ExtractMethodNewMethodSourceCode removeSelector: #initiliazeNewMethodSourceCode!
!methodRemoval: ExtractMethodNewMethodSourceCode #from:detectNodeNamed: stamp: 'HAW 7/19/2023 11:15:35'!
ExtractMethodNewMethodSourceCode removeSelector: #from:detectNodeNamed:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initializeFrom:of:to:in:withArgumentNames: stamp: 'HAW 7/19/2023 11:29:14'!
ExtractMethodNewMethodSourceCode removeSelector: #initializeFrom:of:to:in:withArgumentNames:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initializeExtractedSourceCode stamp: 'HAW 7/19/2023 11:20:55'!
ExtractMethodNewMethodSourceCode removeSelector: #initializeExtractedSourceCode!
!methodRemoval: ExtractMethodNewMethodSourceCode #newMethodSourceCode stamp: 'HAW 7/19/2023 11:21:06'!
ExtractMethodNewMethodSourceCode removeSelector: #newMethodSourceCode!
!methodRemoval: Message #pushReceiver stamp: 'HAW 7/19/2023 12:34:36'!
Message removeSelector: #pushReceiver!

!Message reorganize!
('accessing' argument arguments fullName keywords lookupClass numArgs selector sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!


!ExtractMethodNewMethodSourceCode reorganize!
('value' value)
('initialization' initializeFrom:of:to:withArgumentNames:)
('private - source code' initializeSourceCodeToExtract initiliazeNewSourceCode needsExplicitReturn: newMessageString renameArgumentFrom:to: renameArguments returnCharacterIfNeeded sourceClass startingMethodIdentation)
!


!ExtractMethodNewMethodSourceCode class reorganize!
('instance creation' fromInterval:of:to:withArgumentNames:)
!

!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'categoryOfNewSelector existingMethod newSourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodNewMethod reorganize!
('applying' apply)
('initialization' initializeOf:in:newSourceCode:)
!


!ExtractMethodNewMethod class reorganize!
('error messages' invalidMethodNameError noSelectionErrorMessage outOfBoundsSelectionErrorMessage wrongNumberOfArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalInvalidMethodNameError signalNoSelectedCodeError signalOutOfBoundsIntervalError)
('instance creation' fromInterval:of:to:categorizedAs:withArgumentNames:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
('private - pre-conditions' assert:canBeDefinedIn: assert:containsValidCodeToBeExtractedAt: assert:hasValidParametersForExtracting:from: assert:isWithinBoundsOf: assertIntervalToExtractIsNotEmpty: is:withinBoundsOf: isNotEmpty:)
!

!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider sourceCodeToExtract newMethodRefactoring finder renamedNewMessageArguments newSelectorKeywords userArgumentNames'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacementsWith: requestNewMessage requestRefactoringParameters)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' argumentNamesOf: assertHasRequiredParameters: assertValidMethodNameFrom: assertValidUserMethodName:from: buildInitialSelectorAnswer: formatAsKeyword: formatAsMethodArgument: initializeKewordsAndArgumentsNamesFrom: initializeNewMessageArguments: initializeNewSelector initializeRenamedNewMessageArguments: keywordsAndArgumentsOf: parseNodesToParameterize reconstrucMethodName saveBinaryOrKeywordSelector:withArguments: selectorKeywordsOf: sourceCodeToExtract)
!

