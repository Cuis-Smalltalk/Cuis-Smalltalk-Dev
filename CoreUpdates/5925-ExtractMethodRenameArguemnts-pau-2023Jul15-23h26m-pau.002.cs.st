'From Cuis 6.0 [latest update: #5832] on 17 July 2023 at 6:39:44 pm'!
!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'intervalToExtract categoryOfNewSelector newMessage extractedSourceCode existingMethod newArgumentNames '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider sourceCodeToExtract newMethodRefactoring finder renamedNewMessageArguments '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethod class methodsFor: 'instance creation' stamp: 'pau 7/17/2023 13:22:16'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments 

	^self 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage 
			categorizedAs: aCategory 
			withArgumentNames: renamedArguments ) 
		replacements: (Array with: (ExtractMethodReplacement 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage))
	
! !


!ExtractMethodNewMethod methodsFor: 'initialization' stamp: 'pau 7/17/2023 13:23:33'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage in: aCategory withArgumentNames: renamedArguments  

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage.
	categoryOfNewSelector := aCategory.
	newArgumentNames := renamedArguments .
	self initializeExtractedSourceCode.! !

!ExtractMethodNewMethod methodsFor: 'private-rename arguments' stamp: 'pau 7/17/2023 16:26:28'!
compileMethodWithRenameArg: sourceCodeWithRenamedArgument

	^ self sourceClass
		compile: sourceCodeWithRenamedArgument
		classified: categoryOfNewSelector! !

!ExtractMethodNewMethod methodsFor: 'private-rename arguments' stamp: 'pau 7/17/2023 16:22:12'!
from: argName detectNodeNamed: newMethodNode

	^ (newMethodNode arguments) detect: [:arg| arg name = argName]! !

!ExtractMethodNewMethod methodsFor: 'private-rename arguments' stamp: 'pau 7/17/2023 16:23:52'!
newMethodNode

	^ (self sourceClass  methodDict at: (newMessage selector)) methodNode! !

!ExtractMethodNewMethod methodsFor: 'private-rename arguments' stamp: 'pau 7/17/2023 16:31:01'!
renameArgumentFrom: argNodeToRename to: newArgumentName in: newMethodNode

	^ (RenameTemporary fromOldVariableNode: argNodeToRename  
						to: newArgumentName 
						in:  newMethodNode) apply! !

!ExtractMethodNewMethod methodsFor: 'private-rename arguments' stamp: 'pau 7/17/2023 16:33:25'!
renameArguments

	newArgumentNames keysAndValuesDo: [:currentArgName :newArgumentName| 
		| newMethodNode sourceCodeWithRenamedArgument argNodeToRename |
		
		newMethodNode := self newMethodNode. 
		argNodeToRename := self from: currentArgName detectNodeNamed: newMethodNode. 
		sourceCodeWithRenamedArgument := self renameArgumentFrom: argNodeToRename to: newArgumentName in: newMethodNode. 
		
		self compileMethodWithRenameArg: sourceCodeWithRenamedArgument.
		].
! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'pau 7/17/2023 13:23:16'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments 

	| trimmedIntervalToExtract |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory 
		withArgumentNames: renamedArguments! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'pau 7/17/2023 13:07:31'!
argumentsTokensOf: userAnswer

	^((userAnswer findTokens: ':') allButFirst ) collect: [ :tok | (tok findTokens: Character separators) first ]! !


!ExtractMethodNewMethod methodsFor: 'applying' stamp: 'pau 7/16/2023 12:59:35'!
apply

	self sourceClass
		compile: self newMethodSourceCode
		classified: categoryOfNewSelector.
	
	self renameArguments. ! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'pau 7/17/2023 16:05:00'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	newMessageArguments := Dictionary new.
	renamedNewMessageArguments := Dictionary new! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'pau 7/16/2023 17:58:48'!
createExtractMethodNewMethodFor: newMessage

	^ newMethodRefactoring := ExtractMethodNewMethod 
		fromInterval: intervalToExtract
		of: methodToExtractCodeFrom
		to: newMessage
		categorizedAs: methodToExtractCodeFrom category 
		withArgumentNames: renamedNewMessageArguments! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'pau 7/17/2023 16:11:31'!
saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer

	| newSelectorKeywords newArguments |
	newSelectorKeywords := self selectorTokensOf: userAnswer.
	self validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords.
	
	parseNodesToParameterize withIndexDo: [ :parseNode :index |
		newMessageArguments at: (newSelectorKeywords at: index) put: parseNode ].
	
	newArguments := self argumentsTokensOf: userAnswer.
	newArguments with: parseNodesToParameterize do: [:userArg :currentArg |  
		(userArg  ~= currentArg name) 
			ifTrue: [ renamedNewMessageArguments at: currentArg name put: userArg]]
! !

!methodRemoval: ExtractMethodApplier #selectorArgumentsOf: stamp: 'pau 7/16/2023 18:07:39'!
ExtractMethodApplier removeSelector: #selectorArgumentsOf:!
!methodRemoval: ExtractMethodNewMethod class #fromInterval:of:to:categorizedAs: stamp: 'pau 7/16/2023 00:34:02'!
ExtractMethodNewMethod class removeSelector: #fromInterval:of:to:categorizedAs:!
!methodRemoval: ExtractMethodNewMethod #initializeFrom:of:to:in: stamp: 'pau 7/16/2023 00:35:31'!
ExtractMethodNewMethod removeSelector: #initializeFrom:of:to:in:!
!methodRemoval: ExtractMethodNewMethod #renameIn:to:from: stamp: 'pau 7/17/2023 16:30:20'!
ExtractMethodNewMethod removeSelector: #renameIn:to:from:!
!methodRemoval: ExtractMethodNewMethod #renameArgumentFromIn:to:from: stamp: 'pau 7/17/2023 16:30:42'!
ExtractMethodNewMethod removeSelector: #renameArgumentFromIn:to:from:!
!methodRemoval: ExtractMethod class #fromInterval:of:to:categorizedAs: stamp: 'pau 7/16/2023 00:34:02'!
ExtractMethod class removeSelector: #fromInterval:of:to:categorizedAs:!

!ExtractMethod class reorganize!
('instance creation' fromInterval:of:to:categorizedAs:withArgumentNames: newDefinition:replacements:)
!

!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'intervalToExtract categoryOfNewSelector newMessage extractedSourceCode existingMethod newArgumentNames'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodNewMethod reorganize!
('applying' apply)
('initialization' initializeExtractedSourceCode initializeFrom:of:to:in:withArgumentNames:)
('private - source code' newMessageString newMethodSourceCode returnCharacterIfNeeded sourceClass startingMethodIdentation)
('private-rename arguments' compileMethodWithRenameArg: from:detectNodeNamed: newMethodNode renameArgumentFrom:to:in: renameArguments)
!


!ExtractMethodNewMethod class reorganize!
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage wrongNumberOfArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalNoSelectedCodeError signalOutOfBoundsIntervalError)
('instance creation' fromInterval:of:to:categorizedAs:withArgumentNames:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
('private - pre-conditions' assert:canBeDefinedIn: assert:containsValidCodeToBeExtractedAt: assert:hasValidParametersForExtracting:from: assert:isWithinBoundsOf: assertIntervalToExtractIsNotEmpty: is:withinBoundsOf: isNotEmpty:)
!

!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider sourceCodeToExtract newMethodRefactoring finder renamedNewMessageArguments'
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
('private - parameterizing' argumentsTokensOf: buildInitialSelectorAnswer: formatAsKeyword: formatAsMethodArgument: parseNodesToParameterize saveBinaryOrKeywordSelector:withArguments: saveMessageArgumentsForEach:using: selectorTokensOf: sourceCodeToExtract validateRequiredParameters:haveACorrespondingKeywordIn:)
!

