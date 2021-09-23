'From Cuis 5.0 [latest update: #4876] on 22 September 2021 at 9:15:23 pm'!
!classDefinition: #AddParameter category: #'Tools-Refactoring'!
ChangeSelector subclass: #AddParameter
	instanceVariableNames: 'newParameter newParameterValue implementorTrailingString senderTrailingString index isAddingLast'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractAsParameter category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractAsParameter
	instanceVariableNames: 'intervalToExtract sourceMethod addParameter intervalToReplace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractAsParameterApplier category: #'Tools-Refactoring'!
AddParameterApplier subclass: #ExtractAsParameterApplier
	instanceVariableNames: 'interval'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 9/22/2021 20:24:46'!
contextualExtractAsParameter

	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self extractAsParameter ]]! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 9/22/2021 16:36:12'!
contextualExtractAsParameter: aKeyboardEvent 
	
	self contextualExtractAsParameter.
	^true! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 9/22/2021 20:24:56'!
extractAsParameter

	^ RefactoringApplier extractAsParameterApplier createAndValueHandlingExceptions: [
		RefactoringApplier extractAsParameterApplier
			from: self selectionInterval 
			on: model textProvider 
			for: self codeProvider selectedMessageName 
			in: self selectedClassOrMetaClassOrUndefinedObject  ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 9/22/2021 19:41:21'!
selectedClassOrMetaClassOrUndefinedObject

	"I have to do this because some codeProviders do not answer selectedClassOrMetaClass like the Workspace - Hernan"
	
	^ [ self codeProvider selectedClassOrMetaClass ] 
		on: Error 
		do: [ :anError | anError return: UndefinedObject ]! !


!ChangeSelector methodsFor: 'changes' stamp: 'HAW 9/22/2021 20:18:57'!
changes
	
	^changes! !


!AddParameter methodsFor: 'parameter' stamp: 'HAW 9/8/2021 22:37:02'!
newParameter
	
	^newParameter! !


!ExtractAsParameter methodsFor: 'applying' stamp: 'HAW 9/22/2021 20:19:25'!
apply
	
	self 
		applyAddParameter;
		useNewParameter.
		
	^addParameter changes
	
	! !

!ExtractAsParameter methodsFor: 'applying - private' stamp: 'HAW 9/22/2021 20:19:25'!
applyAddParameter

	^ addParameter apply! !

!ExtractAsParameter methodsFor: 'applying - private' stamp: 'HAW 9/22/2021 20:17:24'!
newSourceCode
	
	| intermediateSourceCode newInterval newSourceCode originalSourceCodeSize intermediateMethod |
	
	originalSourceCodeSize := sourceMethod sourceCode size.
	intermediateMethod := sourceMethod methodClass >> self newSelector.
	intermediateSourceCode :=  intermediateMethod sourceCode.
	newInterval := intervalToReplace  + (intermediateSourceCode size - originalSourceCodeSize).
	newSourceCode := intermediateSourceCode copyReplaceFrom: newInterval first to: newInterval last with: addParameter newParameter.
	
	^newSourceCode ! !

!ExtractAsParameter methodsFor: 'applying - private' stamp: 'HAW 9/22/2021 20:17:33'!
useNewParameter
	
	sourceMethod methodClass compile: self newSourceCode.
	
	! !

!ExtractAsParameter methodsFor: 'initialization' stamp: 'HAW 9/21/2021 19:13:59'!
initializeExtractedFrom: anIntervalToExtract replacing: anIntervalToReplace at: aSourceMethod addingParameterWith: anAddParameter 
	
	intervalToExtract := anIntervalToExtract.
	intervalToReplace := anIntervalToReplace.
	sourceMethod := aSourceMethod.
	addParameter := anAddParameter.! !

!ExtractAsParameter methodsFor: 'selectors' stamp: 'HAW 9/22/2021 19:56:45'!
newSelector
	
	^addParameter newSelector ! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:39:26'!
assert: aSourceMethod isInImplementors: implementorsCollection

	^ (implementorsCollection includes: aSourceMethod) ifFalse: [ self signalOrigialMethodMustBeInImplementorsToChange ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 20:07:25'!
assertAndCreateNamed: aNewParameter 
	extractedFrom: anInterval
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection
	creatingAddParameterWith: addParameterCreator

	| addParameter sourceCodeToExtract trimmedIntervalToReplace intervalToReplace intervals |
	
	self assert: aSourceMethod isInImplementors: implementorsCollection.
	intervals := self assertCanExtractedFrom: anInterval for: aSourceMethod.
	
	intervalToReplace := intervals first.
	trimmedIntervalToReplace := intervals second.
	sourceCodeToExtract := aSourceMethod sourceCode copyFrom: trimmedIntervalToReplace first to: trimmedIntervalToReplace last.
	
	addParameter := addParameterCreator value: sourceCodeToExtract.

	^self new 
		initializeExtractedFrom: trimmedIntervalToReplace 
		replacing: intervalToReplace 
		at: aSourceMethod 
		addingParameterWith: addParameter! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 19:01:23'!
assertCanExtractedFrom: anInterval for: aSourceMethod  

	| trimmedIntervalToReplace sourceCode node intervalToReplace |
	
	sourceCode := aSourceMethod sourceCode.
	
	ExtractMethodNewMethod assertIntervalToExtractIsNotEmpty: anInterval.
	ExtractMethodNewMethod assert: anInterval isWithinBoundsOf: sourceCode.
	
	intervalToReplace := self removeDotsAt: anInterval in: sourceCode.
	trimmedIntervalToReplace := intervalToReplace asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: aSourceMethod at: trimmedIntervalToReplace or: intervalToReplace.
	self assertIsValidToExtract: node.
	
	^{ intervalToReplace. trimmedIntervalToReplace }

	! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 14:26:11'!
assertIsValidAssigmentToExtract: anAssignmentNode 
	
	self assertIsValidToExtract: anAssignmentNode variable.
	self assertIsValidToExtract: anAssignmentNode value ! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 09:40:26'!
assertIsValidBlockNodeToExtract: aBlockNode 
	
	aBlockNode block statementsDo: [ :aStatement | self assertIsValidToExtract: aStatement ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 20:05:10'!
assertIsValidKeywordForNewParameter: aNewKeyword

	AddParameter assertIsValidKeywordForNewParameter: aNewKeyword! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:09:49'!
assertIsValidLiteralNodeToExtract: aNode

	^ (aNode isLiteralNode 
		or: [ aNode isTruePseudoVariable
		or: [ aNode isFalsePseudoVariable 
		or: [ aNode isNilPseudoVariable ]]]) ifFalse: [ self signalInvalidExpressionToExtractAsParameter ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:04:00'!
assertIsValidMessageNodeToExtract: aNode

	self assertIsValidToExtract: aNode receiver.
	aNode arguments do: [ :anArgument | self assertIsValidToExtract: anArgument ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 19:56:21'!
assertIsValidParameterName: aName

	AddParameter assertIsValidParameterName: aName ! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:09:49'!
assertIsValidTempOrArgNodeToExtract: aTempVariableNode 
	
	aTempVariableNode isDeclaredAtMethodLevel ifTrue: [ self signalInvalidExpressionToExtractAsParameter ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:03:50'!
assertIsValidToExtract: aNode

	aNode isMessageNode ifTrue: [ ^self assertIsValidMessageNodeToExtract: aNode].
	aNode isBlockNode ifTrue: [ ^self assertIsValidBlockNodeToExtract: aNode ].
	aNode isTempOrArg ifTrue: [  ^self assertIsValidTempOrArgNodeToExtract: aNode ].
	aNode isAssignmentToTemporary ifTrue: [ ^self assertIsValidAssigmentToExtract: aNode ].
	self assertIsValidLiteralNodeToExtract: aNode! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 09:28:16'!
is: aRange equalTo: sourceInterval or: intervalToReplace 
	
	"When selecting literals like 1, the range first is one less than the initial character of the literal - Hernan"
	
	^aRange = sourceInterval 
		or: [ aRange = intervalToReplace
		or: [ aRange first + 1 = sourceInterval first and: [ aRange value last = sourceInterval last]]]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:45:11'!
nodeToExtractFrom: aSourceMethod at: trimmedIntervalToReplace or: intervalToReplace
	
	| nodeWithRangeToExtract nodesWithFirstPosition |
	
	nodesWithFirstPosition := aSourceMethod methodNode parseNodesPathAt: trimmedIntervalToReplace first ifAbsent: [ self signalInvalidSelection ].
	nodeWithRangeToExtract := nodesWithFirstPosition 
		detect: [ :nodeAndRange | self is: nodeAndRange value equalTo: trimmedIntervalToReplace or: intervalToReplace ]
		ifNone: [ self signalInvalidSelection ].
	 
	^nodeWithRangeToExtract key.
	! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 15:42:21'!
removeDotsAt: anInterval in: sourceCode
	
	| lastToReplace |
	
	lastToReplace := anInterval last.
	[(sourceCode at: lastToReplace) = $. ] whileTrue: [ lastToReplace := lastToReplace - 1].
	
	^anInterval first to: lastToReplace! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/22/2021 15:55:37'!
named: aNewParameter 
	extractedFrom: anInterval
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFrom: anInterval
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			AddParameter 
				named: aNewParameter 
				initializedWith: sourceCodeToExtract 
				toUnarySelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/22/2021 15:56:38'!
named: aNewParameter 
	extractedFrom: anInterval
	at: aNewKeywordIndex
	newKeyword: newKeyword
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFrom: anInterval
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			AddParameter 
				named: aNewParameter 
				at: aNewKeywordIndex
				initializedWith: sourceCodeToExtract 
				using: newKeyword 
				toKeywordSelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'error messages' stamp: 'HAW 9/22/2021 15:10:01'!
errorMessageForInvalidExpressionToExtractAsParameter
	
	^'Only literals, message sends to literals with literal parameters and 
blocks with the previous conditions can be extracted as parameters'! !

!ExtractAsParameter class methodsFor: 'error messages' stamp: 'HAW 9/21/2021 17:31:17'!
errorMessageForInvalidSelection
	
	^'The selected source code is invalid for extraction as parameter'! !

!ExtractAsParameter class methodsFor: 'error messages' stamp: 'HAW 9/22/2021 15:36:27'!
errorMessageForOrigialMethodMustBeInImplementorsToChange
	
	^'Method with code to extract must be as implementor to change'! !

!ExtractAsParameter class methodsFor: 'exceptions' stamp: 'HAW 9/22/2021 15:10:01'!
signalInvalidExpressionToExtractAsParameter
	
	self refactoringError: self errorMessageForInvalidExpressionToExtractAsParameter ! !

!ExtractAsParameter class methodsFor: 'exceptions' stamp: 'HAW 9/21/2021 17:30:33'!
signalInvalidSelection
	
	self refactoringError: self errorMessageForInvalidSelection! !

!ExtractAsParameter class methodsFor: 'exceptions' stamp: 'HAW 9/22/2021 15:37:55'!
signalOrigialMethodMustBeInImplementorsToChange
	
	self refactoringError: self errorMessageForOrigialMethodMustBeInImplementorsToChange! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 9/22/2021 19:57:44'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor

	^AddParameter addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 9/22/2021 19:59:56'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	^AddParameter addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 9/22/2021 19:59:25'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	^AddParameter addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 9/22/2021 19:58:31'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	^AddParameter addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass
! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 9/22/2021 20:00:22'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem

	^AddParameter addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem
! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 9/22/2021 18:38:54'!
registerExtractAsParameterApplier: anExtractAsParameterApplierClass

	self registerApplierAt: self extractAsParameterApplierId with: anExtractAsParameterApplierClass ! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 9/22/2021 16:35:35'!
extractAsParameterApplier
	
	^self applierAt: self extractAsParameterApplierId ifAbsent: [ ExtractAsParameterApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 9/22/2021 16:35:22'!
extractAsParameterApplierId

	^#extractAsParameterApplier! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 9/22/2021 18:39:16'!
resetExtractAsParameterApplier
	
	self resetApplierAt: self extractAsParameterApplierId! !


!ExtractAsParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/22/2021 16:30:27'!
askNewParameterValue! !

!ExtractAsParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/22/2021 20:06:39'!
createRefactoringForKeywordSelector
	
	^self refactoringClass 
		named: newParameter 
		extractedFrom:  interval
		at:  parameterIndex 
		newKeyword:  newKeyword 
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders ! !

!ExtractAsParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/22/2021 19:36:15'!
createRefactoringForUnarySelector
	
	^self refactoringClass 
		named: newParameter 
		extractedFrom:  interval
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders ! !

!ExtractAsParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/22/2021 16:30:58'!
refactoringClass

	^ExtractAsParameter ! !

!ExtractAsParameterApplier methodsFor: 'initialization' stamp: 'HAW 9/22/2021 19:05:46'!
initializeInterval: anInterval 
	
	interval := anInterval.! !


!ExtractAsParameterApplier class methodsFor: 'instance creation' stamp: 'HAW 9/22/2021 19:03:29'!
from: anInterval on: aModel for: anOldSelector in: aClassToRefactor 

	ExtractAsParameter assertCanExtractedFrom: anInterval for: aClassToRefactor >> anOldSelector.
	
	^(super on: aModel for: anOldSelector in: aClassToRefactor) initializeInterval: anInterval ! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 9/22/2021 19:41:05'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self selectedClassOrMetaClassOrUndefinedObject.
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents asString ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !


!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/22/2021 16:27:52'!
askNewParameterValue
		
	| enteredString |
	
	enteredString := self request: 'Enter parameter value for senders'.
	newParameterValue := enteredString withBlanksTrimmed.
	self refactoringClass assertNewParameterValueIsValid: newParameterValue.
! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 9/22/2021 16:41:33'!
smalltalkEditorRefactoringMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		35.
				#label 			-> 		'Extract as Parameter... (1)'.
				#selector 		-> 		#contextualExtractAsParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		50.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		60.
				#label 			-> 		'Push Up Instance Variable'.
				#selector 		-> 		#contextualPushUpInClassDefinition.
				#icon 			-> 		#goTopIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		70.
				#label 			-> 		'Push Down Instance Variable'.
				#selector 		-> 		#contextualPushDownInClassDefinition.
				#icon 			-> 		#goBottomIcon
		} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 9/22/2021 19:34:01'!
smalltalkEditorCmdShortcutsSpec

	"
	SmalltalkEditor initializeCmdShortcuts
	" 
	^#(
		#($R	#contextualRename:					'Renames what is under cursor')
		#($A	#contextualAddParameter:			'Adds parameter to message that is under cursor')
		#($S	#contextualRemoveParameter:		'Removes parameter to message that is under cursor')
		#($O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable')
		#($J	#extractToTemporary:					'Extracts the selected code into a temporary variable')
		#($K	#extractMethod:						'Extracts the selected code into a separate method')
		#($1 #contextualExtractAsParameter: 'Extracts the selected code as parameter')
	)! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'HAW 9/22/2021 14:33:51'!
assertSourceCodeContainsAValidExpression

	(self intervalCoversCompleteAstNodes 
		and: [ self startAndEndNodesShareAParentNode
			or: [ self intervalMatchesBeginningOfStatement 
				and: [ self intervalMatchesEndOfStatement ]]])
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 9/22/2021 14:55:22'!
intervalMatchesEndOfStatement

	| closerStatementLastPosition |
	
	closerStatementLastPosition := (self findSourceRangeOfCloserStatementIn: finalNodeAncestors) last.
	^ closerStatementLastPosition = intervalToExtract last or: [ closerStatementLastPosition - 1 = intervalToExtract last ].! !

!methodRemoval: ExtractAsParameterApplier class #on:for:in: stamp: 'HAW 9/22/2021 18:50:49'!
ExtractAsParameterApplier class removeSelector: #on:for:in:!
!methodRemoval: ExtractAsParameter class #assertNamed:extractedFrom:at:implementors:senders:creatingAddParameterWith: stamp: 'HAW 9/22/2021 16:00:58'!
ExtractAsParameter class removeSelector: #assertNamed:extractedFrom:at:implementors:senders:creatingAddParameterWith:!
!methodRemoval: ExtractAsParameter class #assertNamed:extractedFrom:at:implementors:senders: stamp: 'HAW 9/22/2021 15:50:00'!
ExtractAsParameter class removeSelector: #assertNamed:extractedFrom:at:implementors:senders:!
!methodRemoval: ExtractAsParameter #addParameter stamp: 'HAW 9/22/2021 20:19:25'!
ExtractAsParameter removeSelector: #addParameter!

!ExtractAsParameter reorganize!
('applying' apply)
('applying - private' applyAddParameter newSourceCode useNewParameter)
('initialization' initializeExtractedFrom:replacing:at:addingParameterWith:)
('selectors' newSelector)
!


!ExtractAsParameter class reorganize!
('pre-conditions' assert:isInImplementors: assertAndCreateNamed:extractedFrom:at:implementors:senders:creatingAddParameterWith: assertCanExtractedFrom:for: assertIsValidAssigmentToExtract: assertIsValidBlockNodeToExtract: assertIsValidKeywordForNewParameter: assertIsValidLiteralNodeToExtract: assertIsValidMessageNodeToExtract: assertIsValidParameterName: assertIsValidTempOrArgNodeToExtract: assertIsValidToExtract: is:equalTo:or: nodeToExtractFrom:at:or: removeDotsAt:in:)
('instance creation' named:extractedFrom:at:implementors:senders: named:extractedFrom:at:newKeyword:at:implementors:senders:)
('error messages' errorMessageForInvalidExpressionToExtractAsParameter errorMessageForInvalidSelection errorMessageForOrigialMethodMustBeInImplementorsToChange)
('exceptions' signalInvalidExpressionToExtractAsParameter signalInvalidSelection signalOrigialMethodMustBeInImplementorsToChange)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inSystem:)
!


!ExtractAsParameterApplier reorganize!
('refactoring - parameters request' askNewParameterValue)
('refactoring - creation' createRefactoringForKeywordSelector createRefactoringForUnarySelector refactoringClass)
('initialization' initializeInterval:)
!


!ExtractAsParameterApplier class reorganize!
('instance creation' from:on:for:in:)
!

