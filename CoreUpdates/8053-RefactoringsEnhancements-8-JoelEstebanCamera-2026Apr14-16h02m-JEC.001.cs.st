'From Cuis7.9 [latest update: #8045] on 3 July 2026 at 3:50:12 pm'!
!classDefinition: #OriginalMethodMethodSet category: #'Tools-Refactoring'!
MethodSet subclass: #OriginalMethodMethodSet
	instanceVariableNames: 'applier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ParameterizedCodeMessageSet category: #'Tools-Refactoring'!
MethodSet subclass: #ParameterizedCodeMessageSet
	instanceVariableNames: 'finder selectedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractParameterizedCodeWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ExtractParameterizedCodeWindow
	instanceVariableNames: 'finder applier refactoringCreationBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer parameterizableNodes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodParametersDetector category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodParametersDetector
	instanceVariableNames: 'intervalToExtract methodNodeToRefactor parameterizableNodes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodProgrammer category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodProgrammer
	instanceVariableNames: 'intervalToExtract methodNode newMessage extractedCodeModifiers compiledMethod newMethodIsUsingAssignmentBlock newMethodIsUsingReturningBlock parameterizableNodes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodParameterizableReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodParameterizableReplacement
	instanceVariableNames: 'nodes nodeInterval parameterizableNodes methodToExtractFrom nodesWithArguments paramReceivers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration newCompiledMethod extractMethodProgrammer parameterizableNodes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage parameterizableReplacements '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractParameterizedCodeStyler category: #'Tools-Refactoring'!
SHTextStylerST80 subclass: #ExtractParameterizedCodeStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!OriginalMethodMethodSet methodsFor: 'shout styling' stamp: 'JEC 10/Feb/2026, 2:23:48 am (UTC)'!
autoHighlightAllOccurrences

	^ false! !

!OriginalMethodMethodSet methodsFor: 'shout styling' stamp: 'JEC 10/Feb/2026, 2:28:20 am (UTC)'!
autoHighlightString

	^ self messageList first sourceCode! !

!OriginalMethodMethodSet methodsFor: 'shout styling' stamp: 'JEC 10/Feb/2026, 2:29:48 am (UTC)'!
shouldAutoHighlight

	^ true ! !

!OriginalMethodMethodSet methodsFor: 'initialization' stamp: 'JEC 10/Feb/2026, 2:23:24 am (UTC)'!
initializeApplier: anApplier

	applier := anApplier! !

!OriginalMethodMethodSet methodsFor: 'source code ranges' stamp: 'JEC 10/Feb/2026, 2:30:28 am (UTC)'!
messageSendsRangesOf: aSelector

	^ Array with: applier intervalToExtract! !


!OriginalMethodMethodSet class methodsFor: 'as yet unclassified' stamp: 'JEC 10/Feb/2026, 2:27:46 am (UTC)'!
for: anApplier

	^ (self messageList: { anApplier methodToExtractCodeFrom }) initializeApplier: anApplier! !


!ParameterizedCodeMessageSet methodsFor: 'source code ranges' stamp: 'JEC 29/Mar/2026, 1:27:08 am (UTC)'!
messageSendsRangesOf: aSelector

	"This is used for highlight the intervals returned in the array"

	| parameterizableReplacement |
	
	parameterizableReplacement := finder parameterizableReplacements 
			at: self messageListIndex 
			ifAbsent: [ ^#()  ].
	
	^ Array with: parameterizableReplacement intervalToExtract! !

!ParameterizedCodeMessageSet methodsFor: 'source code ranges' stamp: 'JEC 29/Mar/2026, 1:31:09 am (UTC)'!
parametrizableNodesRanges

	"This is used for highlight the intervals returned in the array"

	| parameterizableReplacement |
	
	parameterizableReplacement := finder parameterizableReplacements 
			at: self messageListIndex 
			ifAbsent: [ ^#()  ].
	
	^ parameterizableReplacement parameterizableNodesIntervals! !

!ParameterizedCodeMessageSet methodsFor: 'initialization' stamp: 'JEC 10/Feb/2026, 12:19:45 am (UTC)'!
initialize

	selectedIndex := 0.
	shouldAutoHighlight := true.
	super initialize ! !

!ParameterizedCodeMessageSet methodsFor: 'initialization' stamp: 'JEC 9/Feb/2026, 12:41:16 pm (UTC)'!
initializeFinder: aFinder
	
	finder := aFinder.
! !

!ParameterizedCodeMessageSet methodsFor: 'initialization' stamp: 'JEC 9/Feb/2026, 12:41:42 pm (UTC)'!
messageListIndex: anIndex

	selectedIndex := anIndex.
	super messageListIndex: anIndex.! !

!ParameterizedCodeMessageSet methodsFor: 'shout styling' stamp: 'JEC 10/Feb/2026, 12:14:41 am (UTC)'!
autoHighlightAllOccurrences

	^ false! !

!ParameterizedCodeMessageSet methodsFor: 'shout styling' stamp: 'JEC 1/Jun/2026, 9:03:26 pm (UTC)'!
autoHighlightString

	^ finder hasFoundParameterizableReplacements 
		ifTrue: [ (finder parameterizableReplacementsAt: selectedIndex) sourceCode ]
		ifFalse: [ '' ]! !

!ParameterizedCodeMessageSet methodsFor: 'message list' stamp: 'JEC 9/Feb/2026, 12:41:31 pm (UTC)'!
messageListIndex

	^selectedIndex ! !

!ParameterizedCodeMessageSet methodsFor: 'message list' stamp: 'JEC 15/Apr/2026, 3:17:58 pm (UTC)'!
removeMessageFromBrowserKeepingLabel

	| newIndex |

	selectedMessage ifNil: [ ^nil ].
	messageList removeIndex: selectedIndex.
	finder removeParameterizableReplacementAt: selectedIndex.
	self changed: #messageList.
	
	newIndex := selectedIndex > messageList size 
		ifTrue: [ selectedIndex - 1 ]
		ifFalse: [ selectedIndex ].
	self messageListIndex: newIndex.! !

!ParameterizedCodeMessageSet methodsFor: 'user interface support' stamp: 'JEC 29/Mar/2026, 1:29:32 am (UTC)'!
textStylerClassFor: textGetter

	(Preferences at: #syntaxHighlightingAsYouType) 
		ifFalse: [ ^nil ].

	^ ExtractParameterizedCodeStyler! !


!ParameterizedCodeMessageSet class methodsFor: 'instance creation' stamp: 'JEC 9/Feb/2026, 11:49:22 pm (UTC)'!
finder: aFinder

	| methods |
	
	methods := aFinder parameterizableReplacements collect: [ :parameterizedNode | 
		parameterizedNode methodToExtractFrom methodReference ].

	^ (self messageList: methods) initializeFinder: aFinder! !


!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 12:49:27 am (UTC)'!
addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 25/Jun/2026, 11:56:13 pm (UTC)'!
addButtonsTo: row color: buttonColor

	self 
		addButton: self createRemoveButton to: row color: buttonColor;
		addButton: self createRefactorButton to: row color: buttonColor;
		addButton: self createSkipButton to: row color: buttonColor;
		addButton: self createCancelButton to: row color: buttonColor.
! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 12:34:47 pm (UTC)'!
buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0;
		addAdjusterMorph;
		addMorph: self buildMorphicOriginalCodePane proportionalHeight: 0.5.
	
	^codeAndButtons ! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 3:05:59 am (UTC)'!
buildMorphicOriginalCodePane

	| originalCodePane  |
	
	originalCodePane := LinearLayoutMorph newColumn.
	originalCodePane
		addMorph: self buildOriginalCodeTitle layoutSpec: (LayoutSizeSpec proportionalWidth: 1 fixedHeight: 0);
		addMorph: self buildOriginalCodePane proportionalHeight: 1.
	
	^originalCodePane ! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 2:27:02 am (UTC)'!
buildOriginalCodePane

	^ (TextModelMorph
		textProvider: (OriginalMethodMethodSet for: applier)
		textGetter: #acceptedContents)
		emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)';
		disableEditing;
		yourself! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 12:29:18 pm (UTC)'!
buildOriginalCodeTitle

	| labelMorph |

	labelMorph := (LabelMorph contents: 'Original Code')
		color: Theme current windowLabel darker.

	^ LinearLayoutMorph newRow
		addMorph: labelMorph;
		axisEdgeWeight: 0.5;
		doAdoptWidgetsColor;
		color: self windowColor;
		borderWidth: 2;
		borderColor: self windowColor darker.! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 12:48:48 am (UTC)'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^ row! !

!ExtractParameterizedCodeWindow methodsFor: 'GUI building' stamp: 'JEC 10/Feb/2026, 12:38:03 pm (UTC)'!
createCodePaneMorph

	^super createCodePaneMorph
		disableEditing;
		yourself! !

!ExtractParameterizedCodeWindow methodsFor: 'button creation' stamp: 'JEC 10/Feb/2026, 12:53:04 am (UTC)'!
createCancelButton
	
	^RefactoringApplier createCancelButtonOn: self! !

!ExtractParameterizedCodeWindow methodsFor: 'button creation' stamp: 'JEC 25/Jun/2026, 11:53:09 pm (UTC)'!
createRefactorButton

	^ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #acceptToParameterizeCode
		label: 'Refactor'
		balloonText: 'Replace the highlighted code with a parameter, allowing all shown occurrences to be extracted'! !

!ExtractParameterizedCodeWindow methodsFor: 'button creation' stamp: 'JEC 15/Apr/2026, 2:36:25 pm (UTC)'!
createRemoveButton
	
	^RefactoringApplier createRemoveButtonOn: self! !

!ExtractParameterizedCodeWindow methodsFor: 'button creation' stamp: 'JEC 26/Jun/2026, 9:07:35 pm (UTC)'!
createSkipButton

	^ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #doNotParameterize
		label: 'Skip Parameterizing'
		balloonText: 'Extract the method without parametrizing the highlighted code'! !

!ExtractParameterizedCodeWindow methodsFor: 'actions' stamp: 'JEC 31/May/2026, 8:19:59 pm (UTC)'!
acceptToParameterizeCode

	self closeBefore: [ applier valueAcceptingParameterizableCodeHandlingExceptions ]! !

!ExtractParameterizedCodeWindow methodsFor: 'actions' stamp: 'JEC 26/Mar/2026, 12:01:08 am (UTC)'!
closeBefore: aBlock

	self whenUIinSafeState: [ 
		self delete.
		aBlock value ]! !

!ExtractParameterizedCodeWindow methodsFor: 'actions' stamp: 'JEC 25/Jun/2026, 11:55:46 pm (UTC)'!
doNotParameterize

	self closeBefore: [ applier valueAcceptingOnlyEquivalentsHandlingExceptions ].! !

!ExtractParameterizedCodeWindow methodsFor: 'actions' stamp: 'JEC 15/Apr/2026, 2:36:34 pm (UTC)'!
remove
	
	model removeMessageFromBrowserKeepingLabel! !

!ExtractParameterizedCodeWindow methodsFor: 'initialization' stamp: 'JEC 10/Feb/2026, 1:03:51 pm (UTC)'!
initializeFrom: anExtractMethodApplier with: aFinder using: aRefactoringCreationBlock

	applier := anExtractMethodApplier.
	finder := aFinder.
	refactoringCreationBlock := aRefactoringCreationBlock.! !


!ExtractParameterizedCodeWindow class methodsFor: 'title' stamp: 'JEC 10/Feb/2026, 1:15:36 am (UTC)'!
windowTitle

	^ 'Extract Parameters to Replace Duplicates'! !

!ExtractParameterizedCodeWindow class methodsFor: 'instance creation' stamp: 'JEC 10/Feb/2026, 1:03:31 pm (UTC)'!
openFrom: anExtractMethodApplier with: aFinder using: aRefactoringCreationBlock

	| window messageSet |

	messageSet := ParameterizedCodeMessageSet finder: aFinder.
	
	window := self new.
	window
		model: messageSet;
		setLabel: self windowTitle;
		initializeFrom: anExtractMethodApplier with: aFinder using: aRefactoringCreationBlock;
		buildMorphicWindow.
	
	"I need to do this to make the parameterizable code highlighted"
	messageSet messageListIndex: 1.
	
	window openInWorld.
	
	^ window! !


!ExtractMethodCodeModifier methodsFor: 'accessing' stamp: 'JEC 26/Jun/2026, 12:06:33 am (UTC)'!
newMessage: aNewMessage 

	newMessage := aNewMessage! !


!ExtractMethodNewMethodSourceCode methodsFor: 'initialization' stamp: 'JEC 14/Apr/2026, 8:40:12 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withArgumentNames: renamedArguments  withParameterizableNodes: aCollectionOfParameterizableNodes

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage copy.
	newArgumentNames := renamedArguments.
	parameterizableNodes := aCollectionOfParameterizableNodes.
	
	extractMethodProgrammer := ExtractMethodProgrammer 
		from: intervalToExtract 
		of: existingMethod 
		to: newMessage
		withParameterizableNodes: aCollectionOfParameterizableNodes! !


!ExtractMethodNewMethodSourceCode class methodsFor: 'instance creation' stamp: 'JEC 14/Apr/2026, 8:41:17 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes! !


!ExtractMethodParametersDetector methodsFor: 'initialization' stamp: 'JEC 14/Apr/2026, 8:34:02 pm (UTC)'!
initializeFor: aMethodNodeToRefactor at: anIntervalToExtract with: aCollectionOfParameterizableNodes

	methodNodeToRefactor := aMethodNodeToRefactor.
	intervalToExtract := anIntervalToExtract.
	parameterizableNodes := aCollectionOfParameterizableNodes.! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 31/May/2026, 7:00:23 pm (UTC)'!
isAParameterizableNode: parseNode 

	^ parameterizableNodes 
		anySatisfy: [ :parameterizableNode | parameterizableNode key equivalentTo: parseNode]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 31/May/2026, 7:04:30 pm (UTC)'!
isIncludedInAnyParameterizableNode: parseNode 

	parameterizableNodes ifEmpty: [ ^ false ].
	
	^ parameterizableNodes anySatisfy: [ :paramNode | paramNode key nodeIncludes: parseNode ]! !


!ExtractMethodParametersDetector class methodsFor: 'instance creation' stamp: 'JEC 14/Apr/2026, 8:33:30 pm (UTC)'!
for: aMethodNodeToRefactor at: anIntervalToExtract with: aCollectionOfParameterizableNodes

	^ self new 
		initializeFor: aMethodNodeToRefactor 
		at: anIntervalToExtract 
		with: aCollectionOfParameterizableNodes! !

!ExtractMethodParametersDetector class methodsFor: 'evaluating' stamp: 'JEC 14/Apr/2026, 8:33:38 pm (UTC)'!
valueFor: aMethodNodeToRefactor at: anIntervalToExtract with: parameterizableNodes

	^ (self for: aMethodNodeToRefactor at: anIntervalToExtract with: parameterizableNodes)
		value! !


!ExtractMethodProgrammer methodsFor: 'new message' stamp: 'JEC 26/Jun/2026, 12:04:45 am (UTC)'!
newMessage: aNewMessage 

	newMessage := aNewMessage.
	extractedCodeModifiers do: [ :modifier | modifier newMessage: aNewMessage ].! !

!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'JEC 10/Mar/2026, 10:43:50 pm (UTC)'!
addExtractMethodCodeModifierIfNecessary: returnNodeWithSourceRanges 

	| returnNode sourceRanges renamedVarsReturnNode |
		
	returnNode := returnNodeWithSourceRanges key.
	
	"If the return is part of a parameterizable code, is not going to be extracted. So is ignored."
	sourceRanges := returnNodeWithSourceRanges value
		select: [ :sourceRange | parameterizableNodes noneSatisfy: [ :paramNode | paramNode value includesAllOf: sourceRange ]].
	
	"If we don't do this with a veryDeepCopy, it will rename all the nodes in the method node also"
	renamedVarsReturnNode := self renameTempVarsIfNeeded: returnNode veryDeepCopy.
		
	sourceRanges withIndexDo: [ :sourceRange :index |
		extractedCodeModifiers add:
		(ExtractMethodReturnCodeModifier
			for: returnNode 
			from: intervalToExtract 
			of: methodNode 
			to: newMessage
			sourceRange: sourceRange
			renamedVarsNode: renamedVarsReturnNode) ]! !

!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'JEC 14/Apr/2026, 8:37:32 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withParameterizableNodes: aCollectionOfParameterizableNodes

	intervalToExtract := anIntervalToExtract.
	compiledMethod := aMethodToExtractCodeFrom.
	methodNode := aMethodToExtractCodeFrom methodNode.
	newMessage := aNewMessage copy.
	parameterizableNodes := aCollectionOfParameterizableNodes.
	newMethodIsUsingAssignmentBlock := false.
	newMethodIsUsingReturningBlock := false.
	
	self initializeExtractedModifiers.! !


!ExtractMethodProgrammer class methodsFor: 'instance creation' stamp: 'JEC 14/Apr/2026, 8:35:33 pm (UTC)'!
from: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withParameterizableNodes: aCollectionOfParameterizableNodes

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom 
		to: aNewMessage
		withParameterizableNodes: aCollectionOfParameterizableNodes! !


!ExtractMethod methodsFor: 'accessing' stamp: 'JEC 25/Mar/2026, 11:13:39 pm (UTC)'!
replacements

	^ collectionOfReplacements! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'JEC 1/Jun/2026, 7:36:05 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments withParameterizableNodes: parameterizableNodes 

	| trimmedIntervalToExtract newSourceCodeGenerator |

	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage 
			hasValidParametersForExtracting: anIntervalToExtract 
			from: aMethodToExtractCodeFrom notOptimizedMethodNode
			with: parameterizableNodes;
		assert: parameterizableNodes areValidAt: trimmedIntervalToExtract in: aMethodToExtractCodeFrom.

	newSourceCodeGenerator := (ExtractMethodNewMethodSourceCode
		fromInterval: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
		withParameterizableNodes: parameterizableNodes).

	^ self new
		initializeOf: aMethodToExtractCodeFrom
		in: aCategory
		newSourceCodeFrom: newSourceCodeGenerator! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'JEC 9/Mar/2026, 10:12:51 pm (UTC)'!
assert: parameterizableNodes areValidAt: intervalToExtract in: aMethodToExtractCodeFrom 
	
	(parameterizableNodes allSatisfy: [ :nodeWithRange | intervalToExtract includesAllOf: nodeWithRange value ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'JEC 28/Mar/2026, 11:58:20 pm (UTC)'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor with: parameterizableNodes 

	| parseNodesToParameterize |
	
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract
		with: parameterizableNodes.
	
	(newMessage hasRightNumberOfArguments 
		and: [ newMessage arguments size = parseNodesToParameterize size ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !


!ExtractMethodParameterizableReplacement methodsFor: 'printing' stamp: 'JEC 17/Mar/2026, 11:31:05 pm (UTC)'!
sourceCode

	"This method is needed for highlighting in the ExtractParameterizedCodeWindow"

	^  methodToExtractFrom sourceCode 
		copyFrom: nodeInterval first 
		to: nodeInterval last! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 9/Apr/2026, 1:03:10 am (UTC)'!
anyParameterContainsReturnOrAssignment

	parameterizableNodes do: [ :paramNode |
		paramNode key nodesDo: [ :aNode |
			(aNode isReturn or: [ aNode isAssignmentNode ])
				ifTrue: [ ^true ] ] ].

	^ false! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 3:18:28 pm (UTC)'!
canBeUsedForNaming

	^ paramReceivers
		detect: [ :nodeWithParamReceiver |
			| paramReceiver |
			paramReceiver := nodeWithParamReceiver value.
			
			paramReceiver notNil 
				and: [ paramReceiver key isTemp 
					or: [ paramReceiver key isVariableNode and: [ paramReceiver key isUndeclared ] ] ] ]
		ifFound: [ :found | true ]
		ifNone: [ false ] ! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 9/Mar/2026, 12:31:09 am (UTC)'!
hasIntersectionWith: anotherParamReplacement

	^ (self isFromSameMethodWith: anotherParamReplacement)
		and: [ self intersectsIntervalWith: anotherParamReplacement intervalToExtract ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 30/Mar/2026, 2:18:16 am (UTC)'!
hasParameterizableIntervalsInOrder: aCollectionOfParamIntervals 

	^ self parameterizableNodesIntervals 
		with: aCollectionOfParamIntervals
		allSatisfy: [ :paramNodeInterval :anotherParamNodeInterval |
			paramNodeInterval = anotherParamNodeInterval ] ! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 12/Feb/2026, 3:15:47 am (UTC)'!
hasSameIntervalWith: anotherParamReplacement

	^ self isIntervalToExtract: anotherParamReplacement intervalToExtract! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 9/Mar/2026, 12:31:42 am (UTC)'!
intersectsIntervalWith: anInterval

	^ nodeInterval intersectsRange: anInterval! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 31/Mar/2026, 1:31:30 am (UTC)'!
isFromSameMethodWith: anotherParameterizableReplacement

	| anotherMethod |

	"This is because the CompileMethod equality does not check the methods selectors"
	
	anotherMethod := anotherParameterizableReplacement methodToExtractFrom.

	^ (methodToExtractFrom selector = anotherMethod selector) 
		and: [ methodToExtractFrom = anotherMethod ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 6/Feb/2026, 12:04:28 am (UTC)'!
isIntervalToExtract: anInterval

	^ nodeInterval = anInterval! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 31/Mar/2026, 1:37:12 am (UTC)'!
isOf: aMethod

	^ (methodToExtractFrom selector = aMethod methodSymbol)
		and: [ methodToExtractFrom = aMethod compiledMethod ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 14/Mar/2026, 10:40:11 pm (UTC)'!
isParameterizableParseNode: aParseNode

	^ parameterizableNodes anySatisfy: [ :paramParseNode |
		(paramParseNode key equivalentTo: aParseNode key) 
			and: [ 
				"This happens because the argument source range is not precise"
				(paramParseNode value intersection: aParseNode value) notEmpty ] ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 31/May/2026, 6:49:06 pm (UTC)'!
parameterNodeIncludes: aParseNode 

	^ parameterizableNodes anySatisfy: [ :paramNode | paramNode key nodeIncludes: aParseNode ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'JEC 8/Mar/2026, 10:53:55 pm (UTC)'!
representSamePieceOfCodeWith: anotherParameterizableReplacement

	^ (self isFromSameMethodWith: anotherParameterizableReplacement) 
		and: [ self hasSameIntervalWith: anotherParameterizableReplacement]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 26/Mar/2026, 1:06:04 am (UTC)'!
allArgumentNodesOf: aNode

	| messageNode |

	messageNode := aNode isMessageNode
		ifTrue: [ aNode ]
		ifFalse: [ aNode value ]. "Here the node is an assignment node and the value assigning is a message node"

	^ messageNode argumentNodesSourceRangesIn: methodToExtractFrom methodNode! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 6/Apr/2026, 10:25:38 pm (UTC)'!
hasAParamReceiver: aNode ifTrue: aTrueBlock ifFalse: aFalseBlock 

	| receiverNode |

	(aNode isMessageNode or: [ aNode isAssignmentNode ]) not
		ifTrue: [ ^ aFalseBlock value ].
	
	receiverNode := self receiverNodeOf: aNode.
	
	^ parameterizableNodes 
		detect: [ :aParamNode | (aParamNode key equivalentTo: receiverNode key) 
			and: [ aParamNode value = receiverNode value ] ]
		ifFound: aTrueBlock
		ifNone: aFalseBlock! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 16/Mar/2026, 10:42:34 pm (UTC)'!
isKeywordMessageSendNode: aNode

	^ aNode isMessageNode and: [ aNode isKeywordMessageSend ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 6/Apr/2026, 10:06:18 pm (UTC)'!
messageNodeOf: aNode

	^ aNode isMessageNode
		ifTrue: [ aNode ]
		ifFalse: [ aNode value ]. "Here the node is an assignment node and the value assigning is a message node"! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 6/Apr/2026, 10:25:53 pm (UTC)'!
receiverNodeOf: aNode

	^ (self messageNodeOf: aNode) 
		receiverNodeSourceRangesIn: methodToExtractFrom methodNode! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 16/Mar/2026, 10:43:07 pm (UTC)'!
shouldAddArgumentsOf: aNode
	
	^ (self isKeywordMessageSendNode: aNode)
		or: [ aNode isAssignmentNode and: [ self isKeywordMessageSendNode: aNode value ] ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 6/Apr/2026, 10:44:34 pm (UTC)'!
sortBlockByRange

	^ [ :leftParseNode :rightParseNode | leftParseNode value first < rightParseNode value first ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'JEC 27/Mar/2026, 1:47:16 am (UTC)'!
with: argNodes with: anotherArgNodes detectAll: aBlock ifFound: aFoundBlock

	argNodes
		with: anotherArgNodes
		do: [ :argNode :anotherArgNode |
			(aBlock value: argNode value: anotherArgNode)
				ifTrue: [ aFoundBlock value: argNode value: anotherArgNode ] ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 15/Apr/2026, 3:39:53 pm (UTC)'!
allArgumentsToParameterizeInOrderUsing: parseNodesToParameterize 

	| argumentsToParameterize |
	
	argumentsToParameterize := SortedCollection sortBlock: self sortBlockByRange.
	
	paramReceivers do: [ :paramReceiver |
		paramReceiver value ifNotNil: [ argumentsToParameterize add: paramReceiver value ] ].

	nodesWithArguments do: [ :nodeWithArgs |
		nodeWithArgs value do: [ :argNode |
			((self isParameterizableParseNode: argNode)
			or: [ parseNodesToParameterize anySatisfy: [ :parseNode | parseNode equivalentTo: argNode key ] ])
				ifTrue: [ argumentsToParameterize add: argNode ] ] ].
	
	argumentsToParameterize := argumentsToParameterize collect: [ :argToParam |
		| argString |
		
		argString := methodToExtractFrom sourceCode
			copyFrom: argToParam value first 
			to: argToParam value last.
		
		argToParam key -> argString ].

	^ argumentsToParameterize! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 6/Feb/2026, 12:01:50 am (UTC)'!
intervalToExtract

	^ nodeInterval! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 9/Feb/2026, 12:38:35 pm (UTC)'!
methodToExtractFrom

	^ methodToExtractFrom! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 17/Mar/2026, 11:28:37 pm (UTC)'!
nodesWithArguments

	^ nodesWithArguments! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 6/Apr/2026, 10:19:27 pm (UTC)'!
paramReceivers

	^ paramReceivers! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 10/Feb/2026, 11:14:45 pm (UTC)'!
parameterizableNodes

	^ parameterizableNodes collect: [ :parameterizableNode | parameterizableNode key ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 29/Mar/2026, 1:19:37 am (UTC)'!
parameterizableNodesIntervals

	^ parameterizableNodes collect: [ :parameterizableNode | parameterizableNode value ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 6/Apr/2026, 10:44:17 pm (UTC)'!
parameterizableNodesSortedByInterval

	^ parameterizableNodes asSortedCollection: self sortBlockByRange.! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'JEC 11/Feb/2026, 11:21:09 am (UTC)'!
parameterizableNodesWithInterval

	^ parameterizableNodes! !

!ExtractMethodParameterizableReplacement methodsFor: 'initialization' stamp: 'JEC 6/Apr/2026, 10:16:06 pm (UTC)'!
initializeFor: aCollectionOfNodes interval: aNodeInterval parameterizableNodes: aCollectionOfParameterizableNodes in: aMethod 

	nodes := aCollectionOfNodes.
	nodeInterval := aNodeInterval.
	parameterizableNodes := aCollectionOfParameterizableNodes.
	methodToExtractFrom := aMethod.

	self initializeNodesWithArguments.
	self initializeParamReceivers.! !

!ExtractMethodParameterizableReplacement methodsFor: 'initialization' stamp: 'JEC 17/Mar/2026, 11:28:15 pm (UTC)'!
initializeNodesWithArguments

	nodesWithArguments := OrderedCollection new.

	nodes do: [ :aNode |
		(self shouldAddArgumentsOf: aNode)
			ifTrue: [ nodesWithArguments add: aNode -> (self allArgumentNodesOf: aNode) ] ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'initialization' stamp: 'JEC 6/Apr/2026, 10:03:49 pm (UTC)'!
initializeParamReceivers

	paramReceivers := nodes collect: [ :aNode | 
		self 
			hasAParamReceiver: aNode
			ifTrue: [ :aReceiverNode | aNode -> aReceiverNode ]
			ifFalse: [ aNode -> nil ] ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'converting' stamp: 'JEC 6/Apr/2026, 10:33:44 pm (UTC)'!
createWithSameParametersAs: anotherParameterizableReplacement

	| newParameterizableNodes |

	(self representSamePieceOfCodeWith: anotherParameterizableReplacement)
		ifTrue: [ ^ self ].
	
	newParameterizableNodes := self parameterizableNodesSortedByInterval.
	
	paramReceivers
		with: anotherParameterizableReplacement paramReceivers
		do: [ :nodeWithParamReceiver :anotherNodeWithParamReceiver |
			(nodeWithParamReceiver value isNil and: [ anotherNodeWithParamReceiver value notNil ])
				ifTrue: [ newParameterizableNodes add: (self receiverNodeOf: nodeWithParamReceiver key) ] ].

	nodesWithArguments 
		with: anotherParameterizableReplacement nodesWithArguments 
		do: [ :nodeWithArgs :anotherNodeWithArgs |
			self 
				with: nodeWithArgs value
				with: anotherNodeWithArgs value
				detectAll: [ :argNode :anotherArgNode |
					(argNode key equivalentTo: anotherArgNode key) not
					and: [newParameterizableNodes 
						noneSatisfy: [ :oldParamNode | argNode value intersectsRange: oldParamNode value ] ] ]
				ifFound: [ :argNode :anotherArgNode | newParameterizableNodes add: argNode ] ].
	
	^ self class 
		for: nodes 
		interval: nodeInterval 
		parameterizableNodes: newParameterizableNodes
		in: methodToExtractFrom! !


!ExtractMethodParameterizableReplacement class methodsFor: 'instance creation' stamp: 'JEC 4/Mar/2026, 12:47:24 pm (UTC)'!
for: aCollectionOfNodes interval: nodeInterval parameterizableNodes: parameterizableNodes in: aMethod 

	^ self new 
		initializeFor: aCollectionOfNodes 
		interval: nodeInterval 
		parameterizableNodes: parameterizableNodes 
		in: aMethod! !


!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'JEC 14/Apr/2026, 8:31:32 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withParameterizableNodes: aCollectionOfParameterizableNodes

	intervalToExtract := anIntervalToExtract.
	methodToExtractFrom := aMethodToExtractCodeFrom.
	newMessage := aNewMessage copy.
	parameterizableNodes := aCollectionOfParameterizableNodes.

	extractMethodProgrammer := ExtractMethodProgrammer 
		from: intervalToExtract 
		of: methodToExtractFrom 
		to: aNewMessage
		withParameterizableNodes: aCollectionOfParameterizableNodes.
! !

!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'JEC 9/Mar/2026, 12:57:13 am (UTC)'!
asParameterizableReplacement

	| nodesToExtract |

	nodesToExtract := (NodesInRangeFinder 
		in: methodToExtractFrom compiledMethod 
		for: intervalToExtract) value.
	
	^ ExtractMethodParameterizableReplacement
		for: nodesToExtract
		interval: intervalToExtract
		parameterizableNodes: #()
		in: methodToExtractFrom compiledMethod.! !

!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'JEC 31/May/2026, 8:43:16 pm (UTC)'!
newMessage: aNewMessage 

	newMessage := aNewMessage.
	extractMethodProgrammer newMessage: aNewMessage.! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'JEC 11/Mar/2026, 1:48:55 am (UTC)'!
areParameterizableNodesIntervals: aCollectionOfParamIntervals 

	^ (parameterizableNodes collect: [ :paramNode | paramNode value ])
		with: aCollectionOfParamIntervals 
		allSatisfy: [ :paramInterval :anotherInterval | paramInterval = anotherInterval]! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'JEC 3/Mar/2026, 11:19:29 pm (UTC)'!
hasEqualParams: aCollectionOfParams

	^ newMessage hasEqualArguments: aCollectionOfParams! !


!ExtractMethodReplacement class methodsFor: 'instance creation' stamp: 'JEC 27/Mar/2026, 1:52:33 am (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withParameterizableNodes: aCollectionOfParameterizableNodes

	| trimmedIntervalToExtract |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.
	self 
		assert: aCollectionOfParameterizableNodes 
		areValidAt: trimmedIntervalToExtract 
		in: aMethodToExtractCodeFrom.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withParameterizableNodes: aCollectionOfParameterizableNodes
		! !

!ExtractMethodReplacement class methodsFor: 'pre-conditions' stamp: 'JEC 11/Mar/2026, 12:26:33 am (UTC)'!
assert: parameterizableNodes areValidAt: intervalToExtract in: aMethodToExtractCodeFrom 
	
	ExtractMethodNewMethod 
		assert: parameterizableNodes 
		areValidAt: intervalToExtract 
		in: aMethodToExtractCodeFrom! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'JEC 14/Mar/2026, 12:20:41 am (UTC)'!
initializeNewMessageArguments

	^ newMessageArguments := OrderedDictionary new! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 1/Jun/2026, 7:48:27 pm (UTC)'!
createExtractMethodNewMethodFor: newMessage using: sourceCodeParameterizableReplacement 

	| parameterizableNodes replacementIntervalToExtract replacementMethodToExtractFrom |

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

	^ newMethodRefactoring := ExtractMethodNewMethod 
		fromInterval: replacementIntervalToExtract
		of: replacementMethodToExtractFrom
		to: newMessage
		categorizedAs: replacementMethodToExtractFrom category 
		withArgumentNames: renamedNewMessageArguments
		withParameterizableNodes: parameterizableNodes! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 1/Jun/2026, 7:57:00 pm (UTC)'!
findReplacements

	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: methodToExtractCodeFrom.
	finder value! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 31/May/2026, 5:04:47 pm (UTC)'!
requestNewMessageFor: parseNodesToParameterize usingForSaving: argumentsForSaving

	| caption initialAnswer userAnswer |

	initialAnswer := self buildInitialSelectorAnswer: argumentsForSaving.
	caption := self selectorCaption: parseNodesToParameterize.
	userAnswer := self request: caption initialAnswer: initialAnswer.
	self saveSelectorFrom: userAnswer for: argumentsForSaving.

	^self buildNewMessage.! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 7/Apr/2026, 4:16:41 pm (UTC)'!
requestNewMessageWithAllParameterizedValues

	| parseNodesToParameterized renameMeArguments |

	self initializeNewMessageArguments.
	
	parseNodesToParameterized := self parseAndParameterizableNodesToParameterizeInOrder.
	
	renameMeArguments := parseNodesToParameterized withIndexCollect: [ :node :index | 
		node isVariableNode
			ifTrue: [ node ]
			ifFalse: [ VariableNode named: 'aRenameMe' , index asString ] ].

	^ self 
		requestNewMessageFor: parseNodesToParameterized 
		usingForSaving: renameMeArguments.! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 1/Jun/2026, 8:10:39 pm (UTC)'!
valueAcceptingOnlyEquivalents

	| newMessage |

	newMessage := self requestNewMessage.
	self createExtractMethodNewMethodFor: newMessage.
	finder newMessageForReplacements: newMessage.
	self valueApplyingRefactoringWith: [ self createRefactoringWithAllReplacements ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 1/Jun/2026, 8:10:39 pm (UTC)'!
valueAcceptingOnlyEquivalentsHandlingExceptions

	requestExitBlock := [ ^self ].

	self valueHandlingRefactoringExceptions: [ self valueAcceptingOnlyEquivalents ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 1/Jun/2026, 7:41:13 pm (UTC)'!
valueAcceptingParameterizableCode

	| newMessage |

	finder useParameterizedToChangeReplacements.

	"All parameterizable replacements here has the same number of parameters"
	newMessage := self requestNewMessageWithAllParameterizedValues.

	finder 
		acceptParamReplacementsWithNodesToParameterized: self notParameterizableParseNodesToParameterize
		to: newMessage.
	
	self 
		createExtractMethodNewMethodFor: newMessage
		using: (finder extractedCodeOrAnyParameterizableReplacement).

	self valueApplyingRefactoringWith: [ self createRefactoringWithAllReplacements ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 31/May/2026, 8:20:17 pm (UTC)'!
valueAcceptingParameterizableCodeHandlingExceptions

	requestExitBlock := [ ^self ].

	self valueHandlingRefactoringExceptions: [ self valueAcceptingParameterizableCode ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 10/Feb/2026, 12:57:50 pm (UTC)'!
valueApplyingRefactoringWith: aRefactoringCreationBlock

	self 
		createAndSetRefactoringHandlingRefactoringExceptions: aRefactoringCreationBlock;
		applyRefactoring;
		showChanges! !

!ExtractMethodApplier methodsFor: 'private - parse nodes to parameterized' stamp: 'JEC 31/May/2026, 7:20:19 pm (UTC)'!
notParameterizableParseNodesToParameterize

	^ self parseNodesToParameterize reject: [ :parseNode | 
		finder anyParameterizableNodeIncludes: parseNode at: intervalToExtract in: methodToExtractCodeFrom ]! !

!ExtractMethodApplier methodsFor: 'private - parse nodes to parameterized' stamp: 'JEC 7/Apr/2026, 3:03:57 pm (UTC)'!
parseAndParameterizableNodesToParameterizeInOrder

	| paramReplacementForNaming |
	
	paramReplacementForNaming := finder parameterizableReplacements
		detect: [ :paramReplacement | paramReplacement canBeUsedForNaming ]
		ifFound: [ :paramReplacement | paramReplacement ]
		ifNone: [ finder parameterizableReplacements first ].
	
	^ (paramReplacementForNaming
		allArgumentsToParameterizeInOrderUsing: self parseNodesToParameterize)
		collect: [ :argParseNode | argParseNode key ]! !

!ExtractMethodApplier methodsFor: 'accessing' stamp: 'JEC 10/Feb/2026, 1:20:24 am (UTC)'!
intervalToExtract

	^ intervalToExtract! !

!ExtractMethodApplier methodsFor: 'accessing' stamp: 'JEC 10/Feb/2026, 1:18:25 am (UTC)'!
methodToExtractCodeFrom

	^ methodToExtractCodeFrom! !


!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 7/Mar/2026, 1:01:29 am (UTC)'!
addReplacementFrom: aCreationBlock
	
	"If there is an error creating the refactoring, then the found text is not extractable and 
	therefore should not be replaced - Hernan"
	[ replacements add: aCreationBlock value ] 
		on: RefactoringError 
		do: [ :anError | ].! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 5/Feb/2026, 12:38:03 am (UTC)'!
defineFindingByEquivalenceStrategy
	
	| useStringEquality |
	
	"Sadly, finding the right ranges when extracting literal nodes is almost imposible becuase literal nodes can have many
	ranges. An alternative algorithm could be to create an 'expanded AST' without reused nodes and each node with its own
	source range. Nodes of that AST could be a polimorphic proxy of the real node, adding only the source range, something
	to try ...
	For the moment, if the selection to extract has only literal nodes, uses string comparison - Hernan"
	useStringEquality := nodesToExtract allSatisfy: [ :aNode | aNode isLiteralNode ].
	
	^useStringEquality 
		ifTrue: [ [ :aMethod | self findReplacementsUsingStringEqualityIn: aMethod ]]
		ifFalse: [ [ :aMethod | self findReplacementsUsingNodeEquivalenceIn: aMethod ]]! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 31/Mar/2026, 12:38:35 am (UTC)'!
isSourceMethod: aMethod

	"This is because two methods have equal CompileMethod when their source code are equal, 
	ignoring their selectors are different"

	^ (sourceMethod methodSymbol = aMethod selector) 
		and: [sourceMethod compiledMethod = aMethod]! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 5/Feb/2026, 12:43:44 am (UTC)'!
useFindingByEquivalenceStrategy

	findingStrategy := self defineFindingByEquivalenceStrategy! !


!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 11/Mar/2026, 12:41:40 am (UTC)'!
addReplacementAt: foundIntervalToExtract 
	in: aMethod 
	with: aNewMessage 
	paramNodes: aCollectionOfParamNodes

	self addReplacementFrom: [ 
		self 
			createReplacementAt: foundIntervalToExtract 
			in: aMethod 
			with: aNewMessage
			paramNodes: aCollectionOfParamNodes ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 11/Mar/2026, 12:43:18 am (UTC)'!
createReplacementAt: foundIntervalToExtract 
	in: aMethod 
	with: aNewMessage 
	paramNodes: aCollectionOfParamNodes
	
	^ ExtractMethodReplacement 
		fromInterval: foundIntervalToExtract asSourceCodeInterval 
		of: aMethod asMethodReference 
		to: aNewMessage
		withParameterizableNodes: aCollectionOfParamNodes! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 30/Mar/2026, 2:00:38 am (UTC)'!
replacementsIntervalsOf: aMethod 

	^ replacements 
		select: [ :replacement | replacement isOf: aMethod methodReference ] 
		thenCollect: [ :replacement | replacement intervalToExtract ]! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 31/May/2026, 8:36:55 pm (UTC)'!
newMessageForReplacements: aNewMessage 

	replacements do: [ :aReplacement | aReplacement newMessage: aNewMessage ]! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 25/Mar/2026, 11:17:07 pm (UTC)'!
useReplacements: aCollectionOfReplacements 

	replacements := aCollectionOfReplacements ! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 15/Apr/2026, 3:40:36 pm (UTC)'!
acceptParamReplacementsWithNodesToParameterized: nodesToBeParameterized to: aNewMessage 

	replacements := OrderedCollection new.

	parameterizableReplacements do: [ :paramReplacement |
		| arguments stringArguments |

		arguments := paramReplacement allArgumentsToParameterizeInOrderUsing: nodesToBeParameterized.
		self assertArgumentsAreValid: arguments.
		
		stringArguments := arguments collect: [ :argParseNode | argParseNode value ]. 
		
		self 
			addReplacementAt: paramReplacement intervalToExtract 
			in: paramReplacement methodToExtractFrom
			with: (Message selector: aNewMessage selector arguments: stringArguments)
			paramNodes: paramReplacement parameterizableNodesSortedByInterval ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 30/Mar/2026, 2:28:14 am (UTC)'!
addNewParameterizable: aParameterizableCodeFound in: aMethod

	| newParamReplacement |

	newParamReplacement := ExtractMethodParameterizableReplacement
		for: aParameterizableCodeFound first
		interval: aParameterizableCodeFound second
		parameterizableNodes: aParameterizableCodeFound third
		in: aMethod.

	(self canAddNewParamReplacement: newParamReplacement)
		ifTrue: [ parameterizableReplacements add: newParamReplacement ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 9/Mar/2026, 1:03:02 am (UTC)'!
addParameterizableFromReplacement: aParameterizableReplacement

	(self intersectsWithAnyParamReplacement: aParameterizableReplacement) not
		ifTrue: [ parameterizableReplacements add: aParameterizableReplacement ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 11/Mar/2026, 1:04:49 pm (UTC)'!
addReplacementsAsParameterizables

	replacements do: [ :replacement |
		self addParameterizableFromReplacement: replacement asParameterizableReplacement ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 26/Mar/2026, 12:49:50 am (UTC)'!
assertArgumentsAreValid: arguments

	"Up to now, if there is a temp var duplicated we cannot apply the refactoring"

	arguments withIndexDo: [ :argumentWithRange :index | 
		| argument |
	
		argument := argumentWithRange key.
	
		arguments from: index + 1 to: arguments size do: [ :otherArgumentWithRange |
			| otherArgument |
			
			otherArgument := otherArgumentWithRange key.
			
			(argument isVariableNode 
				and: [ otherArgument isVariableNode and: [ argument equivalentTo: otherArgument ] ])
				ifTrue: [ self signalCannotApplyTheRefactoringDueToDuplicateTempVar ].
		] 
	]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 9/Apr/2026, 12:54:24 am (UTC)'!
canAddNewParamReplacement: newParamReplacement 

	^ (self intersectsWithAnyParamReplacement: newParamReplacement) not
		and: [ newParamReplacement anyParameterContainsReturnOrAssignment not ].! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 31/Mar/2026, 1:25:27 am (UTC)'!
findParameterizableReplacementsNodesIn: aMethod

	| parameterizableCode replacementsIntervals |
	
	replacementsIntervals := self replacementsIntervalsOf: aMethod.

	parameterizableCode := (self isSourceMethod: aMethod)
		ifTrue: [ 
			aMethod 
				sourceRangesOfParameterizableNodesOf: nodesToExtract 
				keepingSelection: intervalToExtract
				rejectingIntervals: replacementsIntervals ]	
		ifFalse: [ 
			aMethod 
				sourceRangesOfParameterizableNodesOf: nodesToExtract
				rejectingIntervals: replacementsIntervals ].

	parameterizableCode do: [ :aParameterizableCodeFound | 
		self addNewParameterizable: aParameterizableCodeFound in: aMethod ].! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 9/Mar/2026, 1:02:13 am (UTC)'!
intersectsWithAnyParamReplacement: aParamReplacement

	parameterizableReplacements 
		detect: [ :paramReplacement | paramReplacement hasIntersectionWith: aParamReplacement ]
		ifFound: [ :paramReplacement | ^ true ]
		ifNone: [ ^ false ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 27/Mar/2026, 1:23:28 am (UTC)'!
makeParameterizableReplacementsHaveTheSameParameters

	parameterizableReplacements := parameterizableReplacements collect: [ :paramReplacement |
		| newParamReplacement |
		
		newParamReplacement := paramReplacement.
		
		parameterizableReplacements do: [ :anotherParamReplacement |
			newParamReplacement := 
				newParamReplacement createWithSameParametersAs: anotherParamReplacement ].

		newParamReplacement ].! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 26/Mar/2026, 12:51:11 am (UTC)'!
signalCannotApplyTheRefactoringDueToDuplicateTempVar

	Refactoring refactoringError: self class cannotApplyTheRefactoringDueToDuplicateTempVarErrorMessage! !

!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'JEC 27/Mar/2026, 1:22:18 am (UTC)'!
useParameterizedToChangeReplacements

	self 
		addReplacementsAsParameterizables; 
		makeParameterizableReplacementsHaveTheSameParameters! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'JEC 1/Jun/2026, 7:47:09 pm (UTC)'!
extractedCodeOrAnyParameterizableReplacement 

	"The code selected to extract could be removed from the replacements with 
	the ExtractRepeatedCodeWindow. If that is the case, returns any the first one."

	^ parameterizableReplacements 
		detect: [ :parameterizableReplacement | parameterizableReplacement intervalToExtract = intervalToExtract ]
		ifNone: [ parameterizableReplacements first ]! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'JEC 12/Feb/2026, 3:01:12 am (UTC)'!
parameterizableReplacements

	^ parameterizableReplacements! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'JEC 10/Feb/2026, 12:27:38 am (UTC)'!
parameterizableReplacementsAt: anIndex

	^ self parameterizableReplacements at: anIndex! !

!ExtractMethodReplacementsFinder methodsFor: 'parameterizable code' stamp: 'JEC 15/Apr/2026, 3:20:34 pm (UTC)'!
removeParameterizableReplacementAt: anIndex

	^ parameterizableReplacements removeAt: anIndex ! !

!ExtractMethodReplacementsFinder methodsFor: 'testing' stamp: 'JEC 31/May/2026, 7:20:00 pm (UTC)'!
anyParameterizableNodeIncludes: aParseNode at: anIntervalToExtract in: aMethodReference 

	^ parameterizableReplacements anySatisfy: [ :paramReplacement | 
		(paramReplacement isOf: aMethodReference)
		and: [ paramReplacement intersectsIntervalWith: anIntervalToExtract ]
		and: [ paramReplacement parameterNodeIncludes: aParseNode ] ]! !

!ExtractMethodReplacementsFinder methodsFor: 'testing' stamp: 'JEC 12/Feb/2026, 3:01:51 am (UTC)'!
hasFoundParameterizableReplacements

	^ parameterizableReplacements notEmpty! !

!ExtractMethodReplacementsFinder methodsFor: 'private' stamp: 'JEC 12/Feb/2026, 3:01:12 am (UTC)'!
initializeFinding

	super initializeFinding.
	
	parameterizableReplacements := OrderedCollection new.! !


!ExtractMethodReplacementsFinder class methodsFor: 'instance creation' stamp: 'JEC 1/Jun/2026, 7:56:47 pm (UTC)'!
ofCodeIn: anIntervalToExtract at: aMethod
	
	"This is because the Message object hasn't been created yet. 
	It will be instantiated once the user adds the method name at the end of the flow."
	
	^self new initializeOfCodeIn: anIntervalToExtract at: aMethod to: nil ! !

!ExtractMethodReplacementsFinder class methodsFor: 'error message' stamp: 'JEC 26/Mar/2026, 12:51:36 am (UTC)'!
cannotApplyTheRefactoringDueToDuplicateTempVarErrorMessage

	^ 'Cannot apply the refactoring due to a duplicate temporary variable'! !


!ExtractParameterizedCodeStyler methodsFor: 'private' stamp: 'JEC 29/Mar/2026, 1:33:20 am (UTC)'!
privateAutoHighlight

	| attr |

	super privateAutoHighlight.

	(textModel textProvider respondsTo: #parametrizableNodesRanges) ifTrue: [
		attr := TextBackgroundColor color: (Color orange alpha: 0.6).
		textModel textProvider parametrizableNodesRanges do: [ :interval |
			formattedText
				addAttribute: attr
				from: interval first
				to: interval last ] ]! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 8/Apr/2026, 11:49:33 pm (UTC)'!
initializeSourceCodeToExtract

	| parameterizableNodesToReplace |

	sourceCodeToExtract := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last.
	
	parameterizableNodesToReplace := parameterizableNodes withIndexCollect: [ :nodeWithInterval :index | 
		(nodeWithInterval value - intervalToExtract first + 1) -> (newMessage arguments at: index) ].
	
	sourceCodeToExtract := sourceCodeToExtract copyReplacing: parameterizableNodesToReplace.
	
	((sourceCodeToExtract endsWith: '.') not and: [ extractMethodProgrammer shouldReturnAnyVariable ])
		ifTrue: [ sourceCodeToExtract := sourceCodeToExtract , '.' ].! !


!ExtractMethodParametersDetector methodsFor: 'evaluating' stamp: 'JEC 8/Apr/2026, 11:52:34 pm (UTC)'!
value
	
	"Return the parse nodes of the parameters to use in the extracted method.
	
	Implementation details
	--------------
	The enumeration order of the parse nodes is non-deterministic.
	
	To fix this, I keep track of the position for each parameter node to sort
	results in the order they appear in the source code."
	
	| parameterNodePositions |
	
	parameterNodePositions := SortedCollection sortBlock: [ :parameterNode :anotherParameterNode |
		parameterNode value <= anotherParameterNode value ].
	
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ 
				| equivalentParameterizableNodes |
				
				equivalentParameterizableNodes := parameterizableNodes 
					select: [ :paramNode | paramNode key equivalentTo: parseNode ].
				
				equivalentParameterizableNodes 
					ifEmpty: [ parameterNodePositions add: parseNode -> (self firstPositionInIntervalToExtractIn: sourceRanges) ]
					ifNotEmpty: [  :nodesToAdd |
						parameterNodePositions addAll: (nodesToAdd 
							collect: [ :nodeToAdd | nodeToAdd key -> (nodeToAdd value first) ])
					] ] ].
		
	^ parameterNodePositions collect: [ :paramNode | paramNode key ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 31/May/2026, 7:13:53 pm (UTC)'!
shouldBeParameterized: parseNode appearingIn: sourceRanges
	
	| isParamNode isIncludedInAnyParamNode |
	
	(self intervalToExtractIncludesAnyOf: sourceRanges) ifFalse: [ ^ false ].
	
	isParamNode := self isAParameterizableNode: parseNode.
	isIncludedInAnyParamNode := self isIncludedInAnyParameterizableNode: parseNode.
	
	^ isParamNode 
		or: [
			isIncludedInAnyParamNode not
				and: [ parseNode isTempOrArg
				and: [ (self definedInOuterScope: parseNode )
				and: [ (self isExtractedAlongWithItsDeclaration: parseNode) not
				and: [ (self isExtractedAlongWithItsAssignment: parseNode) not
					or: [ self isReadBeforeAssignment: parseNode ] ] ] ] ]
			]! !


!ExtractMethodParametersDetector class methodsFor: 'evaluating' stamp: 'JEC 15/Apr/2026, 3:19:24 pm (UTC)'!
valueFor: aMethodNodeToRefactor at: anIntervalToExtract

	^ (self for: aMethodNodeToRefactor at: anIntervalToExtract with: #())
		value! !


!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'JEC 10/Mar/2026, 10:39:14 pm (UTC)'!
initializeExtractedModifiers

	| returnNodesWithSourceRanges |
	
	returnNodesWithSourceRanges := OrderedCollection new.
	extractedCodeModifiers := SortedCollection sortBlock: [ :leftExtractedTempVar :rightExtractedTempVar |
			leftExtractedTempVar firstSourceCodeIndex < rightExtractedTempVar firstSourceCodeIndex ].

	methodNode 
		completeSourceRangesWithin: intervalToExtract 
		do: [ :parseNode :sourceRanges |
			(parseNode isTemp and: [ parseNode isArg not ])
				ifTrue: [
					extractedCodeModifiers add: 
						(ExtractMethodTempVarCodeModifier 
							for: parseNode 
							from: intervalToExtract 
							of: methodNode 
							to: newMessage) ].
			
			parseNode isReturn
				ifTrue: [ returnNodesWithSourceRanges add: parseNode -> sourceRanges ] ].
	
	"It is necessary to know, for the return nodes, whether the temporary variables in them 
	are going to be renamed.
	This is because, when replacing the code, if the temporary variables are renamed, 
	we no longer have that information and won’t be able to find the place to replace."
	returnNodesWithSourceRanges do: [ :returnNodeWithSourceRanges |
		self addExtractMethodCodeModifierIfNecessary: returnNodeWithSourceRanges ]
	! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'JEC 11/Feb/2026, 11:21:23 am (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments

	^ self 
		fromInterval: anIntervalToExtract 
		of: aMethodToExtractCodeFrom 
		to: newMessage 
		categorizedAs: aCategory 
		withArgumentNames: renamedArguments
		withParameterizableNodes: #()! !


!ExtractMethodReplacement class methodsFor: 'instance creation' stamp: 'JEC 27/Mar/2026, 1:51:47 am (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage 

	^ self 
		fromInterval: anIntervalToExtract 
		of: aMethodToExtractCodeFrom 
		to: newMessage 
		withParameterizableNodes: #()! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'JEC 14/Mar/2026, 12:20:41 am (UTC)'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	self initializeNewMessageArguments.
	self initializeRenamedNewMessageArguments! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 11/Feb/2026, 11:00:46 am (UTC)'!
requestNewMessage

	| parseNodesToParameterize |

	parseNodesToParameterize := self parseNodesToParameterize.

	^ self 
		requestNewMessageFor: parseNodesToParameterize 
		usingForSaving: parseNodesToParameterize.! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 1/Jun/2026, 7:53:32 pm (UTC)'!
requestRefactoringParameters

	self findReplacements.
	
	finder hasOneReplacement 
		ifTrue: [ self valueWithAllReplacements ]
		ifFalse: [ ExtractRepeatedCodeWindow openFrom: self with: finder multimethod: true ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'JEC 1/Jun/2026, 8:10:52 pm (UTC)'!
valueCreatingWith: aRefactoringCreationBlock

	"We can have multiple replacements but choose only one to apply. 
	Therefore, there must be the ones chosen here."
	finder useReplacements: aRefactoringCreationBlock value replacements.

	finder hasFoundParameterizableReplacements
		ifTrue: [
			ExtractParameterizedCodeWindow 
				openFrom: self 
				with: finder 
				using: aRefactoringCreationBlock ]
		ifFalse: [ self valueAcceptingOnlyEquivalentsHandlingExceptions ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'JEC 14/Mar/2026, 2:47:27 am (UTC)'!
buildInitialKeywordSelectorAnswer: parseNodesToParameterize

	^ Text streamContents: [ :stream |
		parseNodesToParameterize 
			withIndexDo: [ :parseNode :index | 
				
				| repeatedParseNodeNumber parseNodeName|
				repeatedParseNodeNumber := 1.
				index > repeatedParseNodeNumber 
					ifTrue: [ 
						parseNodesToParameterize 
							from: 1 
							to: index - 1 
							do: [ :beforeParseNode | 
								beforeParseNode name = parseNode name 
									ifTrue: [ repeatedParseNodeNumber := repeatedParseNodeNumber + 1 ] ] ].
				
				parseNodeName := repeatedParseNodeNumber = 1
					ifTrue: [ parseNode name ]
					ifFalse: [ parseNode name , repeatedParseNodeNumber asString ].
				
				stream
					nextPutAll: (self formatAsKeyword: self initialSelectorKeyword, index printString, ': ');
					nextPutAll: (self formatAsMethodArgument: parseNodeName) ]
			separatedBy: [ stream newLine ]]! !


!ReplacementsFinder methodsFor: 'evaluating' stamp: 'JEC 5/Feb/2026, 12:43:44 am (UTC)'!
value

	self 
		initializeFinding;
		useFindingByEquivalenceStrategy;
		find.! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 7/Mar/2026, 1:02:34 am (UTC)'!
addReplacementAt: foundIntervalToExtract in: aMethod
	
	self addReplacementFrom: [ self createReplacementAt: foundIntervalToExtract in: aMethod ]! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 31/Mar/2026, 12:33:12 am (UTC)'!
findReplacementsUsingNodeEquivalenceIn: aMethod

	| rangesToExtract |
	
	rangesToExtract := (self isSourceMethod: aMethod)
		ifTrue: [  aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract keepingSelection: intervalToExtract ]	
		ifFalse: [ aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract ].
		
	rangesToExtract do: [ :sourceRangeToExtract | self addReplacementAt: sourceRangeToExtract in: aMethod ].! !

!ReplacementsFinder methodsFor: 'private' stamp: 'JEC 5/Feb/2026, 12:42:13 am (UTC)'!
initializeFinding

	sourceCodeToExtract := sourceMethod sourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	nodesToExtract := self nodesToExtract.
	replacements := OrderedCollection new.! !


!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 11/Mar/2026, 12:43:54 am (UTC)'!
createReplacementAt: foundIntervalToExtract in: aMethod
	
	^ self 
		createReplacementAt: foundIntervalToExtract 
		in: aMethod 
		with: newMessage
		paramNodes: #()! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 9/Mar/2026, 11:36:21 pm (UTC)'!
findReplacementsAt: aClass

	"I left this implementation instead of 
	
	aClass methodsDo: findingStrategy
	
	for debugging purposes. We can do Through with this one - Hernan"
	aClass methodsDo: [ :aMethod | 
		findingStrategy value: aMethod.
		self findParameterizableReplacementsNodesIn: aMethod ]
	! !

!methodRemoval: ReplacementsFinder #defineFindingStrategy stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ReplacementsFinder removeSelector: #defineFindingStrategy!
!methodRemoval: ExtractMethodApplier #findReplacementsWith: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodApplier removeSelector: #findReplacementsWith:!
!methodRemoval: ExtractMethodReplacement #initializeFrom:of:to: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodReplacement removeSelector: #initializeFrom:of:to:!
!methodRemoval: ExtractMethodNewMethod class #assert:hasValidParametersForExtracting:from: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodNewMethod class removeSelector: #assert:hasValidParametersForExtracting:from:!
!methodRemoval: ExtractMethodProgrammer class #from:of:to: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodProgrammer class removeSelector: #from:of:to:!
!methodRemoval: ExtractMethodProgrammer #initializeFrom:of:to: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodProgrammer removeSelector: #initializeFrom:of:to:!
!methodRemoval: ExtractMethodParametersDetector class #for:at: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodParametersDetector class removeSelector: #for:at:!
!methodRemoval: ExtractMethodParametersDetector #initializeFor:at: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodParametersDetector removeSelector: #initializeFor:at:!
!methodRemoval: ExtractMethodNewMethodSourceCode class #fromInterval:of:to:withArgumentNames: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodNewMethodSourceCode class removeSelector: #fromInterval:of:to:withArgumentNames:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initializeFrom:of:to:withArgumentNames: stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
ExtractMethodNewMethodSourceCode removeSelector: #initializeFrom:of:to:withArgumentNames:!
!methodRemoval: CompiledMethod #messageFromSelector stamp: 'jmv 3/Jul/2026, 7:46:53 pm (UTC)'!
CompiledMethod removeSelector: #messageFromSelector!

!EquivalentNodesFinder reorganize!
('private - visiting' addAllToFoundNodes: addEquivalentNodesInCascadeNode: addEquivalentNodesOf: addToEquivalentsIfOneStatementEquivalentTo: addToEquivalentsNodesPartiallyOf: areNodesToLookForAPartiallyCascadeNode: hasOneNodeToLookForAndEquivalentTo: selectionToKeepIsEqualOrHasNoIntersectionWith: shouldKeepSelection)
('ranges' equivalentNodesRanges)
('visiting - finding nodes' addSingleAssigmentNodeIfNecessary: addSingleBacktickNodeIfNecessary: addSingleBlockNodeIfNeccesary: addSingleBraceNodeIfNecessary: addSingleCascadeNodeIfNecessary: addSingleInstanceVariableNodeIfNecessary: addSingleLiteralNodeIfNeccesary: addSingleLiteralVariableNodeIfNecessary: addSingleMessageNodeIfNeccesary: addSingleReturnNodeIfNeccesary: addSingleTempVariableNodeIfNecessary: nodesToLookForMatchingConditionWith:)
!


!EquivalentNodesFinder class reorganize!
!

!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer parameterizableNodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodParametersDetector category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodParametersDetector
	instanceVariableNames: 'intervalToExtract methodNodeToRefactor parameterizableNodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodProgrammer category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodProgrammer
	instanceVariableNames: 'intervalToExtract methodNode newMessage extractedCodeModifiers compiledMethod newMethodIsUsingAssignmentBlock newMethodIsUsingReturningBlock parameterizableNodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration extractMethodProgrammer newCompiledMethod parameterizableNodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ReplacementsFinder reorganize!
('evaluating' value)
('replacements' inClassReplacements removeReplacementAt: replacements replacementsAt:ifAbsent: sourceMethodReplacements)
('source code' sourceCodeToExtract)
('testing' hasOneReplacement)
('private-replacement finding' addReplacementAt:in: addReplacementFrom: createReplacementAt:in: defineFindingByEquivalenceStrategy find findReplacementsUsingNodeEquivalenceIn: findReplacementsUsingStringEqualityIn: isSourceMethod: nodesToExtract useFindingByEquivalenceStrategy)
('private' initializeFinding)
!

!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage parameterizableReplacements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
