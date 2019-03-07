'From Cuis 5.0 of 7 November 2016 [latest update: #3637] on 7 March 2019 at 9:40:58 am'!
!classDefinition: #RefactoringError category: #'Tools-Refactoring'!
Error subclass: #RefactoringError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #CanNotRefactorDueToReferencesError category: #'Tools-Refactoring'!
RefactoringError subclass: #CanNotRefactorDueToReferencesError
	instanceVariableNames: 'references referencee'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RefactoringWarning category: #'Tools-Refactoring'!
Warning subclass: #RefactoringWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporaryRewriter category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #ExtractToTemporaryRewriter
	instanceVariableNames: 'refactoring finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ParseNodeToReplaceFinder category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #ParseNodeToReplaceFinder
	instanceVariableNames: 'refactoring currentBlock nodesToReplace blockContainingFirstNodeToReplace firstNodeToReplaceIndex currentStatementIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorWizardStepWindow category: #'Tools-Refactoring'!
MessageSetWindow subclass: #ChangeSelectorWizardStepWindow
	instanceVariableNames: 'applier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorImplementorsStepWindow category: #'Tools-Refactoring'!
ChangeSelectorWizardStepWindow subclass: #ChangeSelectorImplementorsStepWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorSendersStepWindow category: #'Tools-Refactoring'!
ChangeSelectorWizardStepWindow subclass: #ChangeSelectorSendersStepWindow
	instanceVariableNames: 'changedMethods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #Refactoring category: #'Tools-Refactoring'!
Object subclass: #Refactoring
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #AddInstanceVariable
	instanceVariableNames: 'newVariable classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelector category: #'Tools-Refactoring'!
Refactoring subclass: #ChangeSelector
	instanceVariableNames: 'oldSelector newSelector implementors senders changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddParameter category: #'Tools-Refactoring'!
ChangeSelector subclass: #AddParameter
	instanceVariableNames: 'newParameter newParameterValue implementorTrailingString senderTrailingString index isAddingLast'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveParameter category: #'Tools-Refactoring'!
ChangeSelector subclass: #RemoveParameter
	instanceVariableNames: 'parameterToRemove parameterIndex senderReplacementString isLastParameter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameSelector category: #'Tools-Refactoring'!
ChangeSelector subclass: #RenameSelector
	instanceVariableNames: 'newSelectorKeywords'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariable parseNodeToExtract methodNodeToRefactor newTemporary'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InsertSuperclass category: #'Tools-Refactoring'!
Refactoring subclass: #InsertSuperclass
	instanceVariableNames: 'classToRefactor superclassName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
Refactoring subclass: #PushUpMethod
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveAllUnreferencedInstanceVariables category: #'Tools-Refactoring'!
Refactoring subclass: #RemoveAllUnreferencedInstanceVariables
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RemoveInstanceVariable
	instanceVariableNames: 'variableToRemove classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameClass category: #'Tools-Refactoring'!
Refactoring subclass: #RenameClass
	instanceVariableNames: 'classToRename newClassName system undeclared classToRenameOriginalName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable addInstanceVariable rewriter renamedReferences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #RenameTemporary
	instanceVariableNames: 'oldVariable newVariable methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #SafelyRemoveClass category: #'Tools-Refactoring'!
Refactoring subclass: #SafelyRemoveClass
	instanceVariableNames: 'classToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RefactoringApplier category: #'Tools-Refactoring'!
Object subclass: #RefactoringApplier
	instanceVariableNames: 'requestExitBlock refactoring changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddInstanceVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #AddInstanceVariableApplier
	instanceVariableNames: 'classToRefactor oldInstanceVariable newInstanceVariable browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ChangeSelectorApplier
	instanceVariableNames: 'oldSelector scopeChoice implementors senders implementingClass wizardStepWindow shouldShowChanges browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddParameterApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #AddParameterApplier
	instanceVariableNames: 'newParameter newParameterValue newKeyword parameterIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveParameterApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #RemoveParameterApplier
	instanceVariableNames: 'newParameter newParameterValue newKeyword originalMethod parameterToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameSelectorApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #RenameSelectorApplier
	instanceVariableNames: 'newSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InsertSuperclassApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #InsertSuperclassApplier
	instanceVariableNames: 'browser newSuperclassName classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveAllUnreferencedInstanceVariablesApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RemoveAllUnreferencedInstanceVariablesApplier
	instanceVariableNames: 'classToRefactor browser variableToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveInstanceVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RemoveInstanceVariableApplier
	instanceVariableNames: 'classToRefactor browser variableToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameClassApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameClassApplier
	instanceVariableNames: 'browser classToRename newClassName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameInstanceVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameInstanceVariableApplier
	instanceVariableNames: 'classToRefactor oldInstanceVariable newInstanceVariable browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameTemporaryApplier
	instanceVariableNames: 'classToRefactor oldVariable newVariable smalltalkEditor methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #SafelyRemoveClassApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #SafelyRemoveClassApplier
	instanceVariableNames: 'classToRemove browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RefactoringMenues category: #'Tools-Refactoring'!
Object subclass: #RefactoringMenues
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RefactoringPrecondition category: #'Tools-Refactoring'!
Object subclass: #RefactoringPrecondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #NewClassPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #NewClassPrecondition
	instanceVariableNames: 'newClassName system undeclared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #NewInstanceVariablePrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #NewInstanceVariablePrecondition
	instanceVariableNames: 'classToAddInstVar instVarName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CanNotRefactorDueToReferencesError methodsFor: 'references' stamp: 'HAW 7/11/2018 16:56:20'!
anyReference
	
	^references anyOne ! !

!CanNotRefactorDueToReferencesError methodsFor: 'references' stamp: 'HAW 7/11/2018 16:54:54'!
numberOfReferences
	
	^references size! !

!CanNotRefactorDueToReferencesError methodsFor: 'references' stamp: 'HAW 8/1/2018 17:26:49'!
references

	^references copy! !

!CanNotRefactorDueToReferencesError methodsFor: 'initialization' stamp: 'HAW 8/1/2018 17:32:33'!
initialize: aMessageText references: aCollectionOfReferences to: aReferencee

	self messageText: aMessageText.
	references := aCollectionOfReferences.
	referencee := aReferencee ! !

!CanNotRefactorDueToReferencesError methodsFor: 'referencee' stamp: 'HAW 8/1/2018 17:32:46'!
referencee

	^referencee ! !


!CanNotRefactorDueToReferencesError class methodsFor: 'signaling' stamp: 'HAW 8/1/2018 17:32:15'!
signal: aMessageText references: references to: aReferencee

	self new 
		initialize: aMessageText references: references to: aReferencee;
		signal! !


!ExtractToTemporaryRewriter methodsFor: 'initialization' stamp: 'HAW 10/18/2017 18:21:40'!
initializeOf: anExtractToTemporary on: aParseNodeToReplaceFinder 
	
	refactoring := anExtractToTemporary.
	finder := aParseNodeToReplaceFinder ! !

!ExtractToTemporaryRewriter methodsFor: 'visiting' stamp: 'HAW 10/18/2017 18:26:27'!
visitBlockNode: aBlockNode

	| statements |
	
	statements := aBlockNode statements.
	statements withIndexDo: [ :statement :index |
		(finder shouldReplace: statement)
			ifTrue: [ statements at: index put: refactoring newTemporary ]
			ifFalse: [ statement accept: self]]! !

!ExtractToTemporaryRewriter methodsFor: 'visiting' stamp: 'HAW 10/19/2017 06:24:35'!
visitMessageNode: aMessageNode

	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder withIndexDo: [:argument :index | 
		(finder shouldReplace: argument)
			ifTrue: [ aMessageNode arguments at: index put: refactoring newTemporary ]
			ifFalse: [ argument accept: self]]! !

!ExtractToTemporaryRewriter methodsFor: 'visiting' stamp: 'HAW 10/19/2017 06:06:05'!
visitReturnNode: aReturnNode

	(finder shouldReplace: aReturnNode expr)
		ifTrue: [ aReturnNode expr: refactoring newTemporary ]
		ifFalse: [ super visitReturnNode: aReturnNode ]! !


!ExtractToTemporaryRewriter class methodsFor: 'instance creation' stamp: 'HAW 10/18/2017 18:21:16'!
of: anExtractToTemporary on: aParseNodeToReplaceFinder 
	
	^self new initializeOf: anExtractToTemporary on: aParseNodeToReplaceFinder 
	! !


!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/19/2017 06:03:12'!
addNodeToReplace: aParseNode

	nodesToReplace isEmpty ifTrue: [ 
		blockContainingFirstNodeToReplace := currentBlock.
		firstNodeToReplaceIndex := currentStatementIndex ].
	
	nodesToReplace add: aParseNode.! !

!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/18/2017 18:30:44'!
blockContainingFirstNodeToReplace
	
	^blockContainingFirstNodeToReplace! !

!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/18/2017 18:31:00'!
firstNodeToReplace
	
	^nodesToReplace first! !

!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/18/2017 18:39:56'!
firstNodeToReplaceIndex
	
	^firstNodeToReplaceIndex! !

!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/18/2017 18:12:47'!
keepCurrentBlockIfFirstNodeToReplace
	
	nodesToReplace isEmpty ifTrue: [ blockContainingFirstNodeToReplace := currentBlock ].
! !

!ParseNodeToReplaceFinder methodsFor: 'as yet unclassified' stamp: 'HAW 10/18/2017 18:26:50'!
shouldReplace: aParseNode

	^nodesToReplace includes: aParseNode ! !

!ParseNodeToReplaceFinder methodsFor: 'initialization' stamp: 'HAW 10/18/2017 18:12:47'!
initializeOf: anExtractToTemporary

	refactoring := anExtractToTemporary.
	nodesToReplace := OrderedCollection new.! !

!ParseNodeToReplaceFinder methodsFor: 'visiting' stamp: 'HAW 10/19/2017 06:02:34'!
visitBlockNode: aBlockNode

	| previousBlock |
	
	previousBlock := currentBlock.
	currentBlock := aBlockNode.
	
	aBlockNode statements withIndexDo: [:statement :index | 
		currentStatementIndex := index.
		statement accept: self].
	
	currentBlock := previousBlock ! !

!ParseNodeToReplaceFinder methodsFor: 'visiting' stamp: 'HAW 10/18/2017 18:12:47'!
visitLiteralNode: aLiteralNode

	(refactoring shouldExtract: aLiteralNode) ifTrue: [ self addNodeToReplace: aLiteralNode ]! !

!ParseNodeToReplaceFinder methodsFor: 'visiting' stamp: 'HAW 10/19/2017 06:20:02'!
visitMessageNode: aMessageNode

	super visitMessageNode: aMessageNode! !


!ParseNodeToReplaceFinder class methodsFor: 'instance creation' stamp: 'HAW 10/18/2017 18:12:47'!
of: anExtractToTemporary
	
	^self new initializeOf: anExtractToTemporary ! !


!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 5/31/2017 17:26:05'!
add

	self subclassResponsibility ! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 5/31/2017 17:36:12'!
addToList: aMethod

	model addMethodReference: aMethod methodReference ifIncluded: [ self inform: 'Method already in list' ]! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 10/9/2018 20:57:59'!
do: aBlock withEnteredClassLabeled: aLabel
		
	| className |
	
	className := ClassNameRequestMorph request: aLabel onCancel: [ ^self ].
	^self withClassNamed: className do: aBlock! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 5/31/2017 18:17:19'!
inform: aClass doesNotImplement: aSelector

 	self inform: aClass name, ' does not implement #', aSelector ! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:02:43'!
justRefactor
	
	applier doNotShowChanges.
	self refactor.! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:16:36'!
refactor
	
	applier wizardStepWindow: self. 	
	applier wizardEnded.
	! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 5/31/2017 17:07:19'!
remove
	
	model removeMessageFromBrowserKeepingLabel! !

!ChangeSelectorWizardStepWindow methodsFor: 'actions' stamp: 'HAW 10/10/2018 05:46:24'!
withClassNamed: aName do: aBlock

	| trimmedNamed |
	
	trimmedNamed := aName withBlanksTrimmed.
	
	(Smalltalk classNamed: trimmedNamed asSymbol)
		ifNotNil: aBlock
		ifNil: [ self inform: 'Class ', trimmedNamed , ' does not exist' ].
! !

!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'HAW 5/31/2017 17:22:39'!
addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10! !

!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'HAW 5/31/2017 17:18:46'!
addButtonsTo: row color: buttonColor
	
	self subclassResponsibility ! !

!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'HAW 5/31/2017 17:26:55'!
buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'HAW 5/31/2017 17:27:25'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !

!ChangeSelectorWizardStepWindow methodsFor: 'compile methods' stamp: 'HAW 5/31/2017 17:08:14'!
compiledMethodsFrom: methodReferences

	^ methodReferences collect: [:aMethodReference | aMethodReference compiledMethod ]! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 5/31/2017 17:21:11'!
createAddButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #add
		label: 'Add'.
! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 5/31/2017 17:21:19'!
createCancelButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #delete
		label: 'Cancel'.
! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 10/10/2018 19:47:37'!
createJustRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #justRefactor
		label: 'Just Refactor!!'! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 10/10/2018 19:47:43'!
createRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 5/31/2017 17:16:37'!
createRemoveButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: #isMessageSelected
		action: #remove
		label: 'Remove'.
! !

!ChangeSelectorWizardStepWindow methodsFor: 'initialization' stamp: 'HAW 1/7/2019 11:04:02'!
initializeFrom: aChangeSelectorApplier

	applier := aChangeSelectorApplier ! !

!ChangeSelectorWizardStepWindow methodsFor: 'testing' stamp: 'HAW 6/5/2017 17:40:16'!
isMessageSelected

	^model isNil ifTrue: [ false ] ifFalse: [ model selection notNil ]! !

!ChangeSelectorWizardStepWindow methodsFor: 'accessing' stamp: 'HAW 1/7/2019 11:02:43'!
oldSelector
	
	^applier oldSelector ! !


!ChangeSelectorWizardStepWindow class methodsFor: 'instance creation' stamp: 'HAW 5/31/2017 16:59:25'!
methodReferencesOf: methods

	^methods asOrderedCollection collect: [:aCompiledMethod | aCompiledMethod methodReference ].
! !

!ChangeSelectorWizardStepWindow class methodsFor: 'instance creation' stamp: 'HAW 1/7/2019 11:03:50'!
openFrom: aChangeSelectorApplier methods: methods label: aLabel

	| window |
	
	window := self openMessageList: (self methodReferencesOf: methods) label: aLabel autoSelect: aChangeSelectorApplier oldSelector.
	window initializeFrom: aChangeSelectorApplier.
	
	^window 

! !


!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 6/5/2017 17:33:16'!
add
		
	self 
		do: [ :classOfImplementorToAdd | self addImplementorIn: classOfImplementorToAdd ]
		withEnteredClassLabeled:  'Class that implements ', self oldSelector ! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 6/5/2017 17:33:26'!
addImplementorIn: classOfImplementorToAdd 

	| implementorToAdd |
	
	implementorToAdd := classOfImplementorToAdd 
		compiledMethodAt: self oldSelector 
		ifAbsent: [ ^self inform: classOfImplementorToAdd doesNotImplement: self oldSelector ].
		
	self addToList: implementorToAdd! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:02:43'!
changeImplementors
	
	applier implementors: (self compiledMethodsFrom: model messageList).
	! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 10/10/2018 18:07:57'!
refactor

	self changeImplementors.
	super refactor ! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:17:47'!
seeSenders

	self changeImplementors.
	self delete.	
	
	ChangeSelectorSendersStepWindow openFrom: applier ! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'GUI building' stamp: 'HAW 10/10/2018 19:55:34'!
addButtonsTo: row color: buttonColor

	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeSendersButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.
! !

!ChangeSelectorImplementorsStepWindow methodsFor: 'button creation' stamp: 'HAW 10/10/2018 19:55:52'!
createSeeSendersButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeSenders
		label: 'See Senders'.

! !


!ChangeSelectorImplementorsStepWindow class methodsFor: 'instance creation' stamp: 'HAW 8/18/2018 17:26:33'!
openFrom: aChangeSelectorRequest

	^self 
		openFrom: aChangeSelectorRequest 
		methods: aChangeSelectorRequest implementors 
		label: 'Implementors of #', aChangeSelectorRequest oldSelector, ' to Refactor'  
! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 6/5/2017 17:36:00'!
add
	
	self 
		do: [ :classOfSenderToAdd | self askAndAddSenderOf: classOfSenderToAdd ]
		withEnteredClassLabeled: 'Class that sends #', self oldSelector
		! !

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 11/8/2018 15:25:57'!
askAndAddSenderOf: classOfSenderToAdd 
		
	| senderSelector senderToAdd |
	
	senderSelector := FillInTheBlankMorph request: 'Selector of sender of #', self oldSelector onCancel: [^self ].
	senderToAdd := classOfSenderToAdd 
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].
		
	(senderToAdd sendsOrRefersTo: self oldSelector) ifFalse: [ ^self inform: senderToAdd classAndSelector, ' does not refer to #', self oldSelector ].
	
	self addToList: senderToAdd ! !

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:02:43'!
changeRequestSenders
	
	applier senders: (self compiledMethodsFrom: model messageList).
	! !

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 10/10/2018 18:09:18'!
refactor
	
	self changeRequestSenders.
	super refactor ! !

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 1/7/2019 11:17:34'!
seeImplementors
	
	self changeRequestSenders.
	self delete.
	
	ChangeSelectorImplementorsStepWindow openFrom: applier! !

!ChangeSelectorSendersStepWindow methodsFor: 'GUI building' stamp: 'HAW 10/10/2018 19:56:45'!
addButtonsTo: row color: buttonColor
	
	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeImplementorsButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.
! !

!ChangeSelectorSendersStepWindow methodsFor: 'button creation' stamp: 'HAW 10/10/2018 19:56:51'!
createSeeImplementorsButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'.
! !


!ChangeSelectorSendersStepWindow class methodsFor: 'instance creation' stamp: 'HAW 8/18/2018 17:26:18'!
openFrom: aChangeSelectorRequest

	^self 
		openFrom: aChangeSelectorRequest 
		methods: aChangeSelectorRequest senders 
		label: 'Senders of #', aChangeSelectorRequest oldSelector, ' to Refactor' ! !


!Refactoring methodsFor: 'applying' stamp: 'HAW 5/24/2017 21:08:11'!
apply

	self subclassResponsibility ! !


!Refactoring class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 16:34:16'!
canNotRefactorDueToReferencesError: aMessageText references: references to: referencee 
	
	^self canNotRefactorDueToReferencesErrorClass
		signal: aMessageText 
		references: references 
		to: referencee ! !

!Refactoring class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 16:32:42'!
canNotRefactorDueToReferencesErrorClass
	
	^CanNotRefactorDueToReferencesError! !

!Refactoring class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:58:07'!
refactoringError: aMessage

	self refactoringErrorClass signal: aMessage.! !

!Refactoring class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:58:21'!
refactoringErrorClass

	^ RefactoringError.! !

!Refactoring class methodsFor: 'warnings' stamp: 'HAW 6/11/2017 18:49:41'!
refactoringWarning: aMessageText

	^ self refactoringWarningClass signal: aMessageText.! !

!Refactoring class methodsFor: 'warnings' stamp: 'HAW 6/3/2017 12:05:48'!
refactoringWarningClass

	^ RefactoringWarning.! !


!AddInstanceVariable methodsFor: 'applying' stamp: 'HAW 6/11/2017 18:27:26'!
apply
	
	classToRefactor addInstVarName: newVariable.
	! !

!AddInstanceVariable methodsFor: 'accessing' stamp: 'HAW 6/11/2017 19:09:54'!
classToRefactor

	^classToRefactor ! !

!AddInstanceVariable methodsFor: 'accessing' stamp: 'HAW 6/6/2017 10:10:40'!
newVariable
	
	^newVariable ! !

!AddInstanceVariable methodsFor: 'initialization' stamp: 'HAW 6/6/2017 10:10:26'!
initializeNamed: aNewVariable to: aClassToRefactor 
	
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor ! !


!AddInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 3/4/2019 11:43:12'!
named: aVariable to: aClassToRefactor

	| trimmedVariable |
	
	trimmedVariable := aVariable withBlanksTrimmed.
	NewInstanceVariablePrecondition valueOf: trimmedVariable for: aClassToRefactor.	
	^self new initializeNamed: trimmedVariable to: aClassToRefactor 
	! !


!ChangeSelector methodsFor: 'applying' stamp: 'HAW 11/29/2018 15:19:19'!
apply
	
	self 
		createNewImplementors;
		renameSenders;
		removeOldImplementors.
		
	^changes
! !

!ChangeSelector methodsFor: 'remove old implementors - private' stamp: 'HAW 8/18/2018 12:02:34'!
removeOldImplementor: anImplementor 

	anImplementor methodClass removeSelector: anImplementor selector.
	changes add: anImplementor methodReference! !

!ChangeSelector methodsFor: 'remove old implementors - private' stamp: 'HAW 8/18/2018 12:01:49'!
removeOldImplementors
	
	implementors do: [:anImplementor | self removeOldImplementor: anImplementor ]! !

!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 11/8/2018 15:24:06'!
addToSendersIfOldSelectorIsSentIn: newImplementor
	
	(newImplementor sendsOrRefersTo: oldSelector) ifTrue: [ senders add: newImplementor ]. 
	! !

!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 3/4/2019 15:24:22'!
compileNewImplementorOf: anImplementor
		
	| implementorClassification newSourceCode |
	
	newSourceCode := self implementorNewSourceCodeOf: anImplementor.
	implementorClassification := anImplementor methodClass organization categoryOfElement: oldSelector.
	
	anImplementor methodClass 
		compile: newSourceCode  
		classified: implementorClassification.
! !

!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 8/18/2018 12:02:25'!
createNewImplementorOf: anImplementor

	| newImplementor |

	self compileNewImplementorOf: anImplementor.
	newImplementor := anImplementor methodClass compiledMethodAt: newSelector.
	self addToSendersIfOldSelectorIsSentIn: newImplementor.
	
	changes add: newImplementor methodReference 
	
	! !

!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 8/18/2018 12:02:52'!
createNewImplementors
	
	implementors do: [:anImplementor | self createNewImplementorOf: anImplementor ]
	! !

!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 8/18/2018 12:06:01'!
implementorNewSourceCodeOf: anImplementor

	self subclassResponsibility ! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 11/29/2018 15:28:39'!
add: oldSelectorLiteralRanges to: rangesToKeywords

	oldSelectorLiteralRanges do: [ :oldSelectorLiteralRange | 
		rangesToKeywords add: (oldSelectorLiteralRange first + 1 to: oldSelectorLiteralRange last) -> newSelector ].
	

	! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 8/18/2018 12:05:39'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	self subclassResponsibility ! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 11/29/2018 15:29:06'!
addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords

	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsInLiteralArrayOf: oldSelector.
	self add: oldSelectorLiteralRanges to: rangesToKeywords ! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 11/29/2018 15:28:27'!
addRangesForLiteralOf: methodNode to: rangesToKeywords
	
	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsForLiteralNode: oldSelector ifAbsent: [ ^#() ].
	self add: oldSelectorLiteralRanges to: rangesToKeywords.
	! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 11/29/2018 14:16:42'!
rangesToKeywordsOf: aMethod
	
	| methodNode rangesToKeywords |
	
	methodNode := aMethod methodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right | left key first < right key first ].
	
	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.
	
	^rangesToKeywords	! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 8/18/2018 12:03:19'!
renameSenders
	
	senders do: [ :aSender | self renameSendersIn: aSender ].
	! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 9/4/2018 19:25:20'!
renameSendersIn: aMethod

	| newSource rangesToNewStrings |
	
	rangesToNewStrings := self rangesToKeywordsOf: aMethod.
	newSource := aMethod sourceCode copyReplacing: rangesToNewStrings.
	aMethod methodClass compile: newSource.
	
	changes add: (MethodReference class: aMethod methodClass selector: aMethod selector)! !

!ChangeSelector methodsFor: 'implementors/senders' stamp: 'HAW 8/18/2018 12:04:37'!
implementorsSize
	
	^implementors size! !

!ChangeSelector methodsFor: 'implementors/senders' stamp: 'HAW 8/18/2018 12:03:31'!
sendersSize
	
	^senders size! !

!ChangeSelector methodsFor: 'initialization' stamp: 'HAW 9/3/2018 17:17:19'!
initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	
	oldSelector := anOldSelector.
	newSelector := aNewSelector.
	implementors := aCollectionOfImplementors.
	"I have to make a copy of senders because it can change with recursive implementors - Hernan"
	senders := aCollectionOfSenders asOrderedCollection.
	
	changes := Set new
	! !

!ChangeSelector methodsFor: 'selectors' stamp: 'HAW 1/7/2019 13:59:37'!
newSelector
	
	^newSelector ! !


!ChangeSelector class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 12:39:40'!
assertAllImplementors: implementors haveSame: aSelector

	| invalidImplementors |
	
	invalidImplementors := implementors reject: [ :anImplementor | anImplementor selector = aSelector ].
	invalidImplementors notEmpty ifTrue: [ self signalInvalidImplementors: invalidImplementors ].! !

!ChangeSelector class methodsFor: 'pre-conditions' stamp: 'HAW 11/29/2018 12:04:20'!
assertAllSenders: senders send: aSelector

	| invalidSenders |
	
	invalidSenders := senders reject: [ :aSender | aSender sendsOrRefersTo: aSelector ].
	invalidSenders notEmpty ifTrue: [ self signalInvalidSenders: invalidSenders of: aSelector ]! !

!ChangeSelector class methodsFor: 'exceptions' stamp: 'HAW 1/7/2019 10:51:35'!
errorMessageForInvalidImplementors: aCollectionOfInvalidImplementors

	^String streamContents: [ :stream |
		stream 
			nextPutAll: (aCollectionOfInvalidImplementors size = 1 ifTrue: [ 'This method' ] ifFalse: [ 'These methods']);
		 	nextPutAll: ' do not have same the same selector to rename: '.
		aCollectionOfInvalidImplementors asCommaSeparated: [:anImplementor | anImplementor printClassAndSelectorOn: stream ] on: stream ] ! !

!ChangeSelector class methodsFor: 'exceptions' stamp: 'HAW 1/7/2019 10:51:35'!
errorMessageForInvalidSenders: aCollectionOfInvalidSenders of: anOldSelector

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'Message #';
			nextPutAll: anOldSelector;
			nextPutAll: ' is not send by: '.
		aCollectionOfInvalidSenders asCommaSeparated: [:aSender | aSender printClassAndSelectorOn: stream ] on: stream ] ! !

!ChangeSelector class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:43:59'!
signalInvalidImplementors: invalidImplementors

	self refactoringError: (self errorMessageForInvalidImplementors: invalidImplementors).! !

!ChangeSelector class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 15:49:58'!
signalInvalidSenders: invalidSender of: aSelector

	self refactoringError: (self errorMessageForInvalidSenders: invalidSender of: aSelector).! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:35'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor
	
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theNonMetaClass.
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theMetaClass.

! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:39'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	| categories |
	
	categories := Set new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: [:aClassInHierarchy | categories add: aClassInHierarchy category ].
	categories do: [:aCategory | self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization ].
! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:42'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	| classesInCategory |
	
	classesInCategory := anOrganization classesAt: aCategory.
	classesInCategory do: [ :aPotentialClassToRefactor | self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor ].
! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:45'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: [ :aClassInHierarchy | ]
		
	! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:49'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: aBlock

	aClass theNonMetaClass withAllSubAndSuperclassesDo: [ :aPotentialClassToRefactor |
		aPotentialClassToRefactor isMeta ifFalse: [ 
			self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor.
			aBlock value: aPotentialClassToRefactor ]]
	! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/18/2018 17:19:52'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem

	aSystem allBehaviorsDo: [ :aPotentialClassToRefactor | 
		self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor ].
! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 10/15/2018 20:53:23'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor
	
	| potentialImplementor |
	
	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].
	
	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector | senders add: (aPotentialClassToRefactor compiledMethodAt: aSelector) ].
	! !


!AddParameter methodsFor: 'initialization' stamp: 'HAW 9/4/2018 19:00:42'!
initializedNamed: aNewParameter 
	at: anIndex
	addingLast: anIsAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector  
	addingToImplementors: anImplementorTrailingString 
	addingToSenders: aSenderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.
	
	newParameter := aNewParameter.
	newParameterValue := aNewParameterValue.
	implementorTrailingString := anImplementorTrailingString.
	senderTrailingString := aSenderTrailingString.
	
	index := anIndex.
	isAddingLast := anIsAddingLast ! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 3/4/2019 10:21:05'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| insertionPoints senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	isAddingLast 
		ifTrue: [ 
			insertionPoints := aMethodNode messageSendLastPositionsOf: oldSelector ifAbsent: [ #() ].
			insertionPoints do: [ :aPosition | | newPosition |
			newPosition := self firstNoSeparatorIndexIn: senderSourceCode startingFrom: aPosition.
			rangesToKeywords add: ((newPosition+1) to: newPosition) -> senderTrailingString ]]
		ifFalse: [ 
			insertionPoints := aMethodNode messageSendKeywordPositionsAt: index of: oldSelector ifAbsent: [ #()].
			insertionPoints do: [ :aPosition | 
			rangesToKeywords add: (aPosition to: aPosition-1) -> senderTrailingString ]]
	! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 11/11/2018 14:07:23'!
firstNoLineSeparatorIndexIn: sourceCode startingFrom: aStartingPosition
	
	^self firstNot: [ :aChar | aChar isLineSeparator ] indexIn: sourceCode startingFrom: aStartingPosition ! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 11/11/2018 14:06:46'!
firstNoSeparatorIndexIn: sourceCode startingFrom: aStartingPosition

	"Looks going back for the first no separator char. See #test24AddingParameterToSendersTakeCaresOfSeparators
	It assumes that there is always going to be a no separator wich holds due to how aStartPosition is obtained - Hernan"
	
	^self firstNot: [ :aChar | aChar isSeparator ] indexIn: sourceCode startingFrom: aStartingPosition ! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 1/7/2019 13:43:36'!
firstNot: aBlock indexIn: sourceCode startingFrom: aStartingPosition
	
	| noSeparatorIndex |
	
	noSeparatorIndex := aStartingPosition.
	[ noSeparatorIndex > 0 and: [ aBlock value: (sourceCode at: noSeparatorIndex) ]] whileTrue: [ noSeparatorIndex := noSeparatorIndex - 1 ].
	
	^noSeparatorIndex 
		! !

!AddParameter methodsFor: 'create new implementors - private' stamp: 'HAW 11/11/2018 14:07:55'!
implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource insertionPoint |

	implementorMethodNode := anImplementor methodNode.
	insertionPoint := isAddingLast 
		ifTrue: [ implementorMethodNode selectorLastPosition ]
		ifFalse: [ (implementorMethodNode selectorKeywordPositionAt: index) first - 1].

	originalSource := anImplementor sourceCode.
	insertionPoint := self firstNoLineSeparatorIndexIn: originalSource startingFrom: insertionPoint.
	
	newSource := String streamContents: [ :newSourceStream |
		newSourceStream 
			nextPutAll: (originalSource copyFrom: 1 to: insertionPoint);
			nextPutAll: implementorTrailingString;
			nextPutAll: (originalSource copyFrom: insertionPoint+1 to: originalSource size) ].
		
	^newSource! !


!AddParameter class methodsFor: 'instance creation' stamp: 'HAW 9/4/2018 19:59:06'!
named: aNewParameter
	at: anIndex 
	initializedWith: aNewParameterValue
	using: aNewKeyword 
	toKeywordSelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	| validNewParameterValue isAddingLast numberOfParameters |
	
	self assertIsKeyword: anOldSelector.
	self assertIsValidKeywordForNewParameter: aNewKeyword.
	validNewParameterValue := self assertNewParameterValueIsValid: aNewParameterValue.
	numberOfParameters := anOldSelector numArgs.
	self assert: anIndex isValidIndexFor: numberOfParameters.
	isAddingLast := anIndex > numberOfParameters.
	
	^self 
		named: aNewParameter 
		at: anIndex
		addingLast: isAddingLast 
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: (self newSelectorAdding: aNewKeyword at: anIndex to: anOldSelector) 
		addingToImplementors: (self keywordImplementorTrailingFor: aNewKeyword and: aNewParameter addingLast: isAddingLast) 
		addingToSenders: (self keywordSenderTrailingFor: aNewKeyword and: validNewParameterValue addingLast: isAddingLast)
		implementors: implementorsCollection 
		senders: sendersCollection
! !

!AddParameter class methodsFor: 'instance creation' stamp: 'HAW 9/4/2018 19:32:45'!
named: aNewParameter 
	initializedWith: aNewParameterValue 
	toUnarySelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	| validNewParameterValue |
	
	self assertIsUnary: anOldSelector.
	validNewParameterValue := self assertNewParameterValueIsValid: aNewParameterValue.

	^self 
		named: aNewParameter 
		at: 1
		addingLast: true
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: (self newSelectorFromUnary: anOldSelector)
		addingToImplementors: (self unaryImplementorTrailingFor: aNewParameter)
		addingToSenders: (self unarySenderTrailingFor: validNewParameterValue)
		implementors: implementorsCollection 
		senders: sendersCollection
! !

!AddParameter class methodsFor: 'instance creation' stamp: 'HAW 9/4/2018 17:26:10'!
named: aNewParameter 
	initializedWith: aNewParameterValue
	using: aNewKeyword 
	toKeywordSelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		at: anOldSelector numArgs + 1
		initializedWith: aNewParameterValue
		using: aNewKeyword 
		toKeywordSelector: anOldSelector  
		implementors: implementorsCollection 
		senders: sendersCollection
! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 19:05:41'!
keywordImplementorTrailingFor: aNewKeyword and: aNewParameter addingLast: isAddingLast

	^String streamContents: [ :stream |
		isAddingLast ifTrue: [ stream space ].
		stream
			nextPutAll: aNewKeyword;
			space;
			nextPutAll: aNewParameter;
			space ]! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 19:23:31'!
keywordSenderTrailingFor: aNewKeyword and: aNewParameterValue addingLast: isAddingLast

	^String streamContents: [ :stream |
		isAddingLast ifTrue: [ stream space ].
		stream
			nextPutAll: aNewKeyword;
			space;
			nextPutAll: aNewParameterValue.
		isAddingLast ifFalse: [ stream space ] ]

	! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 19:00:16'!
named: aNewParameter 
	at: anIndex
	addingLast: isAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector
	addingToImplementors: implementorTrailingString 
	addingToSenders: senderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	| trimmedNewParameter |
	
	trimmedNewParameter := aNewParameter withBlanksTrimmed.
	
	self assertCanAddParameterTo: anOldSelector.
	self assertIsValidParameterName: trimmedNewParameter.
	self assertNewParameter: trimmedNewParameter isNotDefinedAsInstanceVariableInAny: implementorsCollection.
	self assertNewParameter: trimmedNewParameter isNotDefinedAsLocalInAny: implementorsCollection.
	self assertAllImplementors: implementorsCollection haveSame: anOldSelector.
	self assertAllSenders: sendersCollection send: anOldSelector.
	
	^ self new 
		initializedNamed: trimmedNewParameter
		at: anIndex 
		addingLast: isAddingLast
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: aNewSelector
		addingToImplementors: implementorTrailingString 
		addingToSenders: senderTrailingString
		implementors: implementorsCollection 
		senders: sendersCollection
! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 21:32:44'!
newSelectorAdding: aNewKeyword at: anIndex to: anOldSelector

	| keywords |
	
	keywords := anOldSelector keywords asOrderedCollection. 
	keywords add: aNewKeyword beforeIndex: anIndex.
	
	^Symbol fromCollectionOfStrings: keywords.
	
	! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 8/18/2018 12:14:00'!
newSelectorFromUnary: anOldSelector

	^(anOldSelector, ':') asSymbol! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 8/18/2018 12:15:49'!
unaryImplementorTrailingFor: aNewParameter

	^ ': ', aNewParameter! !

!AddParameter class methodsFor: 'instance creation - private' stamp: 'HAW 8/18/2018 12:16:32'!
unarySenderTrailingFor: aNewParameterValue

	^ ': ', aNewParameterValue! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/25/2018 11:34:59'!
addParenthesisTo: trimmedNewParameterValue ifNewParameterValueIsKeywordMessage: newParameterValueMethodNode

	| newParameterNode |
	
	newParameterNode := newParameterValueMethodNode block statements first expr.
		
	^ (self hasToAddParenthesisBasedOn: newParameterNode)
		ifTrue: [ '(', trimmedNewParameterValue, ')' ]
		ifFalse: [ trimmedNewParameterValue ]
	! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/4/2018 20:08:37'!
assert: anIndex isValidIndexFor: aNumberOfParameters

	(anIndex isInteger and: [ (anIndex between: 1 and: aNumberOfParameters + 1)]) ifFalse: [ self signalInvalidParameterIndex: anIndex for: aNumberOfParameters ]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 16:02:12'!
assertCanAddParameterTo: aSelector

	aSelector isInfix ifTrue: [ self signalSelectorCanNotBeBinary]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 12:29:54'!
assertIsKeyword: aSelector

	aSelector isKeyword ifFalse: [ self signalSelectorMustBeKeyword]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 12:23:04'!
assertIsUnary: aSelector

	aSelector isUnary ifFalse: [ self signalSelectorMustBeUnary]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 16:30:01'!
assertIsValidKeywordForNewParameter: aNewKeyword

	(aNewKeyword isKeyword and: [aNewKeyword numArgs = 1 ]) ifFalse: [ self signalNotValidKeywordForNewParameter]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/17/2018 12:30:40'!
assertIsValidParameterName: aName

	| scannedNames |

	scannedNames _ Scanner new scanFieldNames: aName.
	scannedNames size = 1 ifFalse: [ self signalInvalidParameterName: aName ].
	scannedNames first = aName ifFalse: [ self signalInvalidParameterName: aName ].
! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/17/2018 16:59:29'!
assertNewParameter: aNewParameter isNotDefinedAsInstanceVariableInAny: implementors

	| classesDefiningInsVars |
	
	classesDefiningInsVars := self classesDefiningInstanceVariable: aNewParameter inAny: implementors.
	classesDefiningInsVars notEmpty ifTrue: [ self signalNewParameter: aNewParameter definedAsInstanceVariableIn: classesDefiningInsVars ]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/17/2018 16:59:47'!
assertNewParameter: aNewParameter isNotDefinedAsLocalInAny: implementors

	| implementorsDefiningNewParameterAsLocal |
	
	implementorsDefiningNewParameterAsLocal := implementors select: [ :implementor | implementor methodNode hasLocalNamed: aNewParameter ].
	implementorsDefiningNewParameterAsLocal notEmpty ifTrue: [ 
		self signalNewParameter: aNewParameter isDefinedAsLocalIn: implementorsDefiningNewParameterAsLocal ]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/25/2018 11:30:18'!
assertNewParameterValueCompiles: trimmedNewParameterValue

	^ [ Parser new parse: trimmedNewParameterValue readStream class: self noPattern: true notifying: nil ifFail: [nil] ]
		on: SyntaxErrorNotification 
		do: [ :error | self signalNewParameterValueDoesNotCompile ].! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/25/2018 11:31:42'!
assertNewParameterValueDoesNotHaveMoreThanOneStatement: newParameterValueMethodNode

	newParameterValueMethodNode block statements size = 1 ifFalse: [ self signalNewParameterValueCanNotHaveMoreThanOneStatement ].
	! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/17/2018 12:21:40'!
assertNewParameterValueIsNotEmpty: aNewParameterValue

	aNewParameterValue withBlanksTrimmed isEmpty ifTrue: [ self signalNewParameterValueCanNotBeEmpty]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/25/2018 11:33:38'!
assertNewParameterValueIsValid: aNewParameterValue

	| newParameterValueMethodNode trimmedNewParameterValue |
	
	trimmedNewParameterValue := aNewParameterValue withBlanksTrimmed.
	self assertNewParameterValueIsNotEmpty: trimmedNewParameterValue.

	newParameterValueMethodNode := self assertNewParameterValueCompiles: trimmedNewParameterValue.
	self assertNewParameterValueDoesNotHaveMoreThanOneStatement: newParameterValueMethodNode.
	
	^self addParenthesisTo: trimmedNewParameterValue ifNewParameterValueIsKeywordMessage: newParameterValueMethodNode.
	! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/25/2018 11:35:10'!
hasToAddParenthesisBasedOn: newParameterNode
	
	^ newParameterNode isMessageNode and: [  newParameterNode selector key isKeyword ]! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 15:05:29'!
classesDefiningInstanceVariable: aName inAny: implementors
 
	^ implementors 
		inject: Set new
		into: [ :classesDefiningInstVar :implementor | 
			(implementor methodClass whichClassDefinesInstanceVariable: aName ifNone: [ nil ]) ifNotNil: [ :classDefiningInstVar |
				classesDefiningInstVar add: classDefiningInstVar ].
			classesDefiningInstVar ]
! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 9/4/2018 20:03:25'!
errorMessageForInvalidParameterIndex: anIndex for: aNumberOfParameters

	^anIndex printString, ' is an invalid insertion index. It has to be between 1 and ', (aNumberOfParameters + 1) printString! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:31:35'!
errorMessageForInvalidParameterName: aName

	^ '''' , aName , ''' is not a valid parameter name'.! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 15:18:46'!
errorMessageForNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses

	^ String streamContents: [ :messageStream |
		messageStream 
			nextPutAll: definingClasses asCommaStringAnd;
			space;
			nextPutAll: (definingClasses size = 1 ifTrue: [ 'defines' ] ifFalse: [ 'define' ]);
			space;
			nextPutAll: aNewParameter;
			nextPutAll: ' as instance variable' ]! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 9/2/2018 19:59:27'!
errorMessageForNewParameterDefinedAsLocal: aNewParameter

	^ aNewParameter, ' is already defined as parameter or temporary'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:22:31'!
newParameterValueCanNotBeEmptyErrorMessage
	
	^'New parameter value can not be empty'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/25/2018 11:26:54'!
newParameterValueCanNotHaveMoreThanOneStatementErrorMessage
	
	^'New parameter value can not have more than one statement'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:24:53'!
newParameterValueDoesNotCompileErrorMessage
	
	^'New parameter value code does not compile'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 16:31:03'!
notValidKeywordForNewParameterErrorMessage
	
	^'New keyword must be of keyword type with one parameter'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 12/13/2018 17:46:33'!
selectorCanNotBeBinaryErrorMessage
	
	^'Can not add parameter to a binary selector.
Rename it to a keyword message first.'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:26:46'!
selectorMustBeKeywordErrorMessage
	
	^'Selector must be of keyword type'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:23:47'!
selectorMustBeUnaryErrorMessage
	
	^'Selector must be unary'! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 9/4/2018 20:02:24'!
signalInvalidParameterIndex: anIndex for: aNumberOfParameters

	self refactoringError: (self errorMessageForInvalidParameterIndex: anIndex for: aNumberOfParameters)! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:31:15'!
signalInvalidParameterName: aName

	self refactoringError: (self errorMessageForInvalidParameterName: aName)

! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 15:11:03'!
signalNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses

	self refactoringError: (self errorMessageForNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses)! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 9/2/2018 19:59:27'!
signalNewParameter: aNewParameter isDefinedAsLocalIn: implementors

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForNewParameterDefinedAsLocal: aNewParameter)
		references: (implementors collect: [ :implementor | MethodReference method: implementor ])
		to: aNewParameter 
	 ! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:22:07'!
signalNewParameterValueCanNotBeEmpty
	
	self refactoringError: self newParameterValueCanNotBeEmptyErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/25/2018 11:27:05'!
signalNewParameterValueCanNotHaveMoreThanOneStatement
	
	self refactoringError: self newParameterValueCanNotHaveMoreThanOneStatementErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 12:24:53'!
signalNewParameterValueDoesNotCompile
	
	self refactoringError: self newParameterValueDoesNotCompileErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 16:25:55'!
signalNotValidKeywordForNewParameter
	
	self refactoringError: self notValidKeywordForNewParameterErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:35:05'!
signalSelectorCanNotBeBinary
	
	self refactoringError: self selectorCanNotBeBinaryErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:29:54'!
signalSelectorMustBeKeyword
	
	self refactoringError: self selectorMustBeKeywordErrorMessage! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 8/18/2018 12:23:26'!
signalSelectorMustBeUnary
	
	self refactoringError: self selectorMustBeUnaryErrorMessage! !


!RemoveParameter methodsFor: 'initialization' stamp: 'HAW 9/4/2018 14:31:23'!
initializeNamed: aParameterToRemove
	ofKeywordAtIndex: aParameterIndex
	from: anOldSelector 
	creating: aNewSelector 
	implementors: implementorsCollection 
	senders: sendersCollection
	
	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.

	parameterToRemove := aParameterToRemove.
	parameterIndex := aParameterIndex.
	senderReplacementString := newSelector isUnary ifTrue: [ newSelector asString ] ifFalse: [ '' ].
	isLastParameter := oldSelector numArgs = parameterIndex 
	! !

!RemoveParameter methodsFor: 'create new implementors - private' stamp: 'HAW 9/4/2018 14:42:51'!
implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource parameterToRemovePosition selectorToRemovePosition |
	
	implementorMethodNode := anImplementor methodNode.
	selectorToRemovePosition := implementorMethodNode selectorKeywordPositionAt: parameterIndex.
	parameterToRemovePosition := implementorMethodNode parameterDefinitionPositionAt: parameterIndex.

	originalSource := anImplementor sourceCode.
	newSource := String streamContents: [ :newSourceStream | 
		self writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition.
		self writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition ].
		
	^newSource! !

!RemoveParameter methodsFor: 'create new implementors - private' stamp: 'HAW 9/4/2018 14:42:51'!
writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition
	
	| afterParameterPosition |
	
	afterParameterPosition := parameterToRemovePosition last.
	isLastParameter ifFalse: [ afterParameterPosition := self lastSeparatorIndexIn: originalSource startingFrom: afterParameterPosition ].
	
	newSourceStream nextPutAll: (originalSource copyFrom: afterParameterPosition + 1 to: originalSource size) 	! !

!RemoveParameter methodsFor: 'create new implementors - private' stamp: 'HAW 9/4/2018 14:35:27'!
writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition

	newSelector isUnary 
		ifTrue: [ newSourceStream nextPutAll: newSelector ]
		ifFalse: [ newSourceStream nextPutAll: (originalSource copyFrom: 1 to: selectorToRemovePosition first - 1) ].
! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 9/4/2018 17:42:53'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| keywordAndParameterPositions senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := aMethodNode messageSendKeywordAndParameterPositionsAt: parameterIndex of: oldSelector ifAbsent: [ #() ].
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition | | lastPosition |
		lastPosition := self lastSeparatorIndexIn: senderSourceCode startingFrom: aKeywordAndParameterPosition last.
		rangesToKeywords add: (aKeywordAndParameterPosition first to: lastPosition) -> senderReplacementString ]
	! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 9/4/2018 12:10:34'!
lastSeparatorIndexIn: senderSourceCode startingFrom: aPosition

	| lastPosition senderSourceCodeSize |
	
	lastPosition := aPosition.
	senderSourceCodeSize := senderSourceCode size.
	[ lastPosition := lastPosition + 1.
	lastPosition <= senderSourceCodeSize and: [ (senderSourceCode at: lastPosition) isSeparator ]] whileTrue. 

	^lastPosition - 1! !


!RemoveParameter class methodsFor: 'instance creation' stamp: 'HAW 9/4/2018 15:17:51'!
named: aParameterToRemove from: aMethod implementors: implementorsCollection senders: sendersCollection
	
	| oldSelector methodNode parameterIndex newSelector |
	
	oldSelector := aMethod selector.
	self assertCanRemoveParameterFrom: oldSelector.
	
	methodNode := aMethod methodNode.
	parameterIndex := self assert: aParameterToRemove isDefinedIn: methodNode.
	self assertAllImplementors: implementorsCollection haveSame: oldSelector.
	self assertAllSenders: sendersCollection send: oldSelector.
	self assertNoImplementorFrom: implementorsCollection reference: aParameterToRemove definedAt: parameterIndex.
	
	newSelector := self newSelectorFrom: oldSelector removingParameterAt: parameterIndex.
	
	^self new 
		initializeNamed: aParameterToRemove
		ofKeywordAtIndex: parameterIndex
		from: oldSelector 
		creating: newSelector 
		implementors: implementorsCollection 
		senders: sendersCollection
! !

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/2/2018 20:07:30'!
assert: aParamerterToRemove isDefinedIn: methodNode

	| parameterIndex |
	
	parameterIndex := methodNode arguments findFirst: [ :aParameterNode | aParameterNode name = aParamerterToRemove ].
	parameterIndex = 0 ifTrue: [ self signalParameterNotInMessage: aParamerterToRemove ].
	
	^parameterIndex 
		! !

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/4/2018 15:18:13'!
assertCanRemoveParameterFrom: oldSelector

	self assertIsNotUnary: oldSelector.
	self assertIsNotBinary: oldSelector.! !

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/1/2018 12:34:26'!
assertIsNotBinary: aSelector

	aSelector isInfix ifTrue: [ self signalCanNotRemoveParameterFromBinaryMessages]! !

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/1/2018 12:37:00'!
assertIsNotUnary: aSelector

	aSelector isUnary ifTrue: [ self signalCanNotRemoveParameterFromUnaryMessages]! !

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/4/2018 15:41:24'!
assertNoImplementorFrom: implementorsCollection reference: aParamerterToRemove definedAt: parameterIndex

	| implementorsReferencingParameter |
	
	implementorsReferencingParameter := implementorsCollection select: [:anImplementor | anImplementor referencesParameterAt: parameterIndex ].
	
	implementorsReferencingParameter isEmpty ifFalse: [ self signalCanNotRemove: aParamerterToRemove dueToReferencesIn: implementorsReferencingParameter ].! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 1/7/2019 15:31:35'!
canNotRemoveParameterFromBinaryMessagesErrorMessage
	
	^'Can not remove parameter from binary messages.
Rename message to a keyword one first'! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/1/2018 12:36:43'!
canNotRemoveParameterFromUnaryMessagesErrorMessage
	
	^'There is no parameter to remove in unary messages'! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/2/2018 19:40:52'!
errorMessageForParameterNotInMessage: aParameterToRemove

	^aParameterToRemove, ' is not define as parameter'! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/3/2018 16:27:21'!
errorMessageForParameterToRemoveIsReferenced: aParameterToRemove

	^aParameterToRemove, ' is being referenced in implementors'! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/2/2018 20:21:43'!
signalCanNotRemove: aParameterToRemove dueToReferencesIn: implementorsReferencingParameterToRemove

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForParameterToRemoveIsReferenced: aParameterToRemove)
		references: (implementorsReferencingParameterToRemove collect: [ :implementor | MethodReference method: implementor ])
		to: aParameterToRemove 
	 ! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/1/2018 12:34:45'!
signalCanNotRemoveParameterFromBinaryMessages
	
	self refactoringError: self canNotRemoveParameterFromBinaryMessagesErrorMessage! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/1/2018 12:37:00'!
signalCanNotRemoveParameterFromUnaryMessages
	
	self refactoringError: self canNotRemoveParameterFromUnaryMessagesErrorMessage! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 9/2/2018 19:40:22'!
signalParameterNotInMessage: aParameterToRemove

	self refactoringError: (self errorMessageForParameterNotInMessage: aParameterToRemove)! !

!RemoveParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 21:22:36'!
newSelectorConcatenating: oldSelectorKeywords removingAt: anIndex
	
	| keywords |
	
	keywords := oldSelectorKeywords asOrderedCollection.
	keywords removeIndex: anIndex.
	
	^Symbol fromCollectionOfStrings: keywords.! !

!RemoveParameter class methodsFor: 'instance creation - private' stamp: 'HAW 9/4/2018 21:30:47'!
newSelectorFrom: oldSelector removingParameterAt: parameterIndex

	| oldSelectorKeywords newSelector |
	
	oldSelectorKeywords := oldSelector keywords.
	
	newSelector := oldSelectorKeywords size = 1 
		ifTrue: [ oldSelector allButLast asSymbol ]
		ifFalse: [ self newSelectorConcatenating: oldSelectorKeywords removingAt: parameterIndex ].
	
	^newSelector! !


!RenameSelector methodsFor: 'create new implementors - private' stamp: 'HAW 9/3/2018 19:49:44'!
implementorNewSourceCodeOf: anImplementor

	|  newSource rangesToNewKeywords |
	
	rangesToNewKeywords := OrderedCollection new.
	anImplementor methodNode selectorKeywordsPositions withIndexDo: [ :aKeywordRange :index | 
		rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index) ].
		
	newSource := anImplementor sourceCode copyReplacing: rangesToNewKeywords.	
	^newSource! !

!RenameSelector methodsFor: 'rename senders - private' stamp: 'HAW 9/3/2018 19:47:38'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| oldSelectorKeywordsRanges |
	
	oldSelectorKeywordsRanges := aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].
	oldSelectorKeywordsRanges do: [ :aMessageSendSelectorRanges | 
		aMessageSendSelectorRanges withIndexDo: [ :aRange :index | rangesToKeywords add: aRange -> (newSelectorKeywords at: index) ]].
	! !

!RenameSelector methodsFor: 'initialization' stamp: 'HAW 9/3/2018 17:14:47'!
initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	
	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.
	
	newSelectorKeywords := newSelector keywords.	
	! !


!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 5/25/2017 20:02:05'!
assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector

	aNewSelector numArgs ~= anOldSelector numArgs ifTrue: [ self signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector ]! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2017 00:41:25'!
assert: aNewSelector isNotEqualTo: anOldSelector

	 aNewSelector = anOldSelector ifTrue: [ self signalNewSelectorEqualToOldSelector]! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 12/13/2018 18:53:51'!
assert: aNewSelector isOfSameTypeAs: anOldSelector

	(self isRenamigBetweenBinary: anOldSelector andKeywordOfOneParameter: aNewSelector) ifTrue: [ ^self ].
	(self isRenamigBetweenBinary: aNewSelector andKeywordOfOneParameter: anOldSelector) ifTrue: [ ^self ].

	aNewSelector precedence ~= anOldSelector precedence ifTrue: [ 
		self signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector ]! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 6/3/2017 11:54:48'!
assertIsNotEmpty: aCollection signalMessageText: errorMessageText

	aCollection isEmpty ifTrue: [ self refactoringError: errorMessageText ].! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2017 00:44:37'!
assertIsValidToRenameFrom: anOldSelector to: aNewSelector
	
	self assertIsNotEmpty: anOldSelector signalMessageText: self oldSelectorCanNotBeEmptyErrorMessage.
	self assertIsNotEmpty: aNewSelector signalMessageText: self newSelectorCanNotBeEmptyErrorMessage.
	self assert: aNewSelector isNotEqualTo: anOldSelector.
	self assert: aNewSelector isOfSameTypeAs: anOldSelector.
	self assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector.
	! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 5/31/2017 19:38:12'!
assertNoImplementorClassIn: implementorsCollection implements: aNewSelector

	| classesImplementingNewSelector |
	
	classesImplementingNewSelector := implementorsCollection 
		select: [ :anImplementor | anImplementor methodClass includesSelector: aNewSelector ]
		thenCollect: [ :anImplementor | anImplementor methodClass ].
		
	classesImplementingNewSelector notEmpty ifTrue: [ self signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector ]! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 12/13/2018 18:52:19'!
isRenamigBetweenBinary: aPotentiallyBinarySelector andKeywordOfOneParameter: aPotentiallyKeywordSelector

	^aPotentiallyBinarySelector isInfix 
		and: [ aPotentiallyKeywordSelector isKeyword
		and: [ aPotentiallyKeywordSelector numArgs = 1 ]]
! !

!RenameSelector class methodsFor: 'pre-conditions' stamp: 'HAW 5/31/2017 20:56:22'!
warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: implementorsCollection

	implementorsCollection do: [:anImplementor |
		anImplementor methodClass 
			withSuperclassThatIncludesSelector: aNewSelector 
			do: [ :aSuperclass | self warnImplementionOf: aNewSelector in: anImplementor methodClass willOverrideImplementationIn: aSuperclass ]
			ifNone: []]! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/25/2017 19:59:44'!
errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector

	^'New selector #', aNewSelector, ' does not have the same number of arguments as #', anOldSelector ! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/31/2017 19:41:36'!
errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector

	^'Can not rename because #', aNewSelector, ' is implemented in: ', classesImplementingNewSelector asCommaStringAnd ! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/25/2017 19:54:05'!
errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector

	^'New selector #', aNewSelector, ' is not of same type as #', anOldSelector ! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/25/2017 18:51:46'!
implementorsCanNotBeEmptyErrorMessage
	
	^'There are no methods to rename'! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/25/2017 18:49:15'!
newSelectorCanNotBeEmptyErrorMessage
	
	^'New selector can not be empty'! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/26/2017 00:40:01'!
newSelectorEqualToOldSelectorErrorMessage
	
	^'There is nothing to rename when new selector is equals to old selector'! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 5/25/2017 18:49:07'!
oldSelectorCanNotBeEmptyErrorMessage
	
	^'Old selector can not be empty'! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:55:23'!
signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector
	
	self refactoringError: (self errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector).! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:55:38'!
signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector
	
	self refactoringError: (self errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector).! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:55:54'!
signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector

	self refactoringError: (self errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector).! !

!RenameSelector class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:55:58'!
signalNewSelectorEqualToOldSelector

	self refactoringError: self newSelectorEqualToOldSelectorErrorMessage.! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/31/2017 19:58:50'!
from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	self assertIsValidToRenameFrom: anOldSelector to: aNewSelector.
	self assertIsNotEmpty: aCollectionOfImplementors signalMessageText: self implementorsCanNotBeEmptyErrorMessage.
	self assertAllImplementors: aCollectionOfImplementors haveSame: anOldSelector.
	self assertNoImplementorClassIn: aCollectionOfImplementors implements: aNewSelector.
	self assertAllSenders: aCollectionOfSenders send: anOldSelector.
	
	self warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: aCollectionOfImplementors.
	
	^self new initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/26/2017 00:04:36'!
from: anOldSelector to: aNewSelector in: aClassToRefactor

	| implementors senders |
	
	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aClassToRefactor theNonMetaClass.
	
	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders ! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/30/2017 17:45:16'!
from: anOldSelector to: aNewSelector inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	| implementors senders |
	
	implementors := IdentitySet new.
	senders := IdentitySet new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization.
	
	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders 
! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/25/2017 23:59:19'!
from: anOldSelector to: aNewSelector inCategoryOf: aClass organizedBy: anOrganization

	| implementors senders |
	
	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aClass category organizedBy: anOrganization.
	
	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders 
! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/25/2017 23:53:57'!
from: anOldSelector to: aNewSelector inHierarchyOf: aClass 
	
	| implementors senders |
	
	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass.
	
	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders 
	
	! !

!RenameSelector class methodsFor: 'instance creation' stamp: 'HAW 5/30/2017 17:47:27'!
from: anOldSelector to: aNewSelector inSystem: aSystem

	| implementors senders |
	
	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem.
		
	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders 
	! !

!RenameSelector class methodsFor: 'warnings' stamp: 'HAW 6/3/2017 12:01:34'!
warnImplementionOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass
	
	self refactoringWarning: (self warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass).! !

!RenameSelector class methodsFor: 'warnings' stamp: 'HAW 8/1/2018 18:15:01'!
warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass

	^'Implemention of #', aNewSelector, ' in ', aClass name, ' will override implementation in ', aSuperclass name! !


!ExtractToTemporary methodsFor: 'applying' stamp: 'HAW 10/18/2017 18:39:22'!
apply
	
	| temporaries finder rewriter |
	
	newTemporary := methodNodeToRefactor encoder bindTemp: newVariable.
	temporaries := methodNodeToRefactor temporaries asOrderedCollection.
	temporaries add: newTemporary.
	
	methodNodeToRefactor temporaries: temporaries.
	finder := ParseNodeToReplaceFinder of: self.
	methodNodeToRefactor accept: finder.
	rewriter := ExtractToTemporaryRewriter of: self on: finder.
	methodNodeToRefactor accept: rewriter.
	finder blockContainingFirstNodeToReplace statements 
		add: (AssignmentNode new variable: newTemporary value: parseNodeToExtract) 
		beforeIndex: finder firstNodeToReplaceIndex. 
	
	^methodNodeToRefactor ! !

!ExtractToTemporary methodsFor: 'initialization' stamp: 'HAW 10/4/2017 17:43:11'!
initializeNamed: aNewVariable with: aParseNodeToExtract in: aMethodNodeToRefactor
 
	newVariable := aNewVariable.
	parseNodeToExtract := aParseNodeToExtract.
	methodNodeToRefactor := aMethodNodeToRefactor ! !

!ExtractToTemporary methodsFor: 'accessing' stamp: 'HAW 10/4/2017 18:45:19'!
newTemporary
	
	^newTemporary ! !

!ExtractToTemporary methodsFor: 'accessing' stamp: 'HAW 10/4/2017 18:38:13'!
parseNodeToExtract
	
	^parseNodeToExtract! !

!ExtractToTemporary methodsFor: 'testing' stamp: 'HAW 10/18/2017 18:08:31'!
shouldExtract: aParseNode

	^parseNodeToExtract = aParseNode ! !

!ExtractToTemporary methodsFor: 'as yet unclassified' stamp: 'HAW 9/11/2018 15:52:26'!
research

"
TextEditor>>selectionInterval
hasSelection
selection
"! !


!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/30/2017 06:25:21'!
assert: aSourceCodeToExtract isIncludedIn: aMethodNodeToRefactor

	(aMethodNodeToRefactor sourceText includesSubString: aSourceCodeToExtract) ifFalse:  [
		self signalMethodNodeToRefactorDoesNotInclude: aSourceCodeToExtract ]
	
! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/26/2017 16:30:52'!
assert: aNewVariable isNotDefinedIn: aMethodNode 
	
	(aMethodNode tempNames includes: aNewVariable) ifTrue: [ self signalNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode ]
	! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/26/2017 16:23:25'!
assertIsNotEmpty: aNewVariable

	aNewVariable isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty ]! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 3/6/2018 16:01:34'!
assertIsNotReturn: aSourceCodeToExtract

	"I have to use the source code to check this because parsing it returns the same methodNode for
	1 and ^1, that is, using methodNode does not help to distigished if there was or not a return - Hernan"
	
	aSourceCodeToExtract withBlanksTrimmed first = $^ ifTrue: [ self signalSourceCodeToExtractCanNotIncludeReturn ]! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/27/2017 17:52:29'!
assertIsOneStatement: aMethodNodeToExtract

	aMethodNodeToExtract block statements size ~= 1 ifTrue: [ self signalColaborationToExtractHasToBeOneStatement]! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/26/2017 16:27:32'!
assertIsValidVariableName: aName

	| scannedNames |

	scannedNames _ Scanner new scanFieldNames: aName.
	scannedNames size = 1 ifFalse: [ self signalInvalidTemporaryVariable: aName ].
	scannedNames first = aName ifFalse: [ self signalInvalidTemporaryVariable: aName ].! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/30/2017 06:21:22'!
assertSourceCodeIsNotEmpty: aSourceCodeToExtract

	aSourceCodeToExtract isEmpty ifTrue: [ self signalSourceCodeToExtractCanNotBeEmpty]! !

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/26/2017 16:33:25'!
warnIf: aNewVariable isDefinedAsInstanceVariableInHierarchyOf: aClass 
	
	| classDefiningNewVariable |
	
	classDefiningNewVariable := aClass whichClassDefinesInstanceVariable: aNewVariable ifNone: [ ^self ].
	self warn: aNewVariable willHideInstanceVariableDefinedIn: classDefiningNewVariable ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:28:07'!
errorMessageForInvalidTemporaryVariable: aName

	^ '''' , aName , ''' is not a valid temporary variable name'.! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:31:23'!
errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode

	^aNewVariable, ' is already defined in ', aMethodNode classAndSelector ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:34:01'!
errorMessageForSourceCodeToExtractHasSyntaxError: anErrorDescription

	^'Can not extract a source code with syntax error: ', anErrorDescription ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:26:23'!
errorMessageMethodNodeToRefactorDoesNotInclude: aSourceCodeToExtract

	^'The source code {', aSourceCodeToExtract, '} is not included in the method to refactor'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:24:10'!
newVariableCanNotBeEmptyErrorMessage
	
	^'New variable can not be empty'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:34:15'!
signalColaborationToExtractHasSyntaxError: aSyntaxErrorNotification 
	
	self refactoringError: (self errorMessageForSourceCodeToExtractHasSyntaxError: aSyntaxErrorNotification messageText)! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:32:50'!
signalColaborationToExtractHasToBeOneStatement
	
	self refactoringError: self sourceCodeToExtractHasToBeOneStatementErrorMessage ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:27:51'!
signalInvalidTemporaryVariable: aName

	^ self refactoringError: (self errorMessageForInvalidTemporaryVariable: aName).! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/27/2017 17:23:04'!
signalMethodNodeToRefactorDoesNotInclude: aColaborationToExtract

	^self refactoringError: (self errorMessageMethodNodeToRefactorDoesNotInclude: aColaborationToExtract)! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:31:08'!
signalNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode

	self refactoringError: (self errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode )! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/26/2017 16:23:49'!
signalNewVariableCanNotBeEmpty
	
	self refactoringError: self newVariableCanNotBeEmptyErrorMessage! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:20:49'!
signalSourceCodeToExtractCanNotBeEmpty
	
	self refactoringError: self sourceCodeToExtractCanNotBeEmptyErrorMessage! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:24:37'!
signalSourceCodeToExtractCanNotIncludeReturn
	
	self refactoringError: self sourceCodeToExtractCanNotIncludeReturnErrorMessage ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:20:21'!
sourceCodeToExtractCanNotBeEmptyErrorMessage
	
	^'Source code to extract can not be empty'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:24:02'!
sourceCodeToExtractCanNotIncludeReturnErrorMessage
	
	^'A return can not be extracted'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:32:22'!
sourceCodeToExtractHasToBeOneStatementErrorMessage
	
	^'Can not extract more than one statement'! !

!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 9/11/2018 16:13:23'!
named: aNewVariable at: anIntervalToExtract from: aMethodSourceCode in: aClass

	|  trimmedNewVariable parseNodeToExtract trimmedSourceCodeToExtract methodNodeToExtract methodNodeToRefactor sourceCodeToExtract |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	methodNodeToRefactor := aClass methodNodeFor: aMethodSourceCode.
	
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: methodNodeToRefactor.
	
	sourceCodeToExtract := aMethodSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract. 
	self assert: trimmedSourceCodeToExtract isIncludedIn: methodNodeToRefactor.

	[ methodNodeToExtract := Parser parse: trimmedSourceCodeToExtract class: methodNodeToRefactor methodClass noPattern: true ]
		on: SyntaxErrorNotification 
		do: [ :anError | self signalColaborationToExtractHasSyntaxError: anError ].
	
	parseNodeToExtract := self paseNodeToExtractFrom: methodNodeToExtract. 

	self warnIf: trimmedNewVariable isDefinedAsInstanceVariableInHierarchyOf: methodNodeToRefactor methodClass.

	^self new initializeNamed: trimmedNewVariable with: parseNodeToExtract in: methodNodeToRefactor
 
	! !

!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 10/4/2017 17:41:59'!
named: aNewVariable with: aSourceCodeToExtract in: aMethodNodeToRefactor

	|  trimmedNewVariable parseNodeToExtract trimmedSourceCodeToExtract methodNodeToExtract |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: aMethodNodeToRefactor.
	
	trimmedSourceCodeToExtract := aSourceCodeToExtract withBlanksTrimmed.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract. 
	self assert: trimmedSourceCodeToExtract isIncludedIn: aMethodNodeToRefactor.

	[ methodNodeToExtract := Parser parse: trimmedSourceCodeToExtract class: aMethodNodeToRefactor methodClass noPattern: true ]
		on: SyntaxErrorNotification 
		do: [ :anError | self signalColaborationToExtractHasSyntaxError: anError ].
	
	parseNodeToExtract := self paseNodeToExtractFrom: methodNodeToExtract. 

	self warnIf: trimmedNewVariable isDefinedAsInstanceVariableInHierarchyOf: aMethodNodeToRefactor methodClass.

	^self new initializeNamed: trimmedNewVariable with: parseNodeToExtract in: aMethodNodeToRefactor
 
	! !

!ExtractToTemporary class methodsFor: 'method node to extract' stamp: 'HAW 6/27/2017 17:58:04'!
paseNodeToExtractFrom: aMethodNodeToExtract

	self assertIsOneStatement: aMethodNodeToExtract. 
	
	^aMethodNodeToExtract block statements first expr.
	! !

!ExtractToTemporary class methodsFor: 'warnings' stamp: 'HAW 6/26/2017 16:34:44'!
warn: aNewVariable willHideInstanceVariableDefinedIn: aClass 

	self refactoringWarning: (self warningMessageFor: aNewVariable willHideInstanceVariableDefinedIn: aClass)! !

!ExtractToTemporary class methodsFor: 'warnings' stamp: 'HAW 6/26/2017 16:35:04'!
warningMessageFor: aNewVariable willHideInstanceVariableDefinedIn: aClass

	^aNewVariable, ' will hide instance variable defined in ', aClass name! !


!InsertSuperclass methodsFor: 'applying' stamp: 'HAW 8/13/2018 18:31:33'!
apply

	| newSuperclass |
	
	newSuperclass := self createSuperclass.
	self changeSuperclassOf: classToRefactor to: newSuperclass.

	^newSuperclass ! !

!InsertSuperclass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 18:31:43'!
changeSuperclassOf: classToRefactor to: newSuperclass
	
	newSuperclass subclass: classToRefactor name
		instanceVariableNames: classToRefactor instanceVariablesString 
		classVariableNames: classToRefactor classVariablesString 
		poolDictionaries: classToRefactor sharedPoolsString
		category: classToRefactor category.! !

!InsertSuperclass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 18:31:06'!
createSuperclass
	
	^classToRefactor superclass subclass: superclassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: classToRefactor category.! !

!InsertSuperclass methodsFor: 'initialization' stamp: 'HAW 8/13/2018 17:36:34'!
initializeTo: aClass named: aSuperclassName 

	classToRefactor := aClass.
	superclassName := aSuperclassName.! !


!InsertSuperclass class methodsFor: 'instance creation' stamp: 'HAW 3/3/2019 09:11:36'!
newClassPreconditionClass

	^NewClassPrecondition ! !

!InsertSuperclass class methodsFor: 'instance creation' stamp: 'HAW 8/13/2018 17:34:37'!
to: aClass named: aSuperclassName
	
	^self to: aClass named: aSuperclassName in: Smalltalk undeclared: Undeclared! !

!InsertSuperclass class methodsFor: 'instance creation' stamp: 'HAW 3/3/2019 09:11:31'!
to: aClass named: aSuperclassName in: aSystem undeclared: anUndeclared

	self newClassPreconditionClass valueFor: aSuperclassName in: aSystem undeclared: anUndeclared.
	
	^self new initializeTo: aClass theNonMetaClass named: aSuperclassName ! !


!PushUpMethod methodsFor: 'initialization' stamp: 'HAW 8/18/2018 11:44:09'!
initializeFor: aMethodToPushup

	method := aMethodToPushup ! !

!PushUpMethod methodsFor: 'applying' stamp: 'HAW 3/4/2019 15:23:40'!
apply

	| methodCategory |
	
	methodCategory := method methodClass organization categoryOfElement: method selector.
	method methodClass superclass
		compile: method sourceCode 
		classified: methodCategory.
		
	method methodClass removeSelector: method selector.
 ! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 8/18/2018 11:43:53'!
for: aMethodToPushUp

	^self new initializeFor: aMethodToPushUp ! !


!RemoveAllUnreferencedInstanceVariables methodsFor: 'initialization' stamp: 'HAW 8/2/2018 16:14:57'!
initializeFrom: aClassToRefactor

	classToRefactor := aClassToRefactor ! !

!RemoveAllUnreferencedInstanceVariables methodsFor: 'applying' stamp: 'HAW 8/2/2018 16:05:05'!
apply
	
	| variableNamesToRemove |
	
	variableNamesToRemove := classToRefactor unreferencedInstanceVariables.
	variableNamesToRemove do: [ :aVariableName | classToRefactor removeInstVarName: aVariableName ].
	
	^variableNamesToRemove! !


!RemoveAllUnreferencedInstanceVariables class methodsFor: 'instance creation' stamp: 'HAW 8/2/2018 16:14:45'!
from: aClassToRefactor

	^self new initializeFrom: aClassToRefactor ! !


!RemoveInstanceVariable methodsFor: 'applying' stamp: 'HAW 6/17/2017 19:46:45'!
apply
	
	classToRefactor removeInstVarName: variableToRemove ! !

!RemoveInstanceVariable methodsFor: 'initialization' stamp: 'HAW 6/17/2017 19:45:18'!
initializeNamed: aVariable from: aClassToRefactor 

	variableToRemove := aVariable.
	classToRefactor := aClassToRefactor ! !


!RemoveInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 6/12/2017 19:11:50'!
assert: aClass defines: anInstanceVariable

	(aClass definesInstanceVariableNamed: anInstanceVariable) ifFalse: [ self signalInstanceVariable: anInstanceVariable notDefinedIn: aClass ].! !

!RemoveInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 8/1/2018 19:36:10'!
assert: aVaraible isNotReferencedInHierarchyOf: aClassToRefactor

	| references |
	
	references := OrderedCollection new.
	aClassToRefactor withAllSubclassesDo: [ :aClass | 
		(aClass whichSelectorsAccess: aVaraible) do: [ :aSelector | references add: (MethodReference class: aClass selector: aSelector) ]].
	
	references notEmpty ifTrue: [ self signalInstanceVariable: aVaraible isReferencedInAll: references ]! !

!RemoveInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 8/1/2018 19:38:57'!
errorMessageForInstanceVariable: aVariable isReferencedInAll: methods

	^aVariable, ' can not be removed because it has references'.
		! !

!RemoveInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 6/12/2017 19:12:41'!
errorMessageForInstanceVariable: aName notDefinedIn: aClass

	^ 'Instance variable ''' , aName , ''' is not defined in ' , aClass name.! !

!RemoveInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 16:34:16'!
signalInstanceVariable: aVariable isReferencedInAll: methods

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForInstanceVariable: aVariable isReferencedInAll: methods)
		 references: methods
		 to: aVariable! !

!RemoveInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 6/12/2017 19:12:12'!
signalInstanceVariable: aName notDefinedIn: aClass

	^ self refactoringError: (self errorMessageForInstanceVariable: aName notDefinedIn: aClass).! !

!RemoveInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 6/17/2017 19:44:39'!
named: aVariable from: aClassToRefactor 
	
	self assert: aClassToRefactor defines: aVariable.
	self assert: aVariable isNotReferencedInHierarchyOf: aClassToRefactor.
	
	^self new initializeNamed: aVariable from: aClassToRefactor 
! !


!RenameClass methodsFor: 'applying' stamp: 'HAW 6/4/2017 18:09:21'!
apply
	
	classToRename safeRenameTo: newClassName.
	^self renameReferences.
	
	! !

!RenameClass methodsFor: 'initialization' stamp: 'HAW 8/9/2018 15:40:00'!
initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	classToRename := aClass.
	classToRenameOriginalName := aClass name.
	newClassName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary.
	
	! !

!RenameClass methodsFor: 'accessing' stamp: 'HAW 6/4/2017 18:16:33'!
newClassName
	
	^newClassName ! !

!RenameClass methodsFor: 'accessing' stamp: 'HAW 6/4/2017 18:24:56'!
referencesToOldClass
	
	^system allCallsOn: newClassName! !

!RenameClass methodsFor: 'accessing' stamp: 'HAW 6/4/2017 18:25:56'!
referencesToOldClassName
	
	^system allCallsOn: classToRenameOriginalName! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 15:27:46'!
newSourceCodeOf: aCompiledMethod 
	
	| newSource |
	
	newSource := aCompiledMethod sourceCode copyReplacing: (self rangesToReplaceOf: aCompiledMethod) with: newClassName.

	^newSource! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 9/3/2018 19:48:30'!
rangesForLiteralOf: methodNode
	
	| literalRanges |
	
	literalRanges := methodNode positionsForLiteralNode: classToRenameOriginalName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].
	
	^literalRanges ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 9/3/2018 19:48:49'!
rangesForLiteralVariableOf: methodNode
	
	^methodNode positionsForLiteralVariableNode: classToRenameOriginalName ifAbsent: [ #() ]
	! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 15:20:40'!
rangesToReplaceOf: aCompiledMethod 
	
	| methodNode ranges |
	
	methodNode := aCompiledMethod methodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).
	
	^ranges ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/9/2018 14:50:34'!
references: aMethodReference classVarNamed: aName

	^aMethodReference actualClass definesClassVariableNamedInHierarchy: aName ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/9/2018 14:50:52'!
referencesNewClassName: aMethodReference

	^self references: aMethodReference classVarNamed: newClassName ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/9/2018 14:51:07'!
referencesOldClassName: aMethodReference

	^self references: aMethodReference classVarNamed: classToRenameOriginalName! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/9/2018 14:49:43'!
rejectReferencesToClassVariablesFrom: references
	
	^references reject: [ :aMethodReference | (self referencesOldClassName: aMethodReference) or: [ self referencesNewClassName: aMethodReference ] ].! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 15:18:43'!
renameReference: aReferencingMethod 
	
	| newSource |
	
	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 15:17:56'!
renameReferences
	
	| references |
	
	references := (self referencesToOldClass asSet, self referencesToOldClassName asSet) asOrderedCollection.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	
	^references! !


!RenameClass class methodsFor: 'pre-conditions' stamp: 'HAW 6/1/2017 19:06:21'!
assert: aClass isNotNamed: aNewName

	aClass name = aNewName ifTrue: [ self signalNewNameEqualsOldName]! !

!RenameClass class methodsFor: 'pre-conditions' stamp: 'HAW 6/4/2017 18:49:54'!
assertIsNotMeta: aBehavior

	aBehavior isMeta ifTrue: [ self signalClassToRenameCanNotBeMetaclass]! !

!RenameClass class methodsFor: 'pre-conditions' stamp: 'HAW 8/13/2018 18:45:26'!
newClassPreconditionClass

	^NewClassPrecondition ! !

!RenameClass class methodsFor: 'exceptions' stamp: 'HAW 6/4/2017 18:50:36'!
classToRenameCanNotBeMetaclassErrorMessage
	
	^'Class to rename can not be a metaclass'! !

!RenameClass class methodsFor: 'exceptions' stamp: 'HAW 6/1/2017 19:07:08'!
newNameEqualsOldNameErrorMessage
	
	^'New class name equals old one'! !

!RenameClass class methodsFor: 'exceptions' stamp: 'HAW 6/4/2017 18:50:56'!
signalClassToRenameCanNotBeMetaclass
	
	self refactoringError: self classToRenameCanNotBeMetaclassErrorMessage! !

!RenameClass class methodsFor: 'exceptions' stamp: 'HAW 8/13/2018 18:39:00'!
signalNewNameEqualsOldName

	self refactoringError: self newNameEqualsOldNameErrorMessage.! !

!RenameClass class methodsFor: 'instance creation' stamp: 'HAW 6/2/2017 11:43:48'!
from: aClass to: aNewClassName

	^self from: aClass to: aNewClassName in: Smalltalk
	! !

!RenameClass class methodsFor: 'instance creation' stamp: 'HAW 6/2/2017 11:55:32'!
from: aClass to: aNewClassName in: aSystem

	^self from: aClass to: aNewClassName in: aSystem undeclared: Undeclared 
	
	! !

!RenameClass class methodsFor: 'instance creation' stamp: 'HAW 8/13/2018 18:45:15'!
from: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	self assertIsNotMeta: aClass.
	self assert: aClass isNotNamed: aNewClassName.
	self newClassPreconditionClass valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary.
	
	^self new initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 
! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 6/11/2017 19:06:35'!
addNewInstanceVariable

	addInstanceVariable apply! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 6/11/2017 19:07:49'!
copyOldInstanceVariableToNewOne

	| oldVariableIndex newVariableIndex |
	
	oldVariableIndex := self classToRefactor indexOfInstanceVariable: oldVariable.	
	newVariableIndex := self classToRefactor indexOfInstanceVariable: self newVariable.
	self classToRefactor allSubInstancesDo: [ :anInstance | anInstance instVarAt: newVariableIndex put: (anInstance instVarAt: oldVariableIndex) ].
	
! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 9/3/2018 19:48:09'!
newSourceOf: aCompiledMethod 
	
	| newSource ranges |
	
	ranges := aCompiledMethod methodNode positionsForInstanceVariable: oldVariable ifAbsent: [ #() ].
	newSource := aCompiledMethod sourceCode copyReplacing: ranges with: self newVariable.
	
	^newSource
	! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 6/11/2017 19:07:20'!
removeOldInstanceVariable

	self classToRefactor removeInstVarName: oldVariable.! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 6/11/2017 19:07:10'!
renameReferencesToOldVariable
	
	renamedReferences := OrderedCollection new.
	self classToRefactor withAllSubclassesDo: [ :aClass | self renameReferencesToOldVariableInClass: aClass ]! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 6/17/2017 19:17:33'!
renameReferencesToOldVariableInClass: aClass 

	| referencingMethodNames |
	
	referencingMethodNames := aClass whichSelectorsAccess: oldVariable.
	referencingMethodNames do: [ :referencingMethodName | self renameReferencesToOldVariableInMethod: (aClass compiledMethodAt: referencingMethodName) ]
	! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 8/13/2018 14:53:56'!
renameReferencesToOldVariableInMethod: aCompiledMethod 
	
	aCompiledMethod methodClass compile: (self newSourceOf: aCompiledMethod).
	renamedReferences add: aCompiledMethod methodReference ! !

!RenameInstanceVariable methodsFor: 'applying' stamp: 'HAW 6/5/2017 16:53:57'!
apply

	self 
		addNewInstanceVariable;
		copyOldInstanceVariableToNewOne;
		renameReferencesToOldVariable;
		removeOldInstanceVariable.
		
	^renamedReferences 
		! !

!RenameInstanceVariable methodsFor: 'accessing' stamp: 'HAW 6/11/2017 19:09:35'!
classToRefactor

	^addInstanceVariable classToRefactor! !

!RenameInstanceVariable methodsFor: 'accessing' stamp: 'HAW 6/11/2017 19:07:37'!
newVariable

	^ addInstanceVariable newVariable! !

!RenameInstanceVariable methodsFor: 'initialization' stamp: 'HAW 8/9/2018 17:20:55'!
initializeFrom: anOldvariable addingWith: anAddInstanceVariable 

	oldVariable := anOldvariable.
	addInstanceVariable := anAddInstanceVariable.
! !


!RenameInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 5/24/2017 21:49:18'!
assert: aClass defines: anInstanceVariable

	(aClass definesInstanceVariableNamed: anInstanceVariable) ifFalse: [ self signalInstanceVariable: anInstanceVariable notDefinedIn: aClass ].! !

!RenameInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 5/24/2017 21:56:02'!
errorMessageForInstanceVariable: aName notDefinedIn: aClass

	^ 'Instance variable ''' , aName , ''' is not defined in ' , aClass name.! !

!RenameInstanceVariable class methodsFor: 'exceptions' stamp: 'HAW 6/3/2017 11:54:22'!
signalInstanceVariable: aName notDefinedIn: aClass

	^ self refactoringError: (self errorMessageForInstanceVariable: aName notDefinedIn: aClass).! !

!RenameInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 6/11/2017 19:04:59'!
from: anOldvariable to: aNewVariable in: aClassToRefactor 
	
	| addInstanceVariable |
	
	self assert: aClassToRefactor defines: anOldvariable.
	addInstanceVariable := AddInstanceVariable named: aNewVariable to: aClassToRefactor.
	
	^self new initializeFrom: anOldvariable addingWith: addInstanceVariable ! !


!RenameTemporary methodsFor: 'applying' stamp: 'HAW 9/3/2018 19:49:06'!
apply
	
	| newSource ranges |
	
	ranges := methodNode positionsForTemporaryVariable: oldVariable ifAbsent: [ #() ].
	newSource := methodNode sourceText copyReplacing: ranges with: newVariable.
	
	^ newSource! !

!RenameTemporary methodsFor: 'applying' stamp: 'HAW 8/9/2018 19:34:51'!
methodNodeAfterApply
	
	^methodNode methodClass methodNodeFor: self apply.
	
	! !

!RenameTemporary methodsFor: 'initialization' stamp: 'HAW 6/25/2017 21:53:31'!
initializeFrom: anOldVariable to: aNewVariable in: aMethodNode 
	
	oldVariable := anOldVariable.
	newVariable := aNewVariable.
	methodNode := aMethodNode ! !


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/25/2017 21:53:31'!
assert: aVariable isDefinedIn: aMethodNode 
	
	(aMethodNode tempNames includes: aVariable) ifFalse: [ self signalTemporaryVariable: aVariable notDefinedIn: aMethodNode ]! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/25/2017 21:53:31'!
assert: aNewVariable isNotDefinedIn: aMethodNode 
	
	(aMethodNode tempNames includes: aNewVariable) ifTrue: [ self signalNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode ]
	! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/25/2017 21:53:31'!
assertIsNotEmpty: aNewVariable

	aNewVariable isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty ]! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/25/2017 21:53:31'!
assertIsValidVariableName: aName

	| scannedNames |

	scannedNames _ Scanner new scanFieldNames: aName.
	scannedNames size = 1 ifFalse: [ self signalInvalidTemporaryVariable: aName ].
	scannedNames first = aName ifFalse: [ self signalInvalidTemporaryVariable: aName ].! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 6/25/2017 21:53:31'!
warnIf: aNewVariable isDefinedAsInstanceVariableInHierarchyOf: aClass 
	
	| classDefiningNewVariable |
	
	classDefiningNewVariable := aClass whichClassDefinesInstanceVariable: aNewVariable ifNone: [ ^self ].
	self warn: aNewVariable willHideInstanceVariableDefinedIn: classDefiningNewVariable ! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
errorMessageForInvalidTemporaryVariable: aName

	^ '''' , aName , ''' is not a valid temporary variable name'.! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode

	^aNewVariable, ' is already defined in ', aMethodNode classAndSelector ! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 10/4/2017 17:04:37'!
errorMessageForTemporaryVariable: aVariable notDefinedIn: aMethodNode

	^'Temporary variable ', aVariable, ' is not defined in ', aMethodNode classAndSelector ! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
newVariableCanNotBeEmptyErrorMessage
	
	^'New variable can not be empty'! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
signalInvalidTemporaryVariable: aName

	^ self refactoringError: (self errorMessageForInvalidTemporaryVariable: aName).! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
signalNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode

	self refactoringError: (self errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode )! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/25/2017 21:53:31'!
signalNewVariableCanNotBeEmpty
	
	self refactoringError: self newVariableCanNotBeEmptyErrorMessage! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 10/4/2017 17:04:27'!
signalTemporaryVariable: aVariable notDefinedIn: aMethodNode

	self refactoringError: (self errorMessageForTemporaryVariable: aVariable notDefinedIn: aMethodNode)! !

!RenameTemporary class methodsFor: 'instance creation' stamp: 'HAW 6/25/2017 21:53:31'!
from: anOldVariable to: aNewVariable in: aMethodNode

	|  trimmedNewVariable |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	
	self assert: anOldVariable isDefinedIn: aMethodNode.
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: aMethodNode.
	
	self warnIf: trimmedNewVariable isDefinedAsInstanceVariableInHierarchyOf: aMethodNode methodClass.
	
	^self new initializeFrom: anOldVariable to: trimmedNewVariable in: aMethodNode
! !

!RenameTemporary class methodsFor: 'warnings' stamp: 'HAW 6/25/2017 21:53:31'!
warn: aNewVariable willHideInstanceVariableDefinedIn: aClass 

	self refactoringWarning: (self warningMessageFor: aNewVariable willHideInstanceVariableDefinedIn: aClass)! !

!RenameTemporary class methodsFor: 'warnings' stamp: 'HAW 6/25/2017 21:53:31'!
warningMessageFor: aNewVariable willHideInstanceVariableDefinedIn: aClass

	^aNewVariable, ' will hide instance variable defined in ', aClass name! !


!SafelyRemoveClass methodsFor: 'applying' stamp: 'HAW 8/1/2018 16:42:17'!
apply
	
	self removeWithAllSubclasses: classToRemove.
	! !

!SafelyRemoveClass methodsFor: 'applying - private' stamp: 'HAW 8/1/2018 16:42:17'!
removeWithAllSubclasses: aClassToRemove

	"I have to do 'subclasses do:' and not 'subclassesDo:' because removing a class modifies parent's subclasses collection.
	#subclasses returns a copy of superclass' subclasses collection -Hernan"
	aClassToRemove subclasses do: [ :aSubclassToRemove | self removeWithAllSubclasses: aSubclassToRemove ].
	aClassToRemove removeFromSystem.
	! !

!SafelyRemoveClass methodsFor: 'initialization' stamp: 'HAW 8/1/2018 16:42:17'!
initializeOf: aClassToSafetelyRemove 
	
	classToRemove := aClassToSafetelyRemove ! !


!SafelyRemoveClass class methodsFor: 'instance creation' stamp: 'HAW 8/1/2018 16:42:17'!
of: aClassToSafelyRemove

	| theNonMetaclassToRemove |
	
	theNonMetaclassToRemove := aClassToSafelyRemove theNonMetaClass.	
	self assertNoReferencesTo: theNonMetaclassToRemove.	
	self warnIfHasSubclasses: theNonMetaclassToRemove.
	
	^self new initializeOf: theNonMetaclassToRemove ! !

!SafelyRemoveClass class methodsFor: 'pre-conditions' stamp: 'HAW 8/1/2018 16:42:17'!
assertNoReferencesTo: aClassToSafelyRemove

	| references withAllSubclasses withAllSubclassesNames referenced |
	
	withAllSubclasses := aClassToSafelyRemove withAllSubclasses.
	withAllSubclassesNames := withAllSubclasses collect: [:aClass | aClass name ].
	references :=OrderedCollection new.
	referenced := OrderedCollection new.
	
	withAllSubclasses do: [ :aClass | | allReferences referencesOutsideHierarchy |
		allReferences := aClass allCallsOn.
		referencesOutsideHierarchy := allReferences reject: [ :aReference | withAllSubclassesNames includes: aReference classSymbol ].
		referencesOutsideHierarchy notEmpty ifTrue: [
			referenced add: aClass.
			references addAll: referencesOutsideHierarchy ]].
		
	references notEmpty ifTrue: [ self signalCanNotRemove: aClassToSafelyRemove dueToReferences: references toAll: referenced ]! !

!SafelyRemoveClass class methodsFor: 'pre-conditions' stamp: 'HAW 8/1/2018 16:42:17'!
warnIfHasSubclasses: aClassToSafelyRemove

	| allSubclasses |
	
	allSubclasses := aClassToSafelyRemove allSubclasses.
	allSubclasses isEmpty ifFalse: [ self warn: aClassToSafelyRemove hasSubclasses: allSubclasses ]! !

!SafelyRemoveClass class methodsFor: 'exceptions' stamp: 'HAW 8/1/2018 18:10:36'!
errorMessageForCanNotRemove: aClassToSafelyRemove dueToReferencesToAll: referenced

	^String streamContents: [ :stream |
		stream 
			nextPutAll: aClassToSafelyRemove name asString;
			nextPutAll: ' can not be removed '.
	
		referenced size = 1 
			ifTrue: [ (referenced includes: aClassToSafelyRemove) 
				ifTrue: [ stream nextPutAll: 'because it has references' ]
				ifFalse: [ stream 
					nextPutAll: 'because it subclass, ';
					nextPutAll: referenced anyOne name asString;
					nextPutAll: ', has references' ]]
			ifFalse: [ (referenced includes: aClassToSafelyRemove)
				ifTrue: [ stream 
					nextPutAll: 'due to references to: ';
					nextPutAll: referenced asCommaStringAnd ]
				ifFalse: [ stream
					nextPutAll: 'due to references to its subclasses: ';
					nextPutAll: referenced asCommaStringAnd ]]]! !

!SafelyRemoveClass class methodsFor: 'exceptions' stamp: 'HAW 8/17/2018 16:34:16'!
signalCanNotRemove: aClassToSafelyRemove dueToReferences: references toAll: allReferenced

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForCanNotRemove: aClassToSafelyRemove dueToReferencesToAll: allReferenced)
		references: references 
		to: aClassToSafelyRemove 
	! !

!SafelyRemoveClass class methodsFor: 'warnings' stamp: 'HAW 8/1/2018 16:42:17'!
warn: aClassToSafelyRemove hasSubclasses: allSubclasses

	self refactoringWarning: (self warningMessageFor: aClassToSafelyRemove hasSubclasses: allSubclasses)! !

!SafelyRemoveClass class methodsFor: 'warnings' stamp: 'HAW 8/1/2018 18:24:31'!
warningMessageFor: aClassToSafelyRemove hasSubclasses: allSubclasses

	^String streamContents: [ :stream |
		stream nextPutAll: aClassToSafelyRemove name asString.
		allSubclasses size = 1
			ifTrue: [ stream nextPutAll: ' has a subclass' ]
			ifFalse: [ stream 
					nextPutAll: ' has ';
					print:  allSubclasses size;
					nextPutAll: ' subclasses' ].
		stream nextPutAll: ' that will be removed'].
		
	! !


!RefactoringApplier methodsFor: 'refactoring - applying' stamp: 'HAW 6/5/2017 18:06:39'!
applyRefactoring

	changes := refactoring apply! !

!RefactoringApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/5/2017 18:05:46'!
createRefactoring

	self subclassResponsibility ! !

!RefactoringApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/5/2017 18:05:20'!
createRefactoringHandlingRefactoringExceptions

	self valueHandlingRefactoringExceptions: [ refactoring := self createRefactoring ]
	! !

!RefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 14:24:04'!
endRequest

	^requestExitBlock value! !

!RefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 19:04:16'!
requestRefactoringParameters

	self subclassResponsibility ! !

!RefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 19:05:35'!
requestRefactoringParametersHandlingRefactoringExceptions

	self valueHandlingRefactoringExceptions: [ self requestRefactoringParameters ]
	! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 8/17/2018 16:37:38'!
browseReferencesOn: aCanNotRefactorDueToReferencesError
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoSelect: true
! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 8/17/2018 16:37:55'!
handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError
	
	| options answer question |
	
	options := 
'Browse references
Cancel'.

	question := PopUpMenu labels: options icons: #(mailForwardIcon cancelIcon).
	answer := question startUpWithCaption: aCanNotRefactorDueToReferencesError messageText.
	
	answer = 1 ifTrue: [ self browseReferencesOn: aCanNotRefactorDueToReferencesError ].
	self endRequest.! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 6/5/2017 18:47:40'!
handleRefactoringError: aRefactoringError 

	self inform: aRefactoringError messageText.
	self endRequest ! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 8/1/2018 18:15:21'!
handleRefactoringWarning: aRefactoringWarning
	
	(self confirm: aRefactoringWarning messageText, '. Continue?')
		ifTrue: [ aRefactoringWarning resume ]
		ifFalse: [ self endRequest]! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 8/17/2018 16:38:07'!
referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError

	^'References to ', aCanNotRefactorDueToReferencesError referencee asString! !

!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 8/17/2018 16:35:47'!
valueHandlingRefactoringExceptions: aBlock

	^[[aBlock
		on: Refactoring refactoringWarningClass 
		do: [ :aRefactoringWarning | self handleRefactoringWarning: aRefactoringWarning ]]
		on: Refactoring canNotRefactorDueToReferencesErrorClass
		do: [ :aCanNotRefactorDueToReferencesError | self handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError ]]
		on: Refactoring refactoringErrorClass 
		do: [ :aRefactoringError | self handleRefactoringError: aRefactoringError ]
	! !

!RefactoringApplier methodsFor: 'request information' stamp: 'HAW 6/11/2017 19:22:50'!
request: aLabel
 
	^self request: aLabel initialAnswer: ''
! !

!RefactoringApplier methodsFor: 'request information' stamp: 'HAW 6/5/2017 16:03:04'!
request: aLabel initialAnswer: anAnswer

	^self request: aLabel initialAnswer: anAnswer onCancel: requestExitBlock ! !

!RefactoringApplier methodsFor: 'request information' stamp: 'HAW 6/5/2017 16:03:30'!
request: aLabel initialAnswer: anAnswer onCancel: cancelBlock

	^FillInTheBlankMorph request: aLabel initialAnswer: anAnswer onCancel: cancelBlock ! !

!RefactoringApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2017 18:06:53'!
showChanges

	self subclassResponsibility 	
	! !

!RefactoringApplier methodsFor: 'evaluating' stamp: 'HAW 6/5/2017 19:05:50'!
value

	requestExitBlock := [ ^self ].
	
	self 
		requestRefactoringParametersHandlingRefactoringExceptions;
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		showChanges
	
	! !


!RefactoringApplier class methodsFor: 'value handling exceptions' stamp: 'HAW 1/7/2019 15:28:18'!
createAndValueHandlingExceptions: creationBlock

	| refactoring |
	
	refactoring := creationBlock 
		on: Refactoring refactoringErrorClass 
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

	refactoring value ! !


!AddInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/30/2017 18:12:33'!
askNewVariableName
		
	newInstanceVariable := self request: self newVariableNameLabel. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed ! !

!AddInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/30/2017 18:12:47'!
newVariableNameLabel
	
	^'Enter new variable name:'! !

!AddInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/11/2017 19:18:00'!
requestRefactoringParameters

	self askNewVariableName! !

!AddInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/11/2017 19:19:25'!
createRefactoring
		
	^AddInstanceVariable named: newInstanceVariable to: classToRefactor.
	! !

!AddInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 1/7/2019 14:41:01'!
informChangesToBrowser
		
	browser acceptedContentsChanged! !

!AddInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/11/2017 19:20:03'!
showChanges

	self informChangesToBrowser! !

!AddInstanceVariableApplier methodsFor: 'initialization' stamp: 'HAW 1/7/2019 14:40:44'!
initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor ! !


!AddInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 1/7/2019 14:40:37'!
on: aBrowser for: aClassToRefactor
	
	^self new initializeOn: aBrowser for: aClassToRefactor 
! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/3/2019 07:57:24'!
askForImplementosAndSenders

	self 
		askScope;
		calculateImplementorsAndSenders;
		startWizard ! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:21:12'!
askScope

	| scopeMenu |
	
	scopeMenu := PopUpMenu labelArray: self scopeOptionLabels.
	scopeChoice := scopeMenu startUpWithCaption: 'Select Refactoring Scope'.
	scopeChoice = 0 ifTrue: [ self endRequest ].
	! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/13/2018 17:41:06'!
calculateImplementorsAndSenders
		
	implementors := IdentitySet new.
	senders := IdentitySet new.
	
	scopeChoice = 1 ifTrue: [ ^self implementorsAndSendersForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndSendersForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndSendersInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndSendersInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndSendersInSystem ].
	
	self error: 'Unknown scope option' 
		
		! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:00:51'!
implementorsAndSendersForClass

	^self refactoringClass addImplementorsOf: oldSelector to: implementors andSendersTo: senders forClassAndMetaOf: implementingClass! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:00:56'!
implementorsAndSendersForHierarchy 
		
	^self refactoringClass addImplementorsOf: oldSelector to: implementors andSendersTo: senders inHierarchyOf: implementingClass! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:01:01'!
implementorsAndSendersInCategory 

	^self refactoringClass 
		addImplementorsOf: oldSelector 
		to: implementors 
		andSendersTo: senders 
		inCategory: implementingClass category 
		organizedBy: SystemOrganization! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:01:05'!
implementorsAndSendersInCategoryAndHierarchy 

	^self refactoringClass 
		addImplementorsOf: oldSelector 
		to: implementors 
		andSendersTo: senders 
		inCategoriesAndHierarchyOf: implementingClass 
		organizedBy: SystemOrganization ! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:01:15'!
implementorsAndSendersInSystem
		
	^self refactoringClass addImplementorsOf: oldSelector to: implementors andSendersTo: senders inSystem: Smalltalk ! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/13/2018 17:41:33'!
scopeOptionLabels
	
	^{'In Class'. 'In Hierarchy'. 'In Category'. 'In Hierarchy and its Categories'. 'In System'}.! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/7/2019 11:17:34'!
startWizard
		
	ChangeSelectorImplementorsStepWindow openFrom: self! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 8/18/2018 17:02:26'!
implementors
	
	^implementors ! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 8/18/2018 17:02:29'!
implementors: implementorsCollection

	implementors := implementorsCollection ! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 8/18/2018 17:02:33'!
oldSelector
	
	^oldSelector ! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 8/18/2018 17:02:37'!
senders
	
	^senders ! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 8/18/2018 17:02:41'!
senders: sendersCollection
 
	senders := sendersCollection ! !

!ChangeSelectorApplier methodsFor: 'accessing' stamp: 'HAW 1/7/2019 11:16:36'!
wizardStepWindow: aWizarStepWindow 
	
	wizardStepWindow := aWizarStepWindow ! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 1/7/2019 11:08:33'!
closeBrowser
	
	wizardStepWindow delete.
	! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 10/10/2018 19:50:39'!
doNotShowChanges

	shouldShowChanges := false! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 1/7/2019 14:09:10'!
informChangesToBrowser

	"If the selected message is not the same as the oldSelector, that is the selector being renamed, 
	then it implies that we are renaming a selector sent in the source code of the selected message then
	I don't have to change the selected message in the browser - Hernan"
	browser selectedMessageName = oldSelector ifTrue: [ 
		browser setSelector: refactoring newSelector ]! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/18/2018 17:02:52'!
messageSetWindowClass
		
	^MessageSetWindow 
	! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 10/10/2018 19:52:08'!
showChanges
		
	self showChangesInMessageSetWindow! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/18/2018 17:28:30'!
showChangesInMessageSetWindow

	self messageSetWindowClass openMessageList: changes asSortedCollection label: 'Changed methods' ! !

!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 1/7/2019 13:51:04'!
createAndApplyRefactoring

	self 
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		informChangesToBrowser.
			
	shouldShowChanges ifTrue: [ self showChanges ]
	
			! !

!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 1/3/2019 08:46:41'!
createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor 
		
	implementors := Array with: anImplementor.
	senders := #().
	shouldShowChanges := false.
	
	self createAndApplyRefactoring ! !

!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 17:03:58'!
refactoringClass

	self subclassResponsibility ! !

!ChangeSelectorApplier methodsFor: 'initialization' stamp: 'HAW 1/7/2019 13:47:34'!
initializeOn: aBrowser for: aSelector in: aClass
	
	oldSelector := aSelector.
	implementingClass := aClass.
	browser := aBrowser.
	shouldShowChanges := true.! !

!ChangeSelectorApplier methodsFor: 'evaluation' stamp: 'HAW 1/7/2019 14:59:52'!
ifHasNoSendersAndOneImplementor: trueBlock ifNot: falseBlock

	| allImplementors |
	
	allImplementors := Smalltalk allImplementorsOf: oldSelector.
	
	"I could try to see if there is one sender and that that sender is in the same method beeing renamed. That could
	mean that it is a recursive call but I should also see if the receiver is self to be sure because if it is other 'type' of 
	object the rename could not be safe. To complex for a small posibility - Hernan"
	(allImplementors size = 1 and: [ (Smalltalk allCallsOn: oldSelector) isEmpty ]) 
		ifTrue: [ trueBlock value: allImplementors anyOne compiledMethod ]
		ifFalse: falseBlock! !

!ChangeSelectorApplier methodsFor: 'evaluation' stamp: 'HAW 1/3/2019 08:41:27'!
value

	requestExitBlock := [ ^self ].
		
	self requestRefactoringParametersHandlingRefactoringExceptions.
	
	self 
		ifHasNoSendersAndOneImplementor: [ :anImplementor | self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor ]
		ifNot: [ self askForImplementosAndSenders ]! !

!ChangeSelectorApplier methodsFor: 'evaluation' stamp: 'HAW 1/3/2019 08:46:11'!
wizardEnded

	requestExitBlock := [ ^self ].
	
	self 
		closeBrowser;
		createAndApplyRefactoring.! !


!ChangeSelectorApplier class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 17:17:52'!
assertCanApplyRefactoringFor: aSelector in: aClass

	self subclassResponsibility ! !

!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 1/7/2019 13:47:54'!
on: aBrowser for: aSelector in: aClass 
	
	self assertCanApplyRefactoringFor: aSelector in: aClass.
	
	^self new initializeOn: aBrowser for: aSelector in: aClass
	! !


!AddParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 16:46:49'!
createRefactoring
	
	oldSelector isUnary ifTrue: [ ^self createRefactoringForUnarySelector].
	oldSelector isKeyword ifTrue: [ ^self createRefactoringForKeywordSelector ].
	
	self error: 'oldSelector should be unary or keyword!!'! !

!AddParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/4/2018 20:24:53'!
createRefactoringForKeywordSelector
	
	^self refactoringClass 
		named: newParameter
		at: parameterIndex 
		initializedWith: newParameterValue 
		using: newKeyword 
		toKeywordSelector: oldSelector 
		implementors: implementors 
		senders: senders ! !

!AddParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 16:56:55'!
createRefactoringForUnarySelector
	
	^self refactoringClass 
		named: newParameter
		initializedWith: newParameterValue 
		toUnarySelector: oldSelector 
		implementors: implementors 
		senders: senders ! !

!AddParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 16:56:55'!
refactoringClass

	^AddParameter! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/4/2018 20:33:41'!
askInsertionIndex

	| methodNode originalMethod parameterNames |
	
	originalMethod := implementingClass compiledMethodAt: oldSelector.
	methodNode := originalMethod methodNode.
	parameterNames := methodNode argumentNames.
	parameterNames add: 'Add as last parameter'.

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Add Before?'.
	parameterIndex = 0 ifTrue: [self endRequest ].
	
	! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/4/2018 20:33:37'!
askInsertionIndexIfNecessary

	oldSelector isKeyword ifTrue: [ self askInsertionIndex ].
	! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:06:28'!
askNewKeyword

 	| enteredString |
	
	enteredString := self request: 'Enter keyword for new parameter'.
	newKeyword := enteredString withBlanksTrimmed asSymbol.
	self refactoringClass assertIsValidKeywordForNewParameter: newKeyword! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 17:06:28'!
askNewKeywordIfNecessary
		
	oldSelector isKeyword ifTrue: [self askNewKeyword]! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 16:56:55'!
askNewParameter
		
	| enteredString |
	
	enteredString := self request: 'Enter new parameter name'.
	newParameter := enteredString withBlanksTrimmed.
	self refactoringClass assertIsValidParameterName: newParameter
! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/25/2018 11:28:58'!
askNewParameterValue
		
	| enteredString |
	
	enteredString := self request: 'Enter parameter value for senders'.
	newParameterValue := enteredString withBlanksTrimmed.
	self refactoringClass assertNewParameterValueIsNotEmpty: newParameterValue.
	self refactoringClass assertNewParameterValueIsValid: newParameterValue.
! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/3/2019 08:01:40'!
requestRefactoringParameters

	self
		askNewParameter;
		askNewParameterValue;
		askInsertionIndexIfNecessary;
		askNewKeywordIfNecessary
		! !


!AddParameterApplier class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 17:18:06'!
assertCanApplyRefactoringFor: aSelector in: aClass

	AddParameter assertCanAddParameterTo: aSelector.
	

	! !


!RemoveParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/4/2018 15:15:48'!
createRefactoring
	
	^self refactoringClass named: parameterToRemove from: originalMethod implementors: implementors senders: senders ! !

!RemoveParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/4/2018 15:15:00'!
refactoringClass

	^RemoveParameter! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/4/2018 15:30:50'!
askParameterToRemove
		
	| methodNode parameterNames |
	
	originalMethod := implementingClass compiledMethodAt: oldSelector.
	methodNode := originalMethod methodNode.
	parameterNames := methodNode argumentNames.
	
	parameterNames size = 1 
		ifTrue: [ parameterToRemove := parameterNames first ]
		ifFalse: [ parameterToRemove := self selectParameterToRemoveForm: parameterNames ].
	
	! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/3/2019 08:01:51'!
requestRefactoringParameters

	self askParameterToRemove
		
		! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/4/2018 15:36:30'!
selectParameterToRemoveForm: parameterNames
	
	| parameterIndex |
	
	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Select parameter to remove'.
	parameterIndex = 0 ifTrue: [self endRequest ].
	
	^parameterNames at: parameterIndex.! !


!RemoveParameterApplier class methodsFor: 'pre-conditions' stamp: 'HAW 9/4/2018 15:17:09'!
assertCanApplyRefactoringFor: aSelector in: aClass

	RemoveParameter assertCanRemoveParameterFrom: aSelector.
	

	! !


!RenameSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 17:02:06'!
askNewSelector

	| enteredString |
	
	enteredString := self request: 'Enter new selector:' initialAnswer: oldSelector.
	newSelector := enteredString withBlanksTrimmed asSymbol.
		
! !

!RenameSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/18/2018 16:56:24'!
assertCanRenameSelector
		
	self refactoringClass assertIsValidToRenameFrom: oldSelector to: newSelector.
	! !

!RenameSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/3/2019 08:02:00'!
requestRefactoringParameters

	self
		askNewSelector;
		assertCanRenameSelector
		! !

!RenameSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 16:56:24'!
createRefactoring
	
	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	! !

!RenameSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 17:04:02'!
refactoringClass

	^RenameSelector ! !


!RenameSelectorApplier class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 17:24:18'!
assertCanApplyRefactoringFor: aSelector in: aClass 
	
	! !


!InsertSuperclassApplier methodsFor: 'initialization' stamp: 'FJG 8/5/2018 22:31:03'!
initializeOn: aBrowser for: aClass
	browser _ aBrowser.
	classToRefactor _ aClass.! !

!InsertSuperclassApplier methodsFor: 'refactoring - parameters request' stamp: 'FJG 8/5/2018 22:31:31'!
askNewSuperclassName
	newSuperclassName _ self
		request: 'Enter new superclass name:'.
	newSuperclassName _ newSuperclassName withBlanksTrimmed asSymbol.! !

!InsertSuperclassApplier methodsFor: 'refactoring - parameters request' stamp: 'FJG 8/5/2018 22:29:07'!
requestRefactoringParameters

	self askNewSuperclassName! !

!InsertSuperclassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/13/2018 15:55:10'!
createRefactoring

	^InsertSuperclass to: classToRefactor named: newSuperclassName! !

!InsertSuperclassApplier methodsFor: 'refactoring - changes' stamp: 'FJG 8/5/2018 22:33:49'!
showChanges
	
	browser changed: #classList.
! !


!InsertSuperclassApplier class methodsFor: 'instance creation' stamp: 'FJG 8/5/2018 22:27:02'!
on: aBrowser for: aClass

	^self new initializeOn: aBrowser for: aClass! !


!RemoveAllUnreferencedInstanceVariablesApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/2/2018 16:11:13'!
requestRefactoringParameters

	! !

!RemoveAllUnreferencedInstanceVariablesApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/2/2018 16:15:05'!
createRefactoring
		
	^RemoveAllUnreferencedInstanceVariables from: classToRefactor ! !

!RemoveAllUnreferencedInstanceVariablesApplier methodsFor: 'refactoring - changes' stamp: 'HAW 1/7/2019 14:42:50'!
informChangesToBrowser
		
	browser acceptedContentsChanged! !

!RemoveAllUnreferencedInstanceVariablesApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/2/2018 16:20:16'!
showChanges

	| removedInstanceVariablesMessage |
	
	self informChangesToBrowser.
	removedInstanceVariablesMessage := changes isEmpty 
		ifTrue: [ 'No instance variable was removed' ]
		ifFalse: [ changes size = 1
			ifTrue: [ changes first, ' was removed' ]
			ifFalse: [ changes asCommaStringAnd, ' were removed' ]].
			
	self inform: removedInstanceVariablesMessage! !

!RemoveAllUnreferencedInstanceVariablesApplier methodsFor: 'initialization' stamp: 'HAW 1/7/2019 14:42:41'!
initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor ! !


!RemoveAllUnreferencedInstanceVariablesApplier class methodsFor: 'instance creation' stamp: 'HAW 1/7/2019 14:42:34'!
on: aBrowser for: aClassToRefactor
	
	^self new initializeOn: aBrowser for: aClassToRefactor 
! !


!RemoveInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/30/2017 18:22:28'!
chooseInstanceVariable

	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :aVariableToRemove | ^variableToRemove := aVariableToRemove ].
	self endRequest 

	! !

!RemoveInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/17/2017 19:51:21'!
requestRefactoringParameters

	self chooseInstanceVariable.

	! !

!RemoveInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/30/2017 18:22:41'!
selectVariableLabel
	
	^'Select instance variable to remove'! !

!RemoveInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/17/2017 19:50:03'!
createRefactoring
		
	^RemoveInstanceVariable named: variableToRemove from: classToRefactor ! !

!RemoveInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 1/7/2019 14:43:43'!
informChangesToBrowser
		
	browser acceptedContentsChanged! !

!RemoveInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/17/2017 19:50:31'!
showChanges

	self informChangesToBrowser! !

!RemoveInstanceVariableApplier methodsFor: 'initialization' stamp: 'HAW 1/7/2019 14:43:37'!
initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor ! !


!RemoveInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 1/7/2019 14:43:28'!
on: aBrowser for: aClassToRefactor
	
	^self new initializeOn: aBrowser for: aClassToRefactor 
! !


!RenameClassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 16:02:13'!
askNewClassName

	newClassName := self request: 'Enter new name:' initialAnswer: classToRename name.
	newClassName := newClassName withBlanksTrimmed asSymbol.
	! !

!RenameClassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 19:05:08'!
requestRefactoringParameters

	self askNewClassName! !

!RenameClassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/5/2017 18:07:28'!
createRefactoring

	^RenameClass from: classToRename to: newClassName in: Smalltalk undeclared: Undeclared.
	
! !

!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2017 16:05:00'!
informChangesToBrowser
	
	browser changed: #classList.
	browser selectClass: classToRename.
! !

!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2017 18:08:13'!
openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references' autoSelect: newClassName ]
! !

!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2017 18:07:55'!
showChanges

	self 
		informChangesToBrowser;
		openChangedMethods
! !

!RenameClassApplier methodsFor: 'initialization' stamp: 'HAW 6/5/2017 16:55:57'!
initializeOn: aBrowser for: aClass

	browser := aBrowser.
	classToRename := aClass.
	! !


!RenameClassApplier class methodsFor: 'instance creation' stamp: 'HAW 6/5/2017 12:27:45'!
on: aBrowser for: aClass

	^self new initializeOn: aBrowser for: aClass! !


!RenameInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 16:49:50'!
askNewVariableName
		
	newInstanceVariable := self request: 'Enter new name:' initialAnswer: oldInstanceVariable. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed ! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 11/28/2018 19:37:54'!
chooseInstanceVariable
	
	oldInstanceVariable ifNotNil: [ ^self ].
		
	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anOldInstanceVariable | ^oldInstanceVariable := anOldInstanceVariable ].
	self endRequest ! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2017 19:05:16'!
requestRefactoringParameters

	self 
		chooseInstanceVariable;
		askNewVariableName! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/30/2017 17:27:22'!
selectVariableLabel
	
	^'Select instance variable to rename'! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/5/2017 18:09:13'!
createRefactoring
		
	^RenameInstanceVariable from: oldInstanceVariable to: newInstanceVariable in: classToRefactor.
	! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 11/28/2018 19:38:47'!
informChangesToBrowser
		
	browser acceptedContentsChanged! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/30/2017 17:30:22'!
openChangedMethods 

	changes ifNotEmpty: [ self openChangedMethodsWhenChangesNotEmpty ]! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/30/2017 17:30:13'!
openChangedMethodsWhenChangesNotEmpty 

	MessageSetWindow openMessageList: changes label: 'Renamed references' autoSelect: newInstanceVariable ! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2017 18:09:30'!
showChanges

	self 
		informChangesToBrowser;
		openChangedMethods ! !

!RenameInstanceVariableApplier methodsFor: 'initialization' stamp: 'HAW 11/28/2018 19:40:02'!
initializeOn: aBrowserWindow for: anOldInstanceVariable at: aClassToRefactor 
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	oldInstanceVariable := anOldInstanceVariable ! !


!RenameInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 11/29/2018 20:19:06'!
on: aBrowser at: aClassToRefactor
	
	^self new initializeOn: aBrowser for: nil at: aClassToRefactor 
! !

!RenameInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 11/29/2018 20:18:59'!
on: aBrowser for: anInstanceVariableName at:  aClassToRefactor 
	
	^self new initializeOn: aBrowser for: anInstanceVariableName at: aClassToRefactor ! !


!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/25/2017 21:37:58'!
askNewVariableName
		
	newVariable := (self request: 'Enter new name:' initialAnswer: oldVariable) withBlanksTrimmed ! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 11/28/2018 19:42:45'!
chooseTemporaryVariable
	
	| variables |
	
	oldVariable ifNotNil: [ ^self ].
	
	variables := methodNode tempNames.
	variables isEmpty 
		ifTrue: [ self noTemporaryToRename ]
		ifFalse: [ self chooseTemporaryVariableFrom: variables ]
	
	! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 10/4/2017 17:15:17'!
chooseTemporaryVariableFrom: variables
	
	 | selection |
			
	selection :=  smalltalkEditor selection string withBlanksTrimmed.
	oldVariable := (self is: selection temporaryVariableFrom: variables) 
		ifTrue: [ selection ]
		ifFalse: [ self selectTemporaryVariableFrom: variables]! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 10/4/2017 17:13:25'!
is: selection temporaryVariableFrom: variables
		
	^smalltalkEditor hasSelection and: [variables includes: selection]! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 10/4/2017 17:09:43'!
noTemporaryToRename
	
	self inform: 'There are no temporary to rename'.
	self endRequest ! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/25/2017 21:29:58'!
requestRefactoringParameters

	self 
		chooseTemporaryVariable;
		askNewVariableName! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 10/4/2017 17:16:25'!
selectTemporaryVariableFrom: variables

	| selectionIndex |
	
	selectionIndex := (PopUpMenu labelArray: variables lines: #()) startUpWithCaption: 'Select temporary to rename'.
	
	^selectionIndex = 0 
		ifTrue: [ self endRequest ]
		ifFalse: [ variables at: selectionIndex ]! !

!RenameTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'HAW 6/25/2017 21:54:40'!
createRefactoring
		
	^RenameTemporary from: oldVariable to: newVariable in: methodNode 
	! !

!RenameTemporaryApplier methodsFor: 'initialization' stamp: 'HAW 11/28/2018 19:43:15'!
initializeOn: aSmalltalkEditor for: aTemporary

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := classToRefactor methodNodeFor: smalltalkEditor actualContents string.
	oldVariable := aTemporary 
	! !

!RenameTemporaryApplier methodsFor: 'refactoring - changes' stamp: 'HAW 8/9/2018 19:43:41'!
showChanges

	smalltalkEditor actualContents: changes.
	smalltalkEditor hasUnacceptedEdits ifFalse: [
		smalltalkEditor 
			hasUnacceptedEdits: true;
			acceptContents ]
	! !


!RenameTemporaryApplier class methodsFor: 'exceptions' stamp: 'HAW 10/4/2017 17:05:53'!
errorMessageForCanNotParseMethod: anError

	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'Method can not be parsed due to:';
			newLine;
			nextPutAll: anError messageText ]! !

!RenameTemporaryApplier class methodsFor: 'instance creation' stamp: 'HAW 11/28/2018 19:44:13'!
on: aSmalltalkEditor for: aTemporary
	
	^self new initializeOn: aSmalltalkEditor for: aTemporary! !


!SafelyRemoveClassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/1/2018 16:54:40'!
confirmationMessageText
	
	^'Are you sure you want to remove ', classToRemove name asString, '?'! !

!SafelyRemoveClassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/1/2018 16:52:57'!
requestRefactoringParameters

	(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ].

	! !

!SafelyRemoveClassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/1/2018 16:54:33'!
createRefactoring
		
	^SafelyRemoveClass of: classToRemove ! !

!SafelyRemoveClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 9/14/2018 13:48:37'!
informChangesToBrowser

	browser classListIndex: 0! !

!SafelyRemoveClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 9/14/2018 13:47:58'!
showChanges

	self informChangesToBrowser! !

!SafelyRemoveClassApplier methodsFor: 'initialization' stamp: 'HAW 9/14/2018 13:47:41'!
initializeOn: aBrowser of: aClassToRemove 
	
	browser := aBrowser.
	classToRemove := aClassToRemove ! !


!SafelyRemoveClassApplier class methodsFor: 'instance creation' stamp: 'HAW 9/14/2018 13:47:16'!
on: aBrowser of: aClassToRemove
	
	^self new initializeOn: aBrowser of: aClassToRemove ! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 11/28/2018 20:23:45'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 	-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary 
	}`! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 19:25:26'!
classListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		5.
				#itemOrder 		-> 		10.
				#label 			-> 		'refactorings...'.
				#selector 		-> 		#openClassRefactoringMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 8/17/2018 17:50:40'!
classRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class ...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'insert superclass ...'.
				#object 			-> 		#model.
				#selector 		-> 		#insertSuperclass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'safely remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#safelyRemoveClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'add inst var...'.
				#selector 		-> 		#addInstVar.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'rename inst var...'.
				#selector 		-> 		#renameInstVar.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove inst var...'.
				#selector 		-> 		#removeInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove all unreferenced inst var...'.
				#selector 		-> 		#removeAllUnreferencedInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
	}`.
	! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 19:38:01'!
messageListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		5.
				#itemOrder 		-> 		10.
				#label 			-> 		'refactorings...'.
				#selector 		-> 		#openMessageRefactoringMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 3/6/2019 15:05:24'!
messsageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename...'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
	}`.
	
	! !

!RefactoringMenues class methodsFor: 'initialization' stamp: 'HAW 12/28/2018 12:51:56'!
initialize 

	Editor initialize! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 2/4/2019 16:43:23'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor'))
! !


!RefactoringPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:48:22'!
refactoringError: aMessage

	Refactoring refactoringError: aMessage.! !

!RefactoringPrecondition methodsFor: 'warnings' stamp: 'HAW 3/4/2019 13:30:01'!
refactoringWarning: aMessageText

	^ Refactoring refactoringWarning: aMessageText ! !


!NewClassPrecondition methodsFor: 'evaluating' stamp: 'HAW 3/3/2019 09:18:10'!
value

	self assertNewClassNameIsNotEmpty.
	self assertNewClassNameSymbol.
	self assertNewClassNameStartsWithUppercaseLetter.
	self assertNewClassNameHasNoSeparators.
	self assertNewClassNameDoesNotExistInSystem.
	self assertNewClassNameIsNotUndeclaredInUndeclared.

! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:55:05'!
assertNewClassNameDoesNotExistInSystem

	system at: newClassName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists]
			ifFalse: [  self signalGlobalAlreadyExists]].! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:50:39'!
assertNewClassNameHasNoSeparators

	(newClassName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewClassNameCanNotHaveSeparators]! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:50:43'!
assertNewClassNameIsNotEmpty

	newClassName withBlanksTrimmed isEmpty ifTrue: [ self signalNewClassNameCanNotBeEmpty]! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:55:23'!
assertNewClassNameIsNotUndeclaredInUndeclared

	(undeclared includesKey: newClassName) ifTrue: [ self signalNewClassIsUndeclared]! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:50:51'!
assertNewClassNameStartsWithUppercaseLetter

	newClassName first isUppercase ifFalse: [ self signalNewNameMustStartWithUppercaseLetter]! !

!NewClassPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/4/2019 11:50:55'!
assertNewClassNameSymbol

	newClassName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]! !

!NewClassPrecondition methodsFor: 'initialization' stamp: 'HAW 8/13/2018 16:00:19'!
initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	newClassName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary ! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:54:34'!
signalClassAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newClassName).! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:55:05'!
signalGlobalAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newClassName)! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:55:23'!
signalNewClassIsUndeclared

	self refactoringError: (self class errorMessageForNewClassIsUndeclared: newClassName).! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:52:08'!
signalNewClassNameCanNotBeEmpty
	
	self refactoringError: self class newClassNameCanNotBeEmptyErrorMessage! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:52:13'!
signalNewClassNameCanNotHaveSeparators
	
	self refactoringError: self class newClassNameCanNotHaveSeparatorsErrorMessage ! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:52:17'!
signalNewNameMustBeSymbol

	self refactoringError: self class newNameMustBeSymbolErrorMessage.! !

!NewClassPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/4/2019 11:52:22'!
signalNewNameMustStartWithUppercaseLetter

	self refactoringError: self class newNameMustStartWithUppercaseLetterErrorMessage.! !


!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 12/13/2018 17:56:51'!
errorMessageForAlreadyExistClassNamed: aNewClassName

	^'Class named ', aNewClassName, ' already exist'! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 12/13/2018 17:59:02'!
errorMessageForAlreadyExistGlobalNamed: aNewClassName

	^'There is already a global variable named ', aNewClassName ! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 8/13/2018 15:58:18'!
errorMessageForNewClassIsUndeclared: aNewClassName

	^aNewClassName, ' is undeclared'! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 8/13/2018 15:58:18'!
newClassNameCanNotBeEmptyErrorMessage
	
	^'New class name can not be empty'! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/3/2019 09:20:59'!
newClassNameCanNotHaveSeparatorsErrorMessage
	
	^'New class name can not have separators'! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 8/13/2018 15:58:18'!
newNameMustBeSymbolErrorMessage
	
	^'New class name must be a symbol'! !

!NewClassPrecondition class methodsFor: 'error messages' stamp: 'HAW 8/13/2018 15:58:18'!
newNameMustStartWithUppercaseLetterErrorMessage

	^'New class name must start with an uppercase letter'! !

!NewClassPrecondition class methodsFor: 'instance creation' stamp: 'HAW 8/13/2018 17:31:51'!
for: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	^self new initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 
! !

!NewClassPrecondition class methodsFor: 'evaluation' stamp: 'HAW 8/13/2018 17:37:20'!
valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	^(self for: aNewClassName in: aSystem undeclared: anUndeclaredDictionary) value! !


!NewInstanceVariablePrecondition methodsFor: 'initialization' stamp: 'HAW 3/3/2019 08:10:58'!
initializeOf: anInstanceVariableName for: aClass

	instVarName := anInstanceVariableName withBlanksTrimmed.
	classToAddInstVar := aClass.! !

!NewInstanceVariablePrecondition methodsFor: 'evaluating' stamp: 'HAW 3/3/2019 08:30:12'!
value
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefined.

	self warnIfIsDefinedInMethods.! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 3/3/2019 08:36:05'!
assertIsNotAReservedName
	
	(ClassBuilder reservedNames includes: instVarName) ifTrue: [ self signalNewInstanceVariableCanNotBeAReservedName ]! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 3/3/2019 08:35:12'!
assertIsNotAlreadyDefined
	
	^ classToAddInstVar 
		withClassesThatDefineInHierarchyInstanceVariable: instVarName 
		do: [ :definingClasses | self signalAlreadyDefinedInAll: definingClasses ]
		ifNone: [ ].! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'FGJ 12/17/2018 15:29:44'!
assertIsNotEmpty

	instVarName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty]! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 3/3/2019 08:34:17'!
assertIsValidInstanceVariableName

	| scannedNames |

	scannedNames := Scanner new scanFieldNames: instVarName .
	scannedNames size = 1 ifFalse: [ self signalInvalidInstanceVariable ].
	scannedNames first = instVarName ifFalse: [ self signalInvalidInstanceVariable ].! !

!NewInstanceVariablePrecondition methodsFor: 'exceptions' stamp: 'HAW 3/3/2019 08:35:12'!
signalAlreadyDefinedInAll: classes

	^ self refactoringError: (self class errorMessageForNewInstanceVariable: instVarName alreadyDefinedInAll: classes).! !

!NewInstanceVariablePrecondition methodsFor: 'exceptions' stamp: 'HAW 3/3/2019 08:34:09'!
signalInvalidInstanceVariable

	^ self refactoringError: (self class errorMessageForInvalidInstanceVariable: instVarName).! !

!NewInstanceVariablePrecondition methodsFor: 'exceptions' stamp: 'HAW 3/3/2019 08:36:00'!
signalNewInstanceVariableCanNotBeAReservedName

	self refactoringError: (self class errorMessageForNewInstanceVariableCanNotBeAReservedName: instVarName)! !

!NewInstanceVariablePrecondition methodsFor: 'exceptions' stamp: 'FGJ 12/17/2018 16:27:50'!
signalNewVariableCanNotBeEmpty
	
	self refactoringError: self class newVariableCanNotBeEmptyErrorMessage! !

!NewInstanceVariablePrecondition methodsFor: 'warnings' stamp: 'HAW 3/3/2019 08:19:24'!
methodsDefiningNewVariable
	
	| methodsDefiningNewVariableInHierarchy |
	
	methodsDefiningNewVariableInHierarchy := OrderedCollection new.
	
	classToAddInstVar withAllSubclassesDo: [ :class | 
		methodsDefiningNewVariableInHierarchy addAll: (class methodsWithArgumentOrTemporaryNamed: instVarName) ].

	^methodsDefiningNewVariableInHierarchy 
		
	! !

!NewInstanceVariablePrecondition methodsFor: 'warnings' stamp: 'HAW 3/3/2019 08:13:49'!
warnIfIsDefinedInMethods
	
	| methodsDefiningNewVariable |
	
	methodsDefiningNewVariable := self methodsDefiningNewVariable.
	
	methodsDefiningNewVariable notEmpty ifTrue: [ self warnNewVariable: instVarName willBeHiddenAtAll: methodsDefiningNewVariable ].! !

!NewInstanceVariablePrecondition methodsFor: 'warnings' stamp: 'FGJ 12/17/2018 16:29:18'!
warnNewVariable: newVariable willBeHiddenAtAll: methods

	^ self refactoringWarning: (self class warningMessageForNewVariable: newVariable willBeHiddenAtAll: methods).! !


!NewInstanceVariablePrecondition class methodsFor: 'instance creation' stamp: 'HAW 3/3/2019 08:10:58'!
of: anInstanceVariableName for: aClass

	^self new initializeOf: anInstanceVariableName for: aClass ! !

!NewInstanceVariablePrecondition class methodsFor: 'error messages' stamp: 'FGJ 12/17/2018 16:27:09'!
errorMessageForInvalidInstanceVariable: aName

	^ '''' , aName , ''' is not a valid instance variable name'.! !

!NewInstanceVariablePrecondition class methodsFor: 'error messages' stamp: 'FGJ 12/17/2018 16:28:18'!
errorMessageForNewInstanceVariable: aName alreadyDefinedInAll: classes

	^ 'Instance variable ''' , aName , ''' is already defined in ' , classes asCommaStringAnd.! !

!NewInstanceVariablePrecondition class methodsFor: 'error messages' stamp: 'HAW 3/3/2019 08:32:29'!
errorMessageForNewInstanceVariableCanNotBeAReservedName: aName

	^'''', aName, ''' can not be used as instance variable name because it is a reserved name'! !

!NewInstanceVariablePrecondition class methodsFor: 'error messages' stamp: 'FGJ 12/17/2018 16:05:38'!
newVariableCanNotBeEmptyErrorMessage
	
	^'New variable can not be empty'! !

!NewInstanceVariablePrecondition class methodsFor: 'warning messages' stamp: 'HAW 3/3/2019 08:27:30'!
warningMessageForNewVariable: newVariable willBeHiddenAtAll: methods

	^String streamContents: [ :stream |
		stream 
			nextPutAll: newVariable;
			nextPutAll: ' will be hidden at '.
		methods asCommaSeparated: [:aMethod | aMethod printClassAndSelectorOn: stream ] on: stream ]! !

!NewInstanceVariablePrecondition class methodsFor: 'evaluating' stamp: 'HAW 3/3/2019 10:06:23'!
valueOf: anInstanceVariableName for: aClass

	^(self of: anInstanceVariableName for: aClass) value! !

RefactoringMenues initialize!
