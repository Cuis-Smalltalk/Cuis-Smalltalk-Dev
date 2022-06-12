'From Cuis 6.0 [latest update: #5234] on 11 June 2022 at 8:28:35 pm'!
!classDefinition: #InlineMethodMessageSendsSet category: #'Tools-Refactoring'!
MessageSet subclass: #InlineMethodMessageSendsSet
	instanceVariableNames: 'applier selectedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodWizardStepWindow category: #'Tools-Refactoring'!
MessageSetWindow subclass: #InlineMethodWizardStepWindow
	instanceVariableNames: 'applier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodImplementorsStepWindow category: #'Tools-Refactoring'!
InlineMethodWizardStepWindow subclass: #InlineMethodImplementorsStepWindow
	instanceVariableNames: 'selectedImplementor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodImplementorsWithShowUsagesStepWindow category: #'Tools-Refactoring'!
InlineMethodImplementorsStepWindow subclass: #InlineMethodImplementorsWithShowUsagesStepWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodUsagesStepWindow category: #'Tools-Refactoring'!
InlineMethodWizardStepWindow subclass: #InlineMethodUsagesStepWindow
	instanceVariableNames: 'changedMethods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodUsagesWithShowImplementorsStepWindow category: #'Tools-Refactoring'!
InlineMethodUsagesStepWindow subclass: #InlineMethodUsagesWithShowImplementorsStepWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MessageNodeReference category: #'Tools-Refactoring'!
Object subclass: #MessageNodeReference
	instanceVariableNames: 'messageNode selector classReference completeSourceRange methodNode stringVersion'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PossibleMessageNodeReference category: #'Tools-Refactoring'!
MessageNodeReference subclass: #PossibleMessageNodeReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethod category: #'Tools-Refactoring'!
Refactoring subclass: #InlineMethod
	instanceVariableNames: 'methodToInline messageSendsToInline updatedSendersCode methodNodeToInline replacementsByMessageSend temporariesDeclarationsByNode temporariesToDeclareByInsertionPoint implementorCompleteSourceRanges removeMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineTemporaryVariable category: #'Tools-Refactoring'!
Refactoring subclass: #InlineTemporaryVariable
	instanceVariableNames: 'variableToInline methodToRefactor updatedSourceCode methodNode oldVariableNode usageToInline sourceCodeChanges methodOrBlockNodeDeclaringTemporary rangeOfNodeDeclaringTemporary assignmentToInlineRange endOfNodeEnclosingAssignment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #InlineMethodApplier
	instanceVariableNames: 'selectorToInline scopeChoice implementors messageSends selectedClass wizardStepWindow shouldShowChanges browser shouldRemoveImplementor triggeringMessageSend shouldInlineTriggeringMessageSendOnly'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineTemporaryVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #InlineTemporaryVariableApplier
	instanceVariableNames: 'codeProvider temporaryToInline usageInterval methodToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:28:05'!
referencesSelf

	^self methodNode referencesSelf! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:28:09'!
referencesSuper

	^self methodNode referencesSuper! !


!Interval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:29:34'!
includesAllOf: aCollection

	^aCollection isInterval
		ifTrue: [ (self includes: aCollection first)
			and: [ (self includes: aCollection last)
			and: [ self increment = aCollection increment or: [ super includesAllOf:aCollection ]]]]
		ifFalse: [ super includesAllOf: aCollection ]! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:30:38'!
codeAfterIntervalOn: aSourceCode

	^ aSourceCode copyFrom: (self last + 1) to: aSourceCode size! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:36:07'!
expandUntilStartOfNextStatementOn: aSourceCode

	"Expands a source code interval containing a valid expression until the next statement. If there isn't one, the interval is not modified.
	Examples:
	- The interval *a := 1 + 2.*   b = 3 + 4.	 is expanded to *a := 1 + 2.   *b = 3 + 4.
	- The interval *a := 1 + 2.*  '
		 b = 3 + 4.	 is expanded to
		*a := 1 + 2.
		   *b = 3 + 4.
	- The interval *| a b |*
		a : =1. is expanded to
		*| a b |
		*a := 1.

	TODO: improve to take into account that there could be a comment in the middle of the statements or
	 after the last statement - Fernando"

	((self last >= aSourceCode size)
		or:[ (self isEndOfLastStatementOn: aSourceCode)
		or:[self isLastStatementOfBlockOn: aSourceCode]]) ifTrue: [^self class from: self first to: self last].

	((self isEndOfTemporariesDeclarationOn: aSourceCode) or: [self endsOnDotOn: aSourceCode ])
		ifTrue: [^self expandUntilNextNonBlankCharacterOn: aSourceCode ].

	(self nextNonBlankCharacterIsDotOn: aSourceCode)
		ifTrue: [^(self expandUntilStatementEndOn: aSourceCode) expandUntilStartOfNextStatementOn: aSourceCode ].

	self error: self class canNotExpandIncompleteStatementError.
	! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:34:10'!
expandUntilStatementEndOn: aSourceCode

	^SourceCodeInterval
		from: self first
		to: (aSourceCode findDelimiters: '.' startingAt: self last)
	! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:34:23'!
lastCharacterOfRangeOn: aSourceCode

	^aSourceCode at: self last! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:34:48'!
nextNonBlankCharacterIs: aCharacter on: aSourceCode

	| codeAfterInterval firstNonBlankIndex |

	codeAfterInterval := self codeAfterIntervalOn: aSourceCode.

	firstNonBlankIndex := codeAfterInterval firstNonSeparator.

	^firstNonBlankIndex ~= 0 and: [(codeAfterInterval at: firstNonBlankIndex) = aCharacter]
! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 19:35:00'!
nextNonBlankCharacterIsDotOn: aSourceCode

	^self nextNonBlankCharacterIs: $. on: aSourceCode.
	! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:22'!
endsOnDotOn: aSourceCode

	^(self lastCharacterOfRangeOn: aSourceCode) = $.! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:32:09'!
intervalEndsOnEndOfStatementIn: aSourceCode

	^(self endsOnDotOn: aSourceCode)
		or: [ (self nextNonBlankCharacterIsDotOn: aSourceCode)
	 	or: [self isEndOfTemporariesDeclarationOn: aSourceCode ]].! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:03'!
isEndOfLastStatementOn: aSourceCode

	^((self lastCharacterOfRangeOn: aSourceCode) isSeparator not)
		and: [(self codeAfterIntervalOn: aSourceCode) firstNonSeparator = 0].
	! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:07'!
isEndOfTemporariesDeclarationOn: aSourceCode

	^((self lastCharacterOfRangeOn: aSourceCode) = $|) and: [^(aSourceCode indexOf: $|) < self last]! !

!SourceCodeInterval methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:31:11'!
isLastStatementOfBlockOn: aSourceCode

	^self nextNonBlankCharacterIs: $] on: aSourceCode.! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 6/11/2022 18:57:06'!
expandUntilNextNonBlankCharacterOn: aSourceCode

	^SourceCodeInterval from: self first to: (self last +
		(aSourceCode copyFrom: (self last + 1) to: (aSourceCode size)) firstNonSeparator - 1)
	! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:43:52'!
isMethodNode

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:44:17'!
referencesSelf

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:44:05'!
referencesSuper

	^false! !


!AssignmentNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:44:54'!
referencesSelf

	^variable referencesSelf or: [value referencesSelf]! !

!AssignmentNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:44:57'!
referencesSuper

	^variable referencesSuper or: [value referencesSuper]! !


!BlockNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:38:06'!
referencesSelf

	^self statements anySatisfy: [:aParseNode | aParseNode referencesSelf]! !

!BlockNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:38:09'!
referencesSuper

	^self statements anySatisfy: [:aParseNode | aParseNode referencesSuper]! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 6/11/2022 19:38:58'!
completeSourceRangesOf: requestedParseNode ifAbsent: emptySourceRangesBlock
	"Returns the source ranges associated with the requested parse node."

	| completeSourceRanges |

	completeSourceRanges := Set new.

	"There may be more than one equivalent key in the complete source ranges map
	if more than one block declare a temporary variable with the same name - Fernando"
	self completeSourceRangesDo: [:parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [completeSourceRanges addAll: sourceRanges]].

	completeSourceRanges isEmpty ifFalse: [^completeSourceRanges ] ifTrue: [^emptySourceRangesBlock value].
	! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 6/11/2022 19:42:44'!
singleCompleteSourceRangeOf: requestedParseNode
	"Returns the source range associated with the requested parse node.
	Fails if there is no source range, or if there are multiple source ranges."

	^self
		singleCompleteSourceRangeOf: requestedParseNode
		ifPresent: [ :sourceRange | sourceRange ]
		ifAbsent: [ self error: 'could not find source range for node: ' , requestedParseNode printString ]! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 6/11/2022 19:42:35'!
singleCompleteSourceRangeOf: requestedParseNode ifPresent: sourceRangePresentBlock ifAbsent: sourceRangeAbsentBlock
	"Finds the source range associated with the requested parse node.
	If it is present, evaluates sourceRangePresentBlock with the result.
	Otherwise, it evaluates sourceRangeAbsentBlock.
	Raises an error if the requested parse node has multiple source ranges"
	| sourceRanges |

	sourceRanges := self
		completeSourceRangesOf: requestedParseNode
		ifAbsent: [^sourceRangeAbsentBlock value].

	^sourceRanges size > 1
		ifTrue: [self error: 'there are multiple source ranges for the parse node: ' , requestedParseNode printString ]
		ifFalse: [sourceRangePresentBlock value: sourceRanges anyOne].! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:39:19'!
equivalentTo: aParseNode

	^ aParseNode isMethodNode
		and: [ block equivalentTo: aParseNode block]! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:41:48'!
isMethodNode

	^true.! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:38:25'!
referencesSelf

	^block referencesSelf! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:38:28'!
referencesSuper

	^block referencesSuper! !


!VariableNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:52:27'!
referencesSelf

	^ self isSelfPseudoVariable ! !

!VariableNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:49:49'!
referencesSuper

	^self isSuperPseudoVariable ! !

!SelectorNode methodsFor: 'testing' stamp: 'HAW 5/3/2020 22:39:23'!
isInfix

	^key isInfix ! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 5/3/2020 22:39:07'!
isInfix

	^selector isInfix! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:45:21'!
isKeywordMessageSend

	^self selectorSymbol isKeyword! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:46:12'!
referencesSelf

	^self receiver referencesSelf
		or: [self arguments anySatisfy: [:argument | argument referencesSelf]]! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:46:16'!
referencesSuper

	^self receiver referencesSuper
		or: [self arguments anySatisfy: [:argument | argument referencesSuper]]! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 8/3/2019 10:31:57'!
isUnaryMessageSend

	^self selectorSymbol isUnary ! !

!ReturnNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:46:37'!
referencesSelf

	^expr referencesSelf! !

!ReturnNode methodsFor: 'testing' stamp: 'HAW 6/11/2022 19:46:42'!
referencesSuper

	^expr referencesSuper! !


!SmalltalkEditor methodsFor: 'inline method' stamp: 'HAW 6/11/2022 19:58:26'!
contextualInlineMethod

	self isEditingClassDefinition ifTrue: [ ^morph flash ].
	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualInlineMethodOf: methodNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]	]	! !

!SmalltalkEditor methodsFor: 'inline method' stamp: 'HAW 6/11/2022 19:58:29'!
contextualInlineMethod: aKeyboardEvent

	self contextualInlineMethod.
	^true.

	! !

!SmalltalkEditor methodsFor: 'inline method' stamp: 'HAW 6/11/2022 19:58:33'!
contextualInlineMethodOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor |
			nodeUnderCursor isMessageNode ifFalse: [^morph flash].

			((aMethodNode completeSourceRangesOf: nodeUnderCursor ifAbsent: [self shouldNotHappen])
				detect: [ :aSourceRange | aSourceRange includes: self startIndex]
					ifFound: [ :aSourceRange | | messageNodeReference |
						messageNodeReference := MessageNodeReference
							messageNode: nodeUnderCursor
							selector: aMethodNode selector
							class: aSelectedClass
							completeSourceRange: aSourceRange.
						self inlineMethodInUsage: messageNodeReference.	]
					ifNone: [self shouldNotHappen ])]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition ifFalse: [ ^ morph flash ].

			RefactoringApplier inlineMethodApplier
				createAndValueHandlingExceptionsOn: model textProvider
				forMethod: (MethodReference class: aSelectedClass selector: aMethodNode selector)]

! !

!SmalltalkEditor methodsFor: 'inline method' stamp: 'HAW 6/11/2022 19:58:36'!
inlineMethodInUsage: aMessageNodeReference

	RefactoringApplier inlineMethodApplier
		createAndValueHandlingExceptionsOn: model textProvider
		forMessageSend: aMessageNodeReference.! !

!SmalltalkEditor methodsFor: 'inline temporaray variable' stamp: 'HAW 6/11/2022 19:58:40'!
inlineTemporaryVariable

	"To prevent the refactoring to be evaluated on editors w/o methods like the workspace - Fernando"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	InlineTemporaryVariableApplier createAndValueHandlingExceptions: [
		InlineTemporaryVariableApplier
			on: self
			for: self selectionInterval asSourceCodeInterval
			of: self codeProvider currentCompiledMethod ]! !

!SmalltalkEditor methodsFor: 'inline temporaray variable' stamp: 'HAW 6/11/2022 19:58:43'!
inlineTemporaryVariable: aKeyboardEvent

	self inlineTemporaryVariable.
	^true! !


!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 6/11/2022 20:00:51'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel |
	sel _ model selectedMessageName.
	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $R ifTrue: [^ self renameSelector].
			aChar = $U ifTrue: [^ self addParameter ].
			aChar = $I ifTrue: [^ self removeParameter ].
			aChar = $3 ifTrue: [^ self inlineMethod ]].
	super messageListKey: aChar from: view! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 6/11/2022 19:59:39'!
inlineMethod

	model selectedMessageName ifNotNil: [ :selectorToInline |
		RefactoringApplier inlineMethodApplier
			createAndValueHandlingExceptionsOn: model
			forMethod: (MethodReference class: model selectedClassOrMetaClass selector: selectorToInline )]! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 6/11/2022 20:02:15'!
messageRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #messageRefactoringMenuOptions.! !


!MethodReference methodsFor: 'testing' stamp: 'HAW 6/11/2022 20:04:20'!
isMessageListTitle

	^(self actualClass isKindOf: Object) and: [methodSymbol first = $_]
! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 6/11/2022 20:05:08'!
registerInlineMethodApplier: anInlineMethodApplierClass

	self registerApplierAt: self inlineMethodApplierId with: anInlineMethodApplierClass ! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 6/11/2022 20:04:51'!
inlineMethodApplier

	^self applierAt: self inlineMethodApplierId ifAbsent: [ InlineMethodApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 6/11/2022 20:05:00'!
inlineMethodApplierId

	^#inlineMethodApplier! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 6/11/2022 20:05:19'!
resetInlineMethodApplier

	self resetApplierAt: self inlineMethodApplierId ! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 6/11/2022 20:06:01'!
messageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
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
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down'.
				#selector 		-> 		#pushDownSelector.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		60.
				#label 			-> 		'move to instance/class method'.
				#selector 		-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		70.
				#label 			-> 		'inline method... (3)'.
				#selector 		-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
	}`.

	! !

!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 6/11/2022 20:06:58'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract Temporary... (J)'.
				#selector 		-> 		#extractToTemporary.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Extract Method... (K)'.
				#selector 		-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Inline Temporary... (2)'.
				#selector 		-> 		#inlineTemporaryVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		50.
				#label 			-> 		'More Refactorings...'.
				#selector 		-> 		#openSmalltalkEditorRefactoringMenu.
				#icon 			-> 		#listAddIcon
		} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 6/11/2022 20:07:45'!
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
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		80.
				#label 			-> 		'Inline Method... (3)'.
				#selector 		-> 		#contextualInlineMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 6/11/2022 20:06:27'!
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
		#($2	#inlineTemporaryVariable:						'Inlines the selected temporary variable into each usage')
		#($3	#contextualInlineMethod:						'Inlines the selected message send into desired colaborations')
	)! !

!methodRemoval: MethodNode #hasTemporaryNamed: stamp: 'HAW 6/11/2022 19:41:22'!
MethodNode removeSelector: #hasTemporaryNamed:!

!InlineMethod commentStamp: '<historical>' prior: 0!
I am a refactoring that replaces message sends in the sender with the body of the target method , replacing the parameters as needed.
Implementation notes:

- I can refactor multiple message sends at the same time.
- I can delete the target method.!


!InlineMethod methodsFor: 'changes' stamp: 'FB 11/22/2021 17:01:21'!
changedMethods

	| changedMethods |

	changedMethods := (messageSendsToInline collect: [:aMessageNodeReference | aMessageNodeReference methodReference ]) asSet.
	removeMethod ifTrue: [changedMethods add: methodToInline methodReference].
	^changedMethods.! !


!InlineMethod methodsFor: 'applying' stamp: 'FB 5/21/2022 16:49:59'!
apply

	messageSendsToInline do: [:aMessageNodeReference | self inlineMessageSend: aMessageNodeReference].
	self declareNecessaryTemporaries.
	removeMethod ifTrue: [self removeMethodToInline].
	self compileChanges.

	^self changedMethods! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'FB 11/4/2021 22:04:44'!
declareNecessaryTemporaries

	temporariesToDeclareByInsertionPoint keysAndValuesDo: [:aSenderAndPosition :temporariesToDeclare |
		| sender positionToInsertTemporaries temporariesString |
		sender := aSenderAndPosition key.
		positionToInsertTemporaries := aSenderAndPosition value.
		temporariesString := temporariesToDeclare reduce: [:t1 : t2 | t1 value, ' ', t2 value].
		((sender methodNode parseNodesPathAt: positionToInsertTemporaries ifAbsent: 	[self shouldNotHappen ]) first key isTemporariesDeclaration)
			ifTrue: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: temporariesString, ' |'
				inMethod: sender.
			]
			ifFalse: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: '| ',  temporariesString, ' |', Character newLineCharacter asString
				inMethod: sender
			]
	]! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 6/11/2022 19:07:59'!
declareTemporaries: temporariesToDeclare in: aMethodOrBlockNode of: aSenderMethod

	| positionToInsertNewVariablesDeclarations renamedTemporaries temporariesToDeclareInPosition|

	renamedTemporaries := self renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod.

	aMethodOrBlockNode hasTemporaryVariables
		ifTrue: [
			positionToInsertNewVariablesDeclarations := (aSenderMethod methodNode
				singleCompleteSourceRangeOf: (aMethodOrBlockNode temporariesDeclaration)) last.
		]
		ifFalse: [
			| firstStatementRange |
			firstStatementRange := aSenderMethod methodNode singleCompleteSourceRangeOf: (aMethodOrBlockNode statements first).
			positionToInsertNewVariablesDeclarations := firstStatementRange first.
		].
	temporariesToDeclareInPosition := self temporariesToDeclareByInsertionPoint: (aSenderMethod -> positionToInsertNewVariablesDeclarations).
	temporariesToDeclareInPosition addAll: (renamedTemporaries collect: [:aReplacementAssociation | aReplacementAssociation value]).
	^renamedTemporaries.
	! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 6/11/2022 19:05:49'!
declareTemporaries: temporariesToDeclare inBlockOf: aMessageNodeReference

	| methodOrBlockNodeDeclaringNewVariables renamedTemporaries replacements |

	methodOrBlockNodeDeclaringNewVariables := self enclosingBlockOf: aMessageNodeReference.
	renamedTemporaries := self declareTemporaries: temporariesToDeclare in: methodOrBlockNodeDeclaringNewVariables of: aMessageNodeReference compiledMethod.

	replacements := self replacementsMapForMessageSend: aMessageNodeReference .
	renamedTemporaries do: [:oldVariableAndNewName |
			(self findRangesOf: oldVariableAndNewName key in: implementorCompleteSourceRanges) do: [:range |
				replacements at: range put: oldVariableAndNewName value.
			]
	]
	! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'FB 11/4/2021 23:14:31'!
declareTemporariesIfNeededFor: aMessageNodeReference

	| implementorTemporaries |

	implementorTemporaries := methodNodeToInline temporariesDeclaration allDeclaredVariableNodes.
	implementorTemporaries isEmpty ifFalse: [self declareTemporaries: implementorTemporaries
		inBlockOf: aMessageNodeReference].
	! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 6/11/2022 19:04:49'!
renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod

	"If a temporary to declare already exists in the scope, a new fresh name
	will be found for it. The method returns an OrderedCollection of (variableNode -> newVariableName)
	replacements. If the variable does not need a rename, it will still be added to the collection.
	This method also adds the new name to the map that tracks temporaries
	names in scope for the requested method or block node across the whole refactoring - Fernando"

	| alreadyDeclaredTemporaries newTemporaries |

	newTemporaries := OrderedCollection new: temporariesToDeclare size.
	alreadyDeclaredTemporaries := self declaredTemporariesFor: aMethodOrBlockNode ofSender: aSenderMethod.
	temporariesToDeclare do: [:temporary |
		| finalTemporaryName index |
		finalTemporaryName := temporary name.
		index := 1.
		[alreadyDeclaredTemporaries includes: finalTemporaryName] whileTrue: [
			finalTemporaryName := temporary name, index asString.
			index := index + 1.
		].
		alreadyDeclaredTemporaries add: finalTemporaryName.
		newTemporaries add: (temporary -> finalTemporaryName).
	].

	^newTemporaries.! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 6/11/2022 19:07:59'!
temporariesToDeclareByInsertionPoint: aSenderAndPosition

	^temporariesToDeclareByInsertionPoint keys
		detect: [:senderAndPosition | senderAndPosition = aSenderAndPosition]
		ifFound: [ :key | temporariesToDeclareByInsertionPoint at: key ]
		ifNone: [| temporariesToDeclareAtInsertionPoint |
			temporariesToDeclareAtInsertionPoint := OrderedCollection new.
			temporariesToDeclareByInsertionPoint at: aSenderAndPosition put: temporariesToDeclareAtInsertionPoint.
			temporariesToDeclareAtInsertionPoint].
	! !


!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 11/22/2021 17:32:45'!
compileChanges

	updatedSendersCode keysAndValuesDo: [:aMethodReference :aCollectionOfChanges |
		| updatedSenderSourceCode |

		updatedSenderSourceCode := aMethodReference sourceCode copyReplacing: 	aCollectionOfChanges.
		aMethodReference methodClass compile: updatedSenderSourceCode classified: aMethodReference category
	].! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 6/11/2022 19:06:09'!
declaredTemporariesFor: aParseNode ofSender: aSender

	^temporariesDeclarationsByNode keys
		detect: [:senderAndNode | senderAndNode key = aSender and: [senderAndNode value equivalentTo: aParseNode]]
		ifFound: [ :key | temporariesDeclarationsByNode at: key]
		ifNone: [| temporariesAndArgsDict |

			temporariesAndArgsDict := self temporariesAndArgsInScopeOf: aParseNode ofSender: aSender.
			temporariesDeclarationsByNode at: (aSender -> aParseNode) put: temporariesAndArgsDict.
			temporariesAndArgsDict].


	! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 3/31/2022 22:59:39'!
enclosingBlockOf: aMessageNodeReferenceWithReturnValue

	| parseNodeSourceRange methodNode |

	methodNode := aMessageNodeReferenceWithReturnValue methodNode.
	parseNodeSourceRange := aMessageNodeReferenceWithReturnValue completeSourceRange.
	^self enclosingBlockOf: parseNodeSourceRange in: methodNode.! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 3/31/2022 23:01:25'!
enclosingBlockOf: aParseNodeSourceRange in: aMethodNode

	aMethodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode
			and: [ parseNode ~= aMethodNode ]
			and: [ sourceRanges anySatisfy: [ :sourceRange |
					sourceRange first < aParseNodeSourceRange first and: [ sourceRange last > aParseNodeSourceRange last ] ] ])
				ifTrue: [ ^ parseNode ]
		].
	^aMethodNode.! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 7/13/2021 01:17:58'!
findRangesOf: aNode in: completeSourceRanges
	"This is needed because the map of source ranges compares nodes by memory
	instead of using equivalentTo: -- Fernando"

	completeSourceRanges keysAndValuesDo: [:aParseNode :sourceRanges |
		(aParseNode equivalentTo: aNode) ifTrue: [^sourceRanges]
	].
	^#().
		! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 9/5/2021 20:43:20'!
findSourceRangeOf: aMessageNodeWithReturnValue in: aSenderMethod

	^ (aSenderMethod methodNode completeSourceRangesOf: aMessageNodeWithReturnValue ifAbsent: [self shouldNotHappen]) anyOne! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 10/31/2021 23:29:24'!
removeMethodToInline

	methodToInline methodClass removeSelector: methodToInline selector.! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 11/22/2021 17:31:25'!
replaceRange: aSourceCodeInterval withNewSourceCode: newSourceCode inMethod: aCompiledMethod

	| currentUpdates sourceCodeReplacement|

	sourceCodeReplacement := newSourceCode.
	((aSourceCodeInterval last < aCompiledMethod sourceCode size) and: [((aCompiledMethod sourceCode at: aSourceCodeInterval last + 1) = $.)])
		ifTrue: [sourceCodeReplacement := newSourceCode copyUpToLast: $.].
	"It's important to use < and not <= because temporaries declarations are added to the changes collection
	after the changes concerning the inlining of the implementor code, but they need to be replaced first - Fernando"
	currentUpdates := updatedSendersCode at: aCompiledMethod methodReference ifAbsent: (SortedCollection sortBlock: [ :left :right | left key first < right key first ]).
	currentUpdates add: (aSourceCodeInterval -> sourceCodeReplacement).
	updatedSendersCode at: aCompiledMethod methodReference put: currentUpdates.
	! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 6/11/2022 19:05:49'!
replacementsMapForMessageSend: aMessageNodeReference

	^replacementsByMessageSend keys
		detect: [:messageNodeReference | messageNodeReference equivalentTo: aMessageNodeReference]
		ifFound: [ :key | replacementsByMessageSend at: key	]
		 ifNone: [| replacementsDict |

			replacementsDict := Dictionary new.
			replacementsByMessageSend at: aMessageNodeReference put: replacementsDict.
			replacementsDict ].

	! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 6/28/2021 21:08:59'!
sourceCodeOfNode: aParseNode ofSender: aSender using: completeSourceCodeRanges

	completeSourceCodeRanges keysAndValuesDo: [:aNode :aCollectionOfRanges |
		(aNode equivalentTo: aParseNode) ifTrue: [ |range|
			range := aCollectionOfRanges anyOne.
			^aSender sourceCode copyFrom: range first to: range last
		]
	].
	self shouldNotHappen.! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 6/11/2022 19:06:09'!
temporariesAndArgsInScopeOf: aParseNode ofSender: aSender

	| enclosingNodes parseNodeRange methodNode temporariesInScope |

	temporariesInScope := Set new.
	methodNode := aSender methodNode.
	methodNode arguments size > 0 ifTrue: [temporariesInScope addAll: (methodNode arguments collect: [:arg | arg name])].
	methodNode hasTemporaryVariables
		ifTrue: [temporariesInScope addAll: (methodNode temporariesDeclaration allDeclaredVariableNodes
					collect: [:temporaryNode | temporaryNode name])].
	(aParseNode isKindOf: BlockNode) ifTrue: [
		parseNodeRange := (methodNode singleCompleteSourceRangeOf: aParseNode).
		enclosingNodes := (methodNode parseNodesPathAt: parseNodeRange first ifAbsent: [self shouldNotHappen])
			collect: [:nodeAndRange | nodeAndRange key].
		enclosingNodes do: [:enclosingNode |
				enclosingNode isBlockNode ifTrue: [
					enclosingNode hasTemporaryVariables
						ifTrue: [temporariesInScope addAll: (enclosingNode temporariesDeclaration allDeclaredVariableNodes
							collect: [:temporaryNode | temporaryNode name])].
					enclosingNode arguments size > 0
						ifTrue: [temporariesInScope addAll: (enclosingNode arguments collect: [:temporaryNode | temporaryNode name])].
				].
		].
	].

	^temporariesInScope



	! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 6/11/2022 18:58:18'!
addParenthesesIfNeededTo: anExpression

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |

 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) = $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) = $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [^anExpression]
		ifFalse: [^'(', anExpression, ')'].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 6/11/2022 19:05:49'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements senderCompleteSourceRanges argumentsAndTemporariesReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	self calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.

	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	indentation := self indentationUpTo: aMessageNodeReference completeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.
	inlinedStatements := indentation, ((statementsToInline collect: [:aStatementToInline |
		self sourceCodeOfImplementorStatement: aStatementToInline withReplacements: argumentsAndTemporariesReplacements removingReturn: removeReturn
			 ])
			reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
				aStatementWithReplacements, Character newLineCharacter asString, indentation,
					(nextStatementWithReplacements copyReplaceAll: String newLineString with: String newLineString, indentation asTokens: false)]).

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 6/11/2022 19:05:49'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference usedInNode: aParseNode

	| inlinedStatements argumentsAndTemporariesReplacements senderCompleteSourceRanges usageNodeSourceRange
	 inlinedUsageStatement statementsWithReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	usageNodeSourceRange := (self findRangesOf: aParseNode in: senderCompleteSourceRanges) first.

	self calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.

	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	statementsWithReplacements := statementsToInline collect: [:aStatementToInline |
		self sourceCodeOfImplementorStatement: aStatementToInline withReplacements: argumentsAndTemporariesReplacements removingReturn: true].
	indentation := self indentationUpTo: usageNodeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.

	statementsWithReplacements size > 1
		ifTrue: [inlinedStatements := indentation, ((statementsWithReplacements allButLast)
			reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
				aStatementWithReplacements, String newLineString, indentation, (nextStatementWithReplacements
					copyReplaceAll: String newLineString with: String newLineString, indentation asTokens: false) ])]
		 ifFalse: [inlinedStatements := ''].
	inlinedUsageStatement := self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
		lastStatement: statementsToInline last replacement: statementsWithReplacements last.

	statementsWithReplacements size > 1
		ifTrue: [inlinedStatements := inlinedStatements, String newLineString, indentation, inlinedUsageStatement]
		ifFalse: [inlinedStatements := inlinedUsageStatement ].

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 6/11/2022 19:05:49'!
calculateParameterReplacementsFrom: aMessageNodeReference withRanges: senderRanges

	"Return a Dict of (sourceRange -> string) where the source range belongs to a parameter usage
	in the implementor method and the string is the variable name passed as that used parameter
	in the message node
	E.g.

	m1: aParam

	^aParam

	---
	m2
		^m1:2

	then the Dict would have one entry: (<rangeOfAParamUsage> -> 2)"
	| replacements |

	replacements := self replacementsMapForMessageSend: aMessageNodeReference.
	methodToInline methodNode arguments withIndexDo: [:anArgumentNode :argIndex | | passedArgument |
		passedArgument := aMessageNodeReference messageNode arguments at: argIndex.
		(self findRangesOf: anArgumentNode in: 	implementorCompleteSourceRanges) do: [:aRange |
			replacements at: aRange put: (self sourceCodeOfNode: passedArgument ofSender: aMessageNodeReference compiledMethod using: senderRanges).]

	].
	^replacements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 6/11/2022 19:05:49'!
calculateReplacementsFrom: aMessageNodeReference usingRanges: senderRanges

	"Return a Dict of (sourceRange -> string) where the source range belongs to a parameter usage
	or self reference in the implementor method and the string is the variable name passed as
	that used parameter in the message node, or the receiver of the message to inline for self references
	E.g.

	m1: aParam

	^aParam

	---
	m2
		^m1:2

	then the Dict would have one entry: (<rangeOfAParamUsage> -> 2)"
	| replacements |

	replacements := self replacementsMapForMessageSend: aMessageNodeReference.
	methodToInline methodNode arguments withIndexDo: [:anArgumentNode :argIndex | | passedArgument |
		passedArgument := aMessageNodeReference messageNode arguments at: argIndex.
		(self findRangesOf: anArgumentNode in: 	implementorCompleteSourceRanges) do: [:aRange |
			replacements at: aRange put: (self sourceCodeOfNode: passedArgument ofSender: aMessageNodeReference compiledMethod using: senderRanges).]

	].

	methodToInline methodNode nodesDo: [:aParseNode | (aParseNode isVariableNode and: [aParseNode referencesSelf]) ifTrue: [
		(self findRangesOf: aParseNode in: 	implementorCompleteSourceRanges) do: [:rangeOfSelfReference |
			"Given previous validations, we can be sure that the receiver is a variable node"
			replacements at: rangeOfSelfReference put: aMessageNodeReference messageNode receiver name.
		]
	]].
	^replacements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 11/16/2021 20:18:53'!
indentationUpTo: anIndex in: aSourceCode

	| indentation currentIndex character|

	currentIndex := anIndex - 1.
	indentation := ''.

	[currentIndex > 0 and: [
		character := (aSourceCode at: currentIndex).
		(character = Character tab) or: [character = Character space]
	]] whileTrue: [
		indentation := indentation, character asString.
		currentIndex := currentIndex - 1.
	].

	^(character = Character newLineCharacter) ifFalse: [^''] ifTrue: [^indentation reversed].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 6/27/2021 19:35:43'!
removeReturnSelfStatementIfNeeded: aCollectionOfStatements

	(aCollectionOfStatements last isReturnSelf)
		ifTrue: [aCollectionOfStatements removeLast].

	^aCollectionOfStatements! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 3/13/2022 17:44:17'!
sourceCodeOfImplementorStatement: aStatementToInline withReplacements: replacementsDict removingReturn: removeReturn

	| sourceRange originalSourceCode statementReplacements |

	sourceRange := (self findRangesOf: aStatementToInline in: implementorCompleteSourceRanges) anyOne.
	originalSourceCode := methodToInline sourceCode copyFrom: sourceRange first to: sourceRange last.
	"Replacements must be adjusted because the statement string begins at index 1 instead of the
	original position on the source code"
	statementReplacements := ((replacementsDict associations select: [:anAssociation | sourceRange includesAllOf: (anAssociation key)])
		collect: [:anAssociation | ((anAssociation key first - sourceRange first + 1) to: (anAssociation key last - sourceRange first + 1)) -> anAssociation value])
		asSortedCollection: [ :left :right | left key first < right key first ].

	"Remove the return character if present"
	(removeReturn and: [aStatementToInline isReturn]) ifTrue: [statementReplacements add: (1 to: 1) -> ''].
	^(originalSourceCode copyReplacing: statementReplacements), '.'.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 11/21/2021 20:29:45'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression addingParentheses: addParentheses

	| sourceRangeOfUsage usageNodeSourceRange normalizedMessageSendRange expression|

	usageNodeSourceRange := (self findRangesOf: aParseNode in: aMessageNodeReference methodNode completeSourceRanges) first.
	sourceRangeOfUsage := aMessageNodeReference completeSourceRange.
	normalizedMessageSendRange := (sourceRangeOfUsage first - usageNodeSourceRange first + 1)
					to: (sourceRangeOfUsage last - usageNodeSourceRange first + 1).
	expression := addParentheses ifTrue: [(self addParenthesesIfNeededTo: (anExpression copyUpToLast: $.)), '.'] ifFalse: [anExpression].

	^(aMessageNodeReference compiledMethod sourceCode copyFrom: usageNodeSourceRange first to: usageNodeSourceRange last)
		copyReplacing: (Array with: ((normalizedMessageSendRange -> expression)))! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 3/13/2022 17:44:17'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline
	replacement: lastStatementWithReplacements

	| inlinedStatementContainingMessageSend |

	implementorLastStatementToInline expr isMessageNode
		ifTrue: [ | senderSourceRange enclosingNode |
			senderSourceRange := aMessageNodeReference completeSourceRange.
			(aMessageNodeReference methodNode parseNodesPathAt: senderSourceRange first
				ifAbsent: [self shouldNotHappen])
					detect: [:aNodeAndRange | (aNodeAndRange value includesAllOf: senderSourceRange)
						and: [aNodeAndRange value first < senderSourceRange first]]
					ifFound: [:aNodeAndRange |
						enclosingNode := aNodeAndRange key.
					]
					ifNone: [self shouldNotHappen].
			inlinedStatementContainingMessageSend := self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
				inlinedWith: lastStatementWithReplacements addingParentheses: enclosingNode isMessageNode.
		]
		ifFalse: [
			inlinedStatementContainingMessageSend := self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
				inlinedWith: lastStatementWithReplacements addingParentheses: false.
		].

	^inlinedStatementContainingMessageSend.! !


!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 3/13/2022 17:44:17'!
findOutermostStatementContaining: aMessageNodeReference enclosedBy: enclosingBlock ifFound: ifFoundBlock ifNone: ifNoneBlock

	| usageNodesPath isDifferentMessageNode isIncludedInBlock |

	usageNodesPath := aMessageNodeReference methodNode parseNodesPathAt: aMessageNodeReference completeSourceRange first
		ifAbsent: [self shouldNotHappen].
	isDifferentMessageNode := [:aNode | aNode isMessageNode and: [(aNode equivalentTo: aMessageNodeReference messageNode) not]].
	isIncludedInBlock := [:aRange | |sourceRangeOfEnclosingBlock |
		sourceRangeOfEnclosingBlock := enclosingBlock isBlockNode
			ifTrue: [self findSourceRangeOf: enclosingBlock in: aMessageNodeReference compiledMethod.]
			ifFalse: [(1 to: aMessageNodeReference compiledMethod sourceCode size)].
		sourceRangeOfEnclosingBlock includesAllOf: aRange.
	].
	usageNodesPath reversed
		detect: [:aNodeAndRange | | node range |
			node := aNodeAndRange key.
			range := aNodeAndRange value.
			((node isAssignmentNode or: [isDifferentMessageNode value: node]) and: [isIncludedInBlock value: range])
		]
		ifFound: [:aNodeAndRange | ^ifFoundBlock value: aNodeAndRange]
		ifNone: [^ifNoneBlock value].




	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 3/6/2022 14:52:08'!
inlineMessageSend: aMessageNodeReference

	| statementsToInline |

	statementsToInline := self removeReturnSelfStatementIfNeeded: (methodToInline methodNode block statements).
	statementsToInline isEmpty ifTrue: [^self].
	statementsToInline last isReturn
		ifTrue: [self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReference.]
		ifFalse: [self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference
			 removingReturn: true].


	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 5/25/2022 20:44:39'!
inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReferenceWithReturnValue

	| enclosingBlock |

	enclosingBlock := self enclosingBlockOf: aMessageNodeReferenceWithReturnValue.
	(self parseNode: enclosingBlock returns: aMessageNodeReferenceWithReturnValue messageNode)
		ifTrue: [^self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
			removingReturn: false].

	"Find the outermost statement containing the usage to inline that's inside the enclosing block"
	self findOutermostStatementContaining: aMessageNodeReferenceWithReturnValue enclosedBy: enclosingBlock
		ifFound: [ :outermostNodeAndRange |
			self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReferenceWithReturnValue
				usedInParseNode: outermostNodeAndRange	 key
		]
		ifNone: [ | includeLastStatement statements lastStatementOfEnclosingBlock |
			lastStatementOfEnclosingBlock := (self removeReturnSelfStatementIfNeeded:
				((enclosingBlock isBlockNode ifTrue: enclosingBlock ifFalse: enclosingBlock block)
					statements)) last.
			includeLastStatement := (lastStatementOfEnclosingBlock equivalentTo: aMessageNodeReferenceWithReturnValue messageNode)
				or: [lastStatementOfEnclosingBlock equivalentTo: aMessageNodeReferenceWithReturnValue messageNode asReturnNode].
			statements := includeLastStatement ifTrue: statementsToInline ifFalse: statementsToInline allButLast.
			self inlineStatements: statements ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
				removingReturn: true
		]




	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 11/16/2021 21:29:22'!
inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeWithReturnValueReference usedInParseNode: aParseNode

	| inlinedStatements sourceRangeOfUsage senderMethod|

	inlinedStatements := self buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeWithReturnValueReference
		usedInNode: aParseNode.

	senderMethod := aMessageNodeWithReturnValueReference compiledMethod.
	sourceRangeOfUsage := self findSourceRangeOf: aParseNode in: senderMethod.

	self replaceRange: sourceRangeOfUsage withNewSourceCode: inlinedStatements inMethod: senderMethod.


	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 5/18/2022 15:01:06'!
inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements sourceRangeToReplace senderMethod senderSourceCode |

	senderMethod := aMessageNodeReference compiledMethod.
	senderSourceCode := aMessageNodeReference compiledMethod sourceCode.
	statementsToInline isEmpty
		ifTrue: [
			inlinedStatements := ''.
			sourceRangeToReplace := aMessageNodeReference completeSourceRange asSourceCodeInterval
				expandUntilStartOfNextStatementOn: senderSourceCode.
		]
		ifFalse: [
			inlinedStatements := self buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference
				removingReturn: removeReturn.
			removeReturn
				ifTrue: [sourceRangeToReplace := aMessageNodeReference completeSourceRange]
				ifFalse: 	[
					(self enclosingBlockOf: aMessageNodeReference) nodesDo: [:aNode |
						(aNode isReturn and: [aNode expr equivalentTo: aMessageNodeReference messageNode ])
							ifTrue: [sourceRangeToReplace := (self findSourceRangeOf: aNode in: senderMethod)
								asSourceCodeInterval expandUntilStartOfNextStatementOn: senderSourceCode ] ]
				].
		].

	self replaceRange: sourceRangeToReplace withNewSourceCode: inlinedStatements inMethod: senderMethod.



	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 11/3/2021 22:59:59'!
parseNode: aParseNode returns: aMessageNode

	aParseNode nodesDo: [:aNode | (aNode isReturn and: [aNode expr equivalentTo: aMessageNode])
		ifTrue: [^true]].
	^false.




	! !


!InlineMethod methodsFor: 'initialization' stamp: 'FB 3/6/2022 15:12:28'!
methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	updatedSendersCode := Dictionary ofSize: ((messageSendsToInline groupBy: [:aMessageNodeReference | aMessageNodeReference methodReference]) keys) size.
	methodNodeToInline := methodToInline methodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethod class' category: #'Tools-Refactoring'!
InlineMethod class
	instanceVariableNames: ''!

!InlineMethod class methodsFor: 'initialization' stamp: 'FB 4/14/2022 15:14:25'!
from: aCompiledMethodToInline intoSendersAndUsages: aCollectionOfMessageSends removeMethod: removingMethod

	self assertCanInlineMethod: aCompiledMethodToInline intoMessageSends: aCollectionOfMessageSends.

	^self new
		methodToInline: aCompiledMethodToInline
		usages: aCollectionOfMessageSends
		removingMethod: removingMethod.! !


!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 18:00:47'!
assertCanInlineMethod: aCompiledMethodToInline intoMessageSends: aCollectionOfMessageSends

	| senders |

	self assertNoCascadingMessages: aCollectionOfMessageSends.
	senders := aCollectionOfMessageSends collect: [:aMessageNodeReference | aMessageNodeReference actualClass].
	self assertSenders: senders haveAccessToPrivateVariablesOf: aCompiledMethodToInline.
	self assertMethodToInlineHasOnlyOneReturn: aCompiledMethodToInline.

	"When the method to inline references self, only allow inlining on statements like:
		variable := ClassOfMethodToInline new.
		variable methodToInline.
	but not on:
		ClassOfMethodToInline new methodToInline
	to be able to replace the references to self with the variable name
	"

	(aCompiledMethodToInline referencesSelf) ifTrue: [
		(aCollectionOfMessageSends allSatisfy: [:aSender | (self sender: aSender belongsToClassOf: aCompiledMethodToInline)
			or: [aSender messageNode receiver isVariableNode ]]) ifFalse: [
				self signalMethodToInlineReferencesSelf
			]
	].

	"Do not allow inlining implementors that reference super unless all the senders belong to the same class,
	since otherwise it's not possible to replace the super keyword with something that maintains the semantics"
	(aCompiledMethodToInline referencesSuper) ifTrue: [
		self assertSenders: aCollectionOfMessageSends belongToClassOf: aCompiledMethodToInline
			ifFalse: [self signalMethodToInlineReferencesSuper] .
	].
	! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 10/16/2021 21:26:35'!
assertMethodToInlineHasOnlyOneReturn: aCompiledMethod

	| returnsCount |

	returnsCount := 0.
	aCompiledMethod methodNode nodesDo: [:aNode | aNode isReturn ifTrue: [
			returnsCount := returnsCount  + 1.
			returnsCount > 1 ifTrue: [self signalMethodHasMultipleReturnsErrorMessage ]
		]
	]! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 4/14/2022 15:08:07'!
assertNoCascadingMessages: aCollectionOfMessageSends

	(aCollectionOfMessageSends anySatisfy: [:aMessageNodeReference |
		aMessageNodeReference messageNode isCascade ]) ifTrue: [self signalMessageSendCanNotBeCascade]! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 16:36:12'!
assertSenders: aCollectionOfSenders belongToClassOf: aCompiledMethod ifFalse: ifFalseBlock

	(aCollectionOfSenders allSatisfy: [:aSender | self sender: aSender belongsToClassOf: aCompiledMethod])
		ifFalse: [^ifFalseBlock value].! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 15:38:18'!
assertSenders: aSendersCollection haveAccessToPrivateVariablesOf: aCompiledMethod

	| methodReferencesInstanceVariables |

	methodReferencesInstanceVariables := false.
	aCompiledMethod methodNode
		nodesDo: [:aNode | aNode isInstanceVariableNode ifTrue: [methodReferencesInstanceVariables := true]].

	methodReferencesInstanceVariables ifTrue: [
		(aSendersCollection allSatisfy: [:aSenderClass | aSenderClass = aCompiledMethod methodClass])
			ifFalse: [self signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage]
	].
	 ! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 4/9/2022 00:04:03'!
assertSendersHaveAccessToPrivateVariables: aSendersCollection of: aCompiledMethod

	| methodReferencesInstanceVariables |

	methodReferencesInstanceVariables := false.
	aCompiledMethod methodNode
		nodesDo: [:aNode | aNode isInstanceVariableNode ifTrue: [methodReferencesInstanceVariables := true]].

	methodReferencesInstanceVariables ifTrue: [
		(aSendersCollection allSatisfy: [:aSenderClass | aSenderClass = aCompiledMethod methodClass])
			ifFalse: [self signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage]
	].
	 ! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 17:16:46'!
methodToInlineReferencesSelfErrorMessage

	^'The method to inline references self, but at least one sender does not have a variable as the receiver and it
	 does not belong to the same class'! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 16:29:27'!
sender: aSender belongsToClassOf: aCompiledMethod

	^(aSender actualClass = aCompiledMethod methodClass)! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'FB 5/21/2022 17:15:10'!
signalMethodToInlineReferencesSelf

	self refactoringError: self methodToInlineReferencesSelfErrorMessage! !


!InlineMethod class methodsFor: 'errors' stamp: 'FB 4/14/2022 15:11:22'!
messageSendCanNotBeCascadeErrorMessage

	^'Refactoring a cascade message send is not supported'.! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 4/8/2022 23:35:57'!
methodAccessPrivateVariablesNotVisibleToSenderErrorMessage

	^'Method to inline is accessing private variables that are not visible to at least one sender'! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 10/16/2021 21:16:21'!
methodHasMultipleReturnsErrorMessage

	^'Method to inline has more than one possible return value'! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 5/21/2022 16:39:45'!
methodToInlineReferencesSuperErrorMessage

	^'The method to inline references super but there is at least one sender that does not belong to the same class'! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 4/14/2022 15:09:36'!
signalMessageSendCanNotBeCascade

	^self refactoringError: self messageSendCanNotBeCascadeErrorMessage.! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 4/8/2022 23:36:18'!
signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage

	^self refactoringError: self methodAccessPrivateVariablesNotVisibleToSenderErrorMessage.! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 10/16/2021 21:16:58'!
signalMethodHasMultipleReturnsErrorMessage

	^self refactoringError: self methodHasMultipleReturnsErrorMessage.! !

!InlineMethod class methodsFor: 'errors' stamp: 'FB 5/21/2022 16:38:43'!
signalMethodToInlineReferencesSuper

	self refactoringError: self methodToInlineReferencesSuperErrorMessage! !


!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:53:44'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages forClassAndMetaOf: aPotentialClassToRefactor

	self addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages of: aPotentialClassToRefactor theNonMetaClass.
	self addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages of: aPotentialClassToRefactor theMetaClass.

! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:53:48'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	| categories |

	categories := Set new.

	self
		addImplementorsOf: aSelectorToInline
		to: implementors
		andUsagesTo: usages
		inHierarchyOf: aClass
		doingPerClass: [:aClassInHierarchy | categories add: aClassInHierarchy category ].

	categories do: [:aCategory |
		self
			addImplementorsOf: aSelectorToInline
			to: implementors
			andUsagesTo: usages
			inCategory: aCategory
			organizedBy: anOrganization ].
! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 12/21/2021 20:24:21'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages inCategory: aCategory organizedBy: anOrganization

	| classesInCategory |

	classesInCategory := anOrganization classesAt: aCategory.
	classesInCategory do: [ :aPotentialClassToRefactor |
		self
			addImplementorsOf: aSelectorToInline
			to: implementors
			andUsagesTo: usages
			forClassAndMetaOf: aPotentialClassToRefactor ].! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:53:54'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages inHierarchyOf: aClass

	self
		addImplementorsOf: aSelectorToInline
		to: implementors
		andUsagesTo: usages
		inHierarchyOf: aClass
		doingPerClass: [ :aClassInHierarchy | ]

	! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:53:57'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages inHierarchyOf: aClass doingPerClass: aBlock

	| highestClassImplementingOldSelector |

	highestClassImplementingOldSelector := aClass highestClassImplementing: aSelectorToInline ifNone: [ aClass ].
	highestClassImplementingOldSelector theNonMetaClass withAllSubclassesDo: [ :aPotentialClassToRefactor |
		aPotentialClassToRefactor isMeta ifFalse: [
			self
				addImplementorsOf: aSelectorToInline
				to: implementors
				andUsagesTo: usages
				forClassAndMetaOf: aPotentialClassToRefactor.
			aBlock value: aPotentialClassToRefactor ]]
	! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:52:25'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages inSystem: aSystem

	aSystem allBehaviorsDo: [ :aPotentialClassToRefactor |
		self addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages of: aPotentialClassToRefactor ].
! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 11/13/2021 21:50:33'!
addImplementorsOf: aSelectorToInline to: implementors andUsagesTo: usages of: aPotentialClassToRefactor

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not register
	as allowed compiler - Hernan"
	(self canRefactor: aPotentialClassToRefactor) ifFalse: [ ^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: aSelectorToInline ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: aSelectorToInline) do: [ :aSelector |
		usages addAll: (self findReferencesToSelector: aSelectorToInline in: (MethodReference class: aPotentialClassToRefactor selector: aSelector))]! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 5/5/2022 22:14:13'!
convertToSender: aSenderSelector of: aPotentialClassToRefactor referencing: aSelector

	^(self findReferencesToSelector: aSelector
		in: (MethodReference class: aPotentialClassToRefactor selector: aSenderSelector))! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'FB 10/31/2021 22:48:46'!
findReferencesToSelector: aSelectorToInline in: aMethodReference

	| references |

	references := Set new.
	aMethodReference compiledMethod methodNode completeSourceRangesDo: [:aNode :ranges |
		(aNode isMessageNode and: [aNode selector key = aSelectorToInline]) ifTrue: [
			ranges do: [:range  | references add: (MessageNodeReference messageNode: aNode
				selector: aMethodReference methodSymbol class: aMethodReference methodClass completeSourceRange: range)]
		]
	].

	^references.

	! !

!InlineMethodApplier commentStamp: '<historical>' prior: 0!
I can apply the InlineMethod refactoring to a message. I am in charge of handling the configuration windows and instantiating an instance of InlineMethod.
Implementation notes:

- A single implementor will have to be chosen.
- More than one message send can be refactored.
- An option to delete the implementor is provided.!


!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/11/2022 19:14:25'!
askForImplementorsAndMessageSends

	self
		initializeImplementorsAndMessageSends;
		calculateImplementorsAndMessageSends;
		openImplementorSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/11/2022 19:14:09'!
askForImplementorsOnly

	self
		initializeImplementors;
		calculateImplementorsAndMessageSends;
		openImplementorSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/11/2022 19:14:36'!
askForMessagesToInlineOnly

	self
		initializeMessageSends;
		calculateImplementorsAndMessageSends;
		openMessageSendsSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/22/2021 19:06:58'!
askIfImplementorShouldBeRemoved

	self askIfImplementorShouldBeRemoved: 'Do you want to remove the implementor method from the system?'.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/22/2021 19:06:41'!
askIfImplementorShouldBeRemoved: aMessage

	shouldRemoveImplementor := PopUpMenu confirm: aMessage.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/22/2021 19:12:11'!
askIfImplementorShouldBeRemovedWhenNoSenders

	self askIfImplementorShouldBeRemoved: 'This message has no senders so inlining is a no-op. Do you want to remove the implementor method from the system?'.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:26:45'!
askIfOnlyTriggeringMessageSendShouldBeInlined

	| scopeMenu choice |

	scopeMenu := PopUpMenu labelArray: #('Inline only this message send' 'Select message sends to inline').
	choice := scopeMenu startUpWithCaption: 'What message sends should be inlined?'.
	choice = 0 ifTrue: [ self endRequest ].
	shouldInlineTriggeringMessageSendOnly := choice = 1! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 10/18/2021 19:48:26'!
askScope

	| scopeMenu |

	scopeMenu := PopUpMenu labelArray: self scopeOptionLabels.
	scopeChoice := scopeMenu startUpWithCaption: 'Select Refactoring Scope'.
	scopeChoice = 0 ifTrue: [ self endRequest ].
	! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:23:13'!
calculateImplementorsAndMessageSends

	scopeChoice = 1 ifTrue: [ ^self implementorsAndMessageSendsForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndMessageSendsForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndMessageSendsInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndMessageSendsInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndMessageSendsInSystem ].

	self error: 'Unknown scope option'

		! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 10/24/2021 15:03:32'!
createImplementors

	^IdentitySet new
! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:24:50'!
createMessageSends

	^OrderedCollection new.
! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:41'!
implementorsAndMessageSendsForClass

	^self refactoringClass
		addImplementorsOf: selectorToInline
		to: self implementorsOrDiscardCollection
		andUsagesTo: self messageSendsOrDiscardCollection
		forClassAndMetaOf: selectedClass! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:41'!
implementorsAndMessageSendsForHierarchy

	^self refactoringClass
		addImplementorsOf: selectorToInline
		to: self implementorsOrDiscardCollection
		andUsagesTo: self messageSendsOrDiscardCollection
		inHierarchyOf: selectedClass! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:41'!
implementorsAndMessageSendsInCategory

	^self refactoringClass
		addImplementorsOf: selectorToInline
		to: self implementorsOrDiscardCollection
		andUsagesTo: self messageSendsOrDiscardCollection
		inCategory: selectedClass category
		organizedBy: SystemOrganization! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:41'!
implementorsAndMessageSendsInCategoryAndHierarchy

	^self refactoringClass
		addImplementorsOf: selectorToInline
		to: self implementorsOrDiscardCollection
		andUsagesTo: self messageSendsOrDiscardCollection
		inCategoriesAndHierarchyOf: selectedClass
		organizedBy: SystemOrganization ! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:41'!
implementorsAndMessageSendsInSystem

	^self refactoringClass
		addImplementorsOf: selectorToInline
		to: self implementorsOrDiscardCollection
		andUsagesTo: self messageSendsOrDiscardCollection
		inSystem: Smalltalk ! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 3/19/2022 18:39:34'!
implementorsOrDiscardCollection

	"This is implemented like this so we don't have to duplicate every implementorsAndMessageSendsInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForImplementors ifTrue: [implementors] ifFalse: [^Set new]! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/13/2021 18:19:25'!
initializeImplementors

	implementors := self createImplementors.
	! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:24:27'!
initializeImplementorsAndMessageSends

	self initializeImplementors.
	self initializeMessageSends.
	! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:24:50'!
initializeMessageSends

	messageSends := self createMessageSends
	! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/25/2021 00:25:03'!
messageSendsOrDiscardCollection

	"This is implemented like this so we don't have to duplicate every implementorsAndUsagesInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForMessageSendsToInline ifTrue: [messageSends] ifFalse: [^Set new].! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 3/3/2022 21:48:32'!
openImplementorSelectionWindow

	| windowClass |

	implementors isEmpty ifTrue: [self handleRefactoringError: (RefactoringError signal: self class implementorsNotFoundErrorMessage)].
	windowClass := self shouldAskForMessageSendsToInline
		ifTrue: [InlineMethodImplementorsWithShowUsagesStepWindow]
		ifFalse: [InlineMethodImplementorsStepWindow].
	^windowClass openFrom: self.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:27:26'!
openMessageSendsSelectionWindow

	| windowClass |

	windowClass := self shouldAskForImplementors
		ifTrue: [InlineMethodUsagesWithShowImplementorsStepWindow]
		ifFalse: [InlineMethodUsagesStepWindow].
	^windowClass openFrom: self.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 11/24/2021 00:26:21'!
requestRefactoringParameters

	self refactoringRequestedFromUsage ifTrue: [self askIfOnlyTriggeringMessageSendShouldBeInlined].
	self askIfImplementorShouldBeRemoved.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'FB 10/18/2021 19:48:26'!
scopeOptionLabels

	^{'In Class'. 'In Hierarchy'. 'In Category'. 'In Hierarchy and its Categories'. 'In System'}.! !


!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 10/31/2021 16:49:55'!
implementors

	^implementors ! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/13/2021 21:15:51'!
implementors: aCollectionOfImplementors

	implementors := aCollectionOfImplementors ! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/24/2021 20:05:21'!
messageSendAt: anIndex ifAbsent: ifAbsentBlock

	^messageSends at: anIndex ifAbsent: ifAbsentBlock! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/25/2021 00:24:47'!
messageSends: aCollectionOfMessageSends

	messageSends := aCollectionOfMessageSends reject: [:aMessageNodeReference | aMessageNodeReference isMessageListTitle ]! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/25/2021 00:25:16'!
messageSendsToInline

	^messageSends ! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/24/2021 00:23:25'!
methodsOfUsagesToInline

	^messageSends collect: [:aMessageNodeReference | aMessageNodeReference methodReference]! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 10/31/2021 16:50:12'!
selectorToInline

	^selectorToInline ! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/22/2021 17:42:30'!
shouldAskForImplementors

	^triggeringMessageSend isNil not.! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 11/25/2021 00:25:03'!
shouldAskForMessageSendsToInline

	^shouldInlineTriggeringMessageSendOnly not.! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'FB 10/18/2021 19:48:26'!
wizardStepWindow: aWizarStepWindow

	wizardStepWindow := aWizarStepWindow ! !


!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'FB 10/18/2021 19:48:26'!
closeBrowser

	wizardStepWindow delete.
	! !

!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'FB 10/18/2021 19:48:26'!
doNotShowChanges

	shouldShowChanges := false! !

!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'FB 10/18/2021 19:48:26'!
messageSetWindowClass

	^MessageSetWindow
	! !

!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'FB 10/18/2021 19:48:26'!
showChanges

	self showChangesInMessageSetWindow! !

!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'FB 10/18/2021 19:48:26'!
showChangesInMessageSetWindow

	self messageSetWindowClass openMessageList: changes asSortedCollection label: 'Changed methods' ! !


!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'FB 6/3/2022 17:27:44'!
createAndApplyRefactoring

	self
		removeMessageSendsBelongingToImplementor;
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring.

	shouldShowChanges ifTrue: [ self showChanges ]

			! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'FB 11/24/2021 00:23:25'!
createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor

	implementors := Array with: anImplementor.
	messageSends := #().
	shouldShowChanges := false.

	self createAndApplyRefactoring ! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'FB 11/24/2021 00:23:25'!
createRefactoring

	self assert: implementors size = 1.
	^self refactoringClass
		from: (implementors anyOne)
		intoSendersAndUsages: messageSends
		removeMethod: shouldRemoveImplementor

			! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'FB 10/18/2021 19:50:16'!
refactoringClass

	^InlineMethod ! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'FB 6/3/2022 17:27:29'!
removeMessageSendsBelongingToImplementor

	messageSends := messageSends reject: [:messageNodeReference |
		messageNodeReference compiledMethod equivalentTo: implementors anyOne
	]
! !


!InlineMethodApplier methodsFor: 'initialization' stamp: 'FB 11/24/2021 00:26:29'!
initializeOn: aBrowser for: aSelector in: aSelectedClass

	"To be used when initiating refactoring from message selector pane"

	selectorToInline := aSelector.
	selectedClass := aSelectedClass.
	browser := aBrowser.
	shouldInlineTriggeringMessageSendOnly := false.
	shouldShowChanges := true.! !

!InlineMethodApplier methodsFor: 'initialization' stamp: 'FB 11/22/2021 17:42:30'!
initializeOn: aBrowser forMessageSend: aMessageNodeReference

	"To be used when initiating refactoring from a message send"

	selectorToInline := aMessageNodeReference selectorOfMessageNode.
	triggeringMessageSend := aMessageNodeReference.
	selectedClass := aMessageNodeReference actualClass.
	browser := aBrowser.
	shouldShowChanges := true.! !


!InlineMethodApplier methodsFor: 'evaluation' stamp: 'FB 11/22/2021 19:10:54'!
ifHasNoUsagesAndOneImplementor: trueBlock ifNot: falseBlock

	| allImplementors |

	allImplementors := Smalltalk allImplementorsOf: selectorToInline.

	"I could try to see if there is one sender and that that sender is in the same method beeing renamed. That could
	mean that it is a recursive call but I should also see if the receiver is self to be sure because if it is other 'type' of
	object the rename could not be safe. To complex for a small posibility - Hernan"
	(allImplementors size = 1 and: [ (Smalltalk allCallsOn: selectorToInline) isEmpty ])
		ifTrue: [ trueBlock value: allImplementors anyOne compiledMethod ]
		ifFalse: falseBlock! !

!InlineMethodApplier methodsFor: 'evaluation' stamp: 'FB 11/22/2021 17:42:30'!
refactoringRequestedFromUsage

	^triggeringMessageSend isNil not.! !

!InlineMethodApplier methodsFor: 'evaluation' stamp: 'FB 3/5/2022 14:06:26'!
value

	requestExitBlock := [ ^self ].

	self requestRefactoringParametersHandlingRefactoringExceptions.

	self
		ifHasNoUsagesAndOneImplementor: [ :anImplementor |
			shouldRemoveImplementor ifFalse: [self askIfImplementorShouldBeRemovedWhenNoSenders ]. "Give the user another chance to delete the message"
			shouldRemoveImplementor ifFalse: [^self].
			self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor
		]
		ifNot: [
			self askScope.
			shouldInlineTriggeringMessageSendOnly ifTrue: [
				messageSends := OrderedCollection new.
				messageSends add: triggeringMessageSend.
				^self askForImplementorsOnly
			].
			self shouldAskForImplementors ifFalse: [implementors := IdentitySet with: (selectedClass >> selectorToInline). ^self askForMessagesToInlineOnly].
			self askForImplementorsAndMessageSends.
	]! !

!InlineMethodApplier methodsFor: 'evaluation' stamp: 'FB 10/18/2021 19:48:26'!
wizardEnded

	requestExitBlock := [ ^self ].

	self
		closeBrowser;
		createAndApplyRefactoring.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethodApplier class' category: #'Tools-Refactoring'!
InlineMethodApplier class
	instanceVariableNames: ''!

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 12/21/2021 19:57:14'!
createAndValueHandlingExceptions: creationBlock


	[super createAndValueHandlingExceptions: creationBlock]
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].
! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 11/22/2021 17:42:17'!
createAndValueHandlingExceptionsOn: aModel forMessageSend: aMessageNodeReference

	self createAndValueHandlingExceptions: [ self initializeOn: aModel forMessageSend: aMessageNodeReference ].
! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 11/22/2021 17:44:22'!
createAndValueHandlingExceptionsOn: aModel forMethod: aMethodReference

	self createAndValueHandlingExceptions: [ self initializeOn: aModel forMethod: aMethodReference ].
! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 3/3/2022 21:42:58'!
implementorsNotFoundErrorMessage

	^'There are no implementors for this message in the selected scope'! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 11/22/2021 17:42:17'!
initializeOn: aModel forMessageSend: aMessageNodeReference

	^self new initializeOn: aModel forMessageSend: aMessageNodeReference
	! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'FB 11/22/2021 18:27:52'!
initializeOn: aModel forMethod: aMethodReference

	"To be called when calling from method selector or when inlining self method from the code editor"

	^self new initializeOn: aModel for: aMethodReference selector in: aMethodReference actualClass.! !

!InlineMethodWizardStepWindow methodsFor: 'actions' stamp: 'FB 10/18/2021 20:25:55'!
do: aBlock withEnteredClassLabeled: aLabel
	ClassNameRequestMorph
		request: aLabel
		initialAnswer: ''
		do: [:className| self withClassNamed: className do: aBlock]! !

!InlineMethodWizardStepWindow methodsFor: 'actions' stamp: 'FB 10/18/2021 20:25:55'!
inform: aClass doesNotImplement: aSelector

 	self inform: aClass name, ' does not implement #', aSelector ! !

!InlineMethodWizardStepWindow methodsFor: 'actions' stamp: 'FB 10/18/2021 20:25:55'!
justRefactor

	applier doNotShowChanges.
	self refactor.! !

!InlineMethodWizardStepWindow methodsFor: 'actions' stamp: 'FB 10/18/2021 20:25:55'!
refactor

	applier wizardStepWindow: self.
	applier wizardEnded.
	! !

!InlineMethodWizardStepWindow methodsFor: 'actions' stamp: 'FB 10/18/2021 20:25:55'!
withClassNamed: aName do: aBlock

	| trimmedNamed |

	trimmedNamed := aName withBlanksTrimmed.

	(Smalltalk classNamed: trimmedNamed asSymbol)
		ifNotNil: aBlock
		ifNil: [ self inform: 'Class ', trimmedNamed , ' does not exist' ].
! !


!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'FB 10/18/2021 20:25:55'!
addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10! !

!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'FB 10/18/2021 20:25:55'!
addButtonsTo: row color: buttonColor

	self subclassResponsibility ! !

!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'FB 10/18/2021 20:25:55'!
buildLowerPanes

	| codeAndButtons  |

	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.

	^codeAndButtons ! !

!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'FB 10/18/2021 20:25:55'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.

	^row

	! !


!InlineMethodWizardStepWindow methodsFor: 'compile methods' stamp: 'FB 10/18/2021 20:25:55'!
compiledMethodsFrom: methodReferences

	"If the method is not implemented, I leave the not implemented reference because actual senders of it
	should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan"
	^ methodReferences collect: [:aMethodReference |
		aMethodReference compiledMethodIfAbsent: [ aMethodReference ]]! !


!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'FB 10/18/2021 20:25:55'!
createCancelButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #delete
		label: 'Cancel'.
! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'FB 10/18/2021 20:25:55'!
createJustRefactorButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #justRefactor
		label: 'Just Refactor!!'! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'FB 10/18/2021 20:25:55'!
createRefactorButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'FB 11/22/2021 16:23:59'!
createRemoveButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #remove
		label: 'Remove'.
! !


!InlineMethodWizardStepWindow methodsFor: 'initialization' stamp: 'FB 10/31/2021 17:27:23'!
initializeFrom: anInlineMethodApplier

	applier := anInlineMethodApplier ! !


!InlineMethodWizardStepWindow methodsFor: 'testing' stamp: 'FB 10/18/2021 20:25:55'!
isMessageSelected

	^model isNil ifTrue: [ false ] ifFalse: [ model selection notNil ]! !


!InlineMethodWizardStepWindow methodsFor: 'accessing' stamp: 'FB 10/31/2021 17:26:48'!
selectorToInline

	^applier selectorToInline ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethodWizardStepWindow class' category: #'Tools-Refactoring'!
InlineMethodWizardStepWindow class
	instanceVariableNames: ''!

!InlineMethodWizardStepWindow class methodsFor: 'instance creation' stamp: 'FB 11/22/2021 15:10:45'!
methodReferencesOf: methods

	^methods asOrderedCollection collect: [:aCompiledMethod | aCompiledMethod methodReference ].
! !

!InlineMethodWizardStepWindow class methodsFor: 'instance creation' stamp: 'FB 11/13/2021 21:16:10'!
openFrom: anInlineMethodApplier methods: methods label: aLabel selecting: somethingToSelect

	| window |

	window := self openMessageList: methods label: aLabel autoSelect: somethingToSelect.
	window initializeFrom: anInlineMethodApplier.

	^window

! !

!InlineMethodImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 11/22/2021 15:25:47'!
justRefactor

	self setImplementorInApplier.
	super justRefactor.! !

!InlineMethodImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 11/22/2021 15:54:27'!
refactor

	self setImplementorInApplier.
	super refactor.! !

!InlineMethodImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 11/14/2021 17:07:45'!
selectImplementor

	selectedImplementor := model selection.
	self refactor.! !

!InlineMethodImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 11/21/2021 21:00:23'!
setImplementorInApplier

	selectedImplementor := selectedImplementor ifNil: [model selection].
	applier implementors: (self compiledMethodsFrom: (Array with: selectedImplementor)).
	! !


!InlineMethodImplementorsStepWindow methodsFor: 'GUI building' stamp: 'FB 11/22/2021 15:52:57'!
addButtonsTo: row color: buttonColor

	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.
! !


!InlineMethodImplementorsStepWindow methodsFor: 'button creation' stamp: 'FB 11/13/2021 19:03:50'!
createSelectImplementorButton

	^PluggableButtonMorph
		model: self
		stateGetter: #isMessageSelected
		action: #selectImplementor
		label: 'Select'.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethodImplementorsStepWindow class' category: #'Tools-Refactoring'!
InlineMethodImplementorsStepWindow class
	instanceVariableNames: ''!

!InlineMethodImplementorsStepWindow class methodsFor: 'instance creation' stamp: 'FB 10/31/2021 22:16:04'!
openFrom: anInlineMethodApplier

	^self
		openFrom: anInlineMethodApplier
		methods: (self methodReferencesOf: anInlineMethodApplier implementors)
		label: 'Implementors of #', anInlineMethodApplier selectorToInline, ' to Refactor'
		selecting: nil
! !

!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'button creation' stamp: 'FB 11/27/2021 17:12:10'!
addButtonsTo: row color: buttonColor

	self addButton: self createSelectAndSeeUsagesButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.! !

!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'button creation' stamp: 'FB 11/22/2021 16:16:53'!
createSelectAndSeeUsagesButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #selectAndSeeUsages
		label: 'Select and see usages'.

! !


!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'actions' stamp: 'FB 11/24/2021 00:27:26'!
selectAndSeeUsages

	self setImplementorInApplier.
	self delete.

	"Necesary indirection to support actual senders in LiveTyping - Hernan"
	applier openMessageSendsSelectionWindow.! !

!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'actions' stamp: 'FB 11/22/2021 14:33:31'!
selectImplementor

	selectedImplementor := model selection.
	self selectAndSeeUsages.! !

	!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 10/31/2021 21:38:33'!
	add

		self
			do: [ :classOfSenderToAdd | self askAndAddAllUsagesInSenderOf: classOfSenderToAdd ]
			withEnteredClassLabeled: 'Sender of #', self selectorToInline
			! !

!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 10/31/2021 23:14:50'!
addToList: aMessageNodeReference

	model addMessageNodeReference: aMessageNodeReference ifIncluded: [ self inform: 'Usage already in list' ]! !

!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 10/31/2021 23:15:46'!
askAndAddAllUsagesInSenderOf: classOfSenderToAdd

	"Adds all usages of the selected sender to the usages to refactor"
	| senderSelector senderToAdd |

	senderSelector := self request: 'Selector of sender of #', self selectorToInline initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].

	(senderToAdd sendsOrRefersTo: self selectorToInline) ifFalse: [ ^self inform: senderToAdd classAndSelector, ' does not refer to #', self selectorToInline].

	(applier refactoringClass findReferencesToSelector: self selectorToInline in: senderToAdd asMethodReference)
		do: [:aMessageNodeReference | self addToList: aMessageNodeReference]! !

!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 11/25/2021 00:24:47'!
changeUsages

	applier messageSends: model messageList
	! !

!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 10/31/2021 17:26:16'!
refactor

	self changeUsages.
	super refactor ! !

!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'FB 11/22/2021 16:24:46'!
remove

	model removeMessageFromBrowserKeepingLabel.! !


!InlineMethodUsagesStepWindow methodsFor: 'GUI building' stamp: 'FB 11/22/2021 16:24:33'!
addButtonsTo: row color: buttonColor

	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethodUsagesStepWindow class' category: #'Tools-Refactoring'!
InlineMethodUsagesStepWindow class
	instanceVariableNames: ''!

!InlineMethodUsagesStepWindow class methodsFor: 'instance creation' stamp: 'FB 11/24/2021 20:04:24'!
openFrom: anInlineMethodApplier

	| window messageSendsSet |

	messageSendsSet := InlineMethodMessageSendsSet applier: anInlineMethodApplier.

	messageSendsSet autoSelectString: 'asd'.

	window := self open: messageSendsSet label: 'Message sends of #', anInlineMethodApplier selectorToInline, ' to Refactor'.
	window initializeFrom: anInlineMethodApplier.

	^window.! !

!InlineMethodUsagesWithShowImplementorsStepWindow methodsFor: 'button creation' stamp: 'FB 11/13/2021 21:40:51'!
addButtonsTo: row color: buttonColor

	super addButtonsTo: row color: buttonColor.
	self addButton: self createSeeImplementorsButton to: row color: buttonColor.! !

!InlineMethodUsagesWithShowImplementorsStepWindow methodsFor: 'button creation' stamp: 'FB 11/13/2021 21:40:02'!
createSeeImplementorsButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'.
! !


!InlineMethodUsagesWithShowImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 11/22/2021 16:19:11'!
seeImplementors

	self changeUsages.
	self delete.

	applier openImplementorSelectionWindow.! !

!InlineTemporaryVariable commentStamp: '<historical>' prior: 0!
I am a refactoring that replaces references to a temporary variable for its actual value.
Implementation notes:

- If the refactoring is initiated from the declaration of the variable, then it must have at most one assignment.
- If there are no assignments, the only effect is the removal of the declaration.
- If the refactoring is triggered from a usage, the value used is the one from the previous last assignment and
the scope is limited to all usages up the next assignment, if any, or the end of the method.!


!InlineTemporaryVariable methodsFor: 'applying' stamp: 'FB 12/21/2021 20:24:52'!
apply

	self usageToInlineIsTemporaryDeclaration
		ifTrue: [self inlineFromTemporaryDeclaration]
		ifFalse: [self inlineFromNonDeclarationUsage].

	self compileChanges.

! !


!InlineTemporaryVariable methodsFor: 'initialization' stamp: 'FB 3/13/2022 17:44:17'!
initializeOldVariableNode: aVariableNodeToInline usage: aUsageToInline method: aCompiledMethodToRefactor methodNode: aMethodNode

	| parseNodesPath  |

	oldVariableNode := aVariableNodeToInline.
	usageToInline := aUsageToInline.
	variableToInline := aVariableNodeToInline name.
	methodToRefactor := aCompiledMethodToRefactor.
	updatedSourceCode := methodToRefactor sourceCode.
	methodNode := aMethodNode.
	parseNodesPath := methodNode parseNodesPathAt: usageToInline first ifAbsent: [self class signalSelectedIntervalIsNotATemporary].
	methodOrBlockNodeDeclaringTemporary := (parseNodesPath collect: [:anAssociation | anAssociation key])
		detect: [:aParseNode | aParseNode isBlockNode and: [aParseNode hasArgumentOrTemporaryNamed: variableToInline]]
		ifNone: methodNode.
	rangeOfNodeDeclaringTemporary := (methodOrBlockNodeDeclaringTemporary = methodNode)
		ifTrue: [1 to: (methodNode sourceText size)]
		ifFalse: [(methodNode completeSourceRangesOf: methodOrBlockNodeDeclaringTemporary ifAbsent: [self shouldNotHappen])
			detect: [:aSourceRange | aSourceRange includesAllOf: usageToInline]].
	sourceCodeChanges := SortedCollection sortBlock: [ :left :right | left key first < right key first ].

! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 6/11/2022 19:21:17'!
addParenthesesIfNeededTo: anExpression

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |

 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) = $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) = $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [anExpression]
		ifFalse: ['(', anExpression, ')'].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 12/21/2021 19:34:24'!
assertTemporaryHasAtMostOneAssignment

	self atMostOneAssignment ifFalse: [self class signalMoreThanOneAssignmentError].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
assertTemporaryNotUsedInBlockBetween: assignmentToInline and: anIndex

	| temporaryPositionsBetweenAssignmentAndIndex |

	temporaryPositionsBetweenAssignmentAndIndex := (methodNode consolidateAsCollection: (methodNode rangeForNode: oldVariableNode
		ifAbsent: [self shouldNotHappen ]))
			select: [:aPosition | aPosition first > assignmentToInline last and: [aPosition last < anIndex]].

	temporaryPositionsBetweenAssignmentAndIndex do: [:aPosition | ((methodNode parseNodesPathAt: aPosition first ifAbsent: [self shouldNotHappen])
		anySatisfy: [:aNodeAndRange | aNodeAndRange key isBlockNode
			and: [aNodeAndRange key ~= methodOrBlockNodeDeclaringTemporary ]
			and: [(aNodeAndRange value includesAllOf: rangeOfNodeDeclaringTemporary) not ]])
				ifTrue: [self class signalTemporaryUsedInBlockSurroundedWithAssignments]]! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 1/9/2022 16:27:33'!
assertUnassignedTemporaryIsNotReferenced

	((methodNode positionsForTemporaryVariable: variableToInline
		ifAbsent: [self shouldNotHappen]) size > 1) ifTrue: [self class signalAssignmentNotFound]






! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
assignmentNodeToInline: assignmentToInline

	methodNode nodesDo: [:aNode | ((self nodeIsAssignmentToTemporaryToInline: aNode)
		and: [
			|ranges|

			ranges := (methodNode rangeForNode: aNode ifAbsent: [#()]).
			(methodNode isMultipleRanges: ranges) ifFalse: [ranges := {ranges}].
			ranges anySatisfy: [:range | assignmentToInline includesAllOf: range].
		])
		ifTrue: [^aNode]].

	self shouldNotHappen.




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 1/8/2022 13:27:10'!
assignmentsToVariableCount

	| count |

	count := 0.
	methodOrBlockNodeDeclaringTemporary nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode) ifTrue: [count := count + 1 ].
	].

	^count.





! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 1/8/2022 13:27:42'!
atMostOneAssignment

	^self assignmentsToVariableCount <= 1.





! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
closestAssignmentBeforeUsage: assignments


	^(assignments
		select: [:assignmentRange | (assignmentRange includesAllOf: usageToInline)
			or: [assignmentRange last < usageToInline first]])
		detectMax: [:assignmentRange | assignmentRange last]




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 2/16/2021 17:03:49'!
compileChanges

	updatedSourceCode := updatedSourceCode copyReplacing: sourceCodeChanges.

	^methodToRefactor methodClass compile: updatedSourceCode classified: methodToRefactor category! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 4/3/2022 23:03:19'!
computeEndOfNodeEnclosingAssignment

	endOfNodeEnclosingAssignment := (methodNode parseNodesPathAt: assignmentToInlineRange first
		ifAbsent: [self shouldNotHappen])
			detect: [:nodeAndRange | nodeAndRange key isBlockNode]
			ifFound: [:nodeAndRangeOfAssignment | nodeAndRangeOfAssignment value last ]
			ifNone: [methodNode sourceText size].




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
findAssignmentOfUsage

	| lastAssignment assignments |

	assignments := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodOrBlockNodeDeclaringTemporary thatMatch:
		[:range | (range includesAllOf: usageToInline) or: [range last < usageToInline first] ].

	lastAssignment := assignments detectMax: [:assignment | assignment last].

	lastAssignment ~= nil ifTrue: [^lastAssignment ] ifFalse: [self class signalAssignmentNotFound]





! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:20:30'!
findLastIndexOfFirstAssignmentAfter: assignmentToInline

	| nextAssignmentsRanges |


	nextAssignmentsRanges := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodOrBlockNodeDeclaringTemporary
		thatMatch: [:range | range first > assignmentToInline last].

	^nextAssignmentsRanges collect: [:assignmentRange | assignmentRange last]
		andFold: [:rangeEnd1 :rangeEnd2 | rangeEnd1 min: rangeEnd2]
		ifEmpty: nil






! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:21:11'!
findOnlyAssignmentRangeInNode: aMethodOrBlockNode

	^(self selectCompleteRangesOfAssignmentsToTemporaryFrom: aMethodOrBlockNode
	 	thatMatch: [:aRange | true])
			ifEmpty: [^nil]
			ifNotEmpty: [:rangesSet | rangesSet anyOne].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:29:23'!
findReferencesBetweenAssignmentAndIndex: anIndex

	^((methodNode completeSourceRangesOf: oldVariableNode ifAbsent: [self shouldNotHappen])
		select: [:range | (range first >= assignmentToInlineRange last) and: [range first <= anIndex]
			and: [(self rangeIsLeftPartOfAssignmentToTemporary: range) not	]])! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 4/3/2022 23:27:25'!
inlineAssignment

	| endOfNextAssignmentIndex endOfRefactoringScopeIndex |

	endOfNextAssignmentIndex := self findLastIndexOfFirstAssignmentAfter: assignmentToInlineRange.
	self computeEndOfNodeEnclosingAssignment.
	endOfRefactoringScopeIndex := endOfNextAssignmentIndex ifNil: [endOfNodeEnclosingAssignment ].
	self inlineAssignment: assignmentToInlineRange upTo: endOfRefactoringScopeIndex.

	(self atMostOneAssignment and: [self noReferencesToTemporaryAfter: endOfRefactoringScopeIndex ])
		ifTrue: [self removeTemporaryVariableDeclaration].




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 4/3/2022 23:04:04'!
inlineAssignment: assignmentToInline upTo: anIndex

	| assignmentNode assignmentNodeValue expression expressionRange |

	(anIndex ~= endOfNodeEnclosingAssignment) ifTrue: [self assertTemporaryNotUsedInBlockBetween: assignmentToInline and: anIndex].
	assignmentNode := self assignmentNodeToInline: assignmentToInline.
	assignmentNodeValue := assignmentNode value.
	expressionRange := (methodNode completeSourceRangesOf: assignmentNodeValue ifAbsent: [self shouldNotHappen])
		detect: [:range | assignmentToInline includesAllOf: range].
	expression := methodNode sourceText copyFrom: expressionRange first to: expressionRange last.

	self inlineAssignmentNode: assignmentNode withExpression: expression upTo: anIndex;
		removeAssignment: assignmentToInline.! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 4/24/2021 18:20:38'!
inlineAssignmentNode: anAssignmentNode withExpression: anExpression upTo: anIndex

	| assignmentNodeValue |

	assignmentNodeValue := anAssignmentNode value.
	assignmentNodeValue isMessageNode
		ifTrue: [
			assignmentNodeValue isUnaryMessageSend
				ifTrue: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression
					upTo: anIndex].
			assignmentNodeValue isInfix
				ifTrue: [self inlineTemporaryNodeWithBinaryMessageExpression: anExpression
					upTo: anIndex].
			assignmentNodeValue isKeywordMessageSend
				ifTrue: [self inlineTemporaryNodeWithKeywordMessageExpression: anExpression
					upTo: anIndex].

		]
		ifFalse: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:18:06'!
inlineFromNonDeclarationUsage

	assignmentToInlineRange := self findAssignmentOfUsage.
	self inlineAssignment.
! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:17:11'!
inlineFromTemporaryDeclaration

	self assertTemporaryHasAtMostOneAssignment.

	assignmentToInlineRange := self findOnlyAssignmentRangeInNode: methodOrBlockNodeDeclaringTemporary.

	assignmentToInlineRange ~= nil
		ifTrue: [self inlineAssignment]
		ifFalse: [
			self assertUnassignedTemporaryIsNotReferenced.
			self removeTemporaryVariableDeclaration
		]






! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 2/16/2021 20:44:12'!
inlineTemp: aTempNode usedIn: aRange insideBinaryMessageNode: aBinaryMessageNode withBinaryExpression: aBinaryExpression

	(self temporaryNode: aTempNode referencedIn: aRange isArgumentIn: aBinaryMessageNode)
		ifTrue: [self replaceRange: aRange withParenthesisedExpression: aBinaryExpression]
		ifFalse: [self replaceRange: aRange withExpression: aBinaryExpression]


					! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:26:48'!
inlineTemporaryNodeWithBinaryMessageExpression: anExpression upTo: anIndex

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.
	referencesToInline do: [:range |
		| parentNode  |

		parentNode := (methodNode parseNodesPathAt: range first ifAbsent: [self shouldNotHappen]) second key.

		((parentNode isMessageNode) and: [parentNode isKeywordMessageSend not])
			ifTrue: [
				parentNode isUnaryMessageSend ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression ].
				parentNode isInfix ifTrue: [self inlineTemp: oldVariableNode usedIn: range insideBinaryMessageNode: parentNode
					withBinaryExpression: anExpression].
			]
			ifFalse: [self replaceRange: range withExpression: anExpression.]
	].	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:26:48'!
inlineTemporaryNodeWithKeywordMessageExpression: anExpression upTo: anIndex

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.
	referencesToInline do: [:range |
		| parentNode  |

		parentNode := (methodNode parseNodesPathAt: range first ifAbsent: [self shouldNotHappen]) second key.

		(parentNode isMessageNode)
			ifTrue: [
				parentNode isUnaryMessageSend ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression ].
				((parentNode isInfix) or: parentNode isKeywordMessageSend) ifTrue: [(self tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: range)
					ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression]
					ifFalse: [self replaceRange: range withExpression: anExpression ]]
			]
			ifFalse: [self replaceRange: range withExpression: anExpression.]
	].	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/19/2022 13:26:48'!
inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.

	referencesToInline do: [:range |
		self replaceRange: range withExpression: anExpression.
	].



	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 4/3/2022 23:26:14'!
noReferencesToTemporaryAfter: endOfRefactoringScopeIndex

	^(methodNode consolidateAsCollection: (methodNode rangeForNode: oldVariableNode
		ifAbsent: [self shouldNotHappen ]))
			noneSatisfy: [:aPosition | aPosition first > endOfRefactoringScopeIndex].




! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 11/28/2020 16:23:56'!
nodeIsAssignmentToTemporaryToInline: aNode

	^ (aNode isAssignmentToTemporary) and: [aNode variable name = variableToInline]! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
range: aRange isArgumentIn: aBinaryMessageNode

	^ (methodNode consolidateAsCollection: (methodNode rangeForNode: aBinaryMessageNode ifAbsent: 	[self shouldNotHappen]))
					anySatisfy: [:aBinaryMessageNodeRange | aBinaryMessageNodeRange includesAllOf: aRange]! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
rangeIsLeftPartOfAssignmentToTemporary: aRange

	methodNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)	 ifTrue: [
			| ranges completeRanges |

			ranges := methodNode consolidateAsCollection: (methodNode rangeForNode: aNode ifAbsent: [self shouldNotHappen ]).
			completeRanges := methodNode consolidateAsCollection:(methodNode completeSourceRangesOf: aNode ifAbsent: [self shouldNotHappen ]).
			((completeRanges anySatisfy: [:aCompleteRange | aCompleteRange includesAllOf: aRange])
				and: [ranges noneSatisfy: [:aNonCompleteRange | aNonCompleteRange includesAllOf: aRange ]]) ifTrue: [^true]
		]
	].

	^false

		! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/16/2022 18:41:08'!
rangeIsPartOfTemporaryDeclaration: aRange

	^(methodNode parseNodesPathAt: aRange first ifAbsent: [self shouldNotHappen])
		detect: [:aNodeAndRange | aNodeAndRange key isTemporariesDeclaration ]
		ifFound: [:foundNodeAndRange | ^true]
		ifNone: [^false].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:34:13'!
removeAssignment: assignmentToInline

	self replaceRange: assignmentToInline withExpression: ''.





! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 19:14:06'!
removeTemporaryVariableDeclaration

	| temporariesDeclarationNodes temporariesDeclarationNode intervalToRemove sourceText |

	temporariesDeclarationNode := methodOrBlockNodeDeclaringTemporary temporariesDeclaration.
	temporariesDeclarationNodes := temporariesDeclarationNode temporaryDeclarationNodes.
	sourceText := methodNode sourceText.
	temporariesDeclarationNodes size = 1
		ifTrue: [
			intervalToRemove := (methodNode rangeForNode: temporariesDeclarationNode
				ifAbsent: [self shouldNotHappen ])
					asSourceCodeInterval expandUntilStartOfNextStatementOn: sourceText.
		]
		ifFalse: [
			| temporaryDeclarationNode |
			temporaryDeclarationNode := temporariesDeclarationNodes detect: [:aTemporaryDeclarationNode |
				aTemporaryDeclarationNode declaresVariable: oldVariableNode ].
			intervalToRemove := (methodNode rangeForNode: temporaryDeclarationNode ifAbsent: [self shouldNotHappen ])
				asSourceCodeInterval expandUntilNextNonBlankCharacterOn: sourceText.
		].

	self replaceRange: intervalToRemove withExpression: ''
	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 12/28/2021 20:01:40'!
replaceRange: aRange withExpression: anExpression

	sourceCodeChanges add: (aRange -> anExpression).

	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 2/20/2021 19:55:15'!
replaceRange: aRange withParenthesisedExpression: anExpression

	self replaceRange: aRange withExpression: (self addParenthesesIfNeededTo: anExpression).

	! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:20:30'!
selectCompleteRangesOfAssignmentsToTemporaryFrom: aParseNode thatMatch: aConditionBlock

	| assignments |

	assignments := Set new.
	aParseNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)
			ifTrue: [
				| nodeRanges satisfyingRanges |
				nodeRanges := methodNode completeSourceRanges at: aNode ifAbsent: [].
				satisfyingRanges := (nodeRanges select: aConditionBlock)
					collect:[:range | range asSourceCodeInterval
						expandUntilStartOfNextStatementOn: methodNode sourceText].
				assignments addAll: (satisfyingRanges)]
	].

	^assignments






! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 3/13/2022 17:44:17'!
tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: aRange

	methodNode nodesDo: [ :aNode |
		(aNode isMessageNode
			and: [(methodNode completeSourceRangesOf: aNode ifAbsent: [self shouldNotHappen])
				anySatisfy: [:completeRange | (completeRange includesAllOf: aRange) and: [completeRange last > aRange last] ]]
		 	and: [(aNode receiver equivalentTo: oldVariableNode)
				or: [aNode receiver isMessageNode and: [aNode receiver arguments
						anySatisfy: [:argument | argument equivalentTo: oldVariableNode]]]]	)
			ifTrue: [^true]
	].
	^false.! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 2/14/2021 16:55:38'!
tempNodeToInline

	^methodNode tempNodes select: [:aNode | aNode name = variableToInline].! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 2/16/2021 20:14:05'!
temporaryNode: aTempNode referencedIn: aRange isArgumentIn: aBinaryMessageNode

	^ (aBinaryMessageNode arguments first equivalentTo: aTempNode) and: [self range: aRange isArgumentIn: aBinaryMessageNode]! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:07:51'!
usageToInlineIsTemporaryDeclaration

	^(methodNode parseNodesPathAt: usageToInline first ifAbsent: [self shouldNotHappen ])
		anySatisfy: [:aNodeAndRangeAssociation | aNodeAndRangeAssociation key isTemporaryDeclaration].





! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineTemporaryVariable class' category: #'Tools-Refactoring'!
InlineTemporaryVariable class
	instanceVariableNames: 'variableToInline blockNodeDeclaringTemporary'!

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 11/29/2020 01:42:06'!
assignmentNotFoundErrorMessage

	^'Variable is never assigned'
! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 11/28/2020 16:27:34'!
moreThanOneAssignmentErrorMessage

	^'There are multiple assignments that could be inlined. Please select a specific usage to inline'.
! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 12/28/2021 20:02:39'!
selectionIsNotATemporaryVariableErrorMessage

	^'Selected interval is not a temporary variable'.! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 12/29/2021 21:11:34'!
signalAssignmentNotFound

	^self refactoringError: (self assignmentNotFoundErrorMessage)





! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 12/21/2021 19:36:51'!
signalMoreThanOneAssignmentError

	self refactoringError: self moreThanOneAssignmentErrorMessage.





! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 12/28/2021 20:02:39'!
signalSelectedIntervalIsNotATemporary

	^self refactoringError: self selectionIsNotATemporaryVariableErrorMessage
! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 3/25/2021 20:15:05'!
signalTemporaryUsedInBlockSurroundedWithAssignments

	^self refactoringError: self temporaryUsedInBlockSurroundedWithAssignmentsErrorMessage
! !

!InlineTemporaryVariable class methodsFor: 'errors' stamp: 'FB 3/25/2021 20:15:05'!
temporaryUsedInBlockSurroundedWithAssignmentsErrorMessage

	^'Temporary is used in block and has multiple assignments'.! !


!InlineTemporaryVariable class methodsFor: 'instance creation - private' stamp: 'FB 4/11/2021 20:18:01'!
findTemporaryNamed: temporaryName atUsage: usageInterval inMethodNode: aMethodNode

	| oldVariableNodeAndUsageInterval parseNodesPath |

	parseNodesPath := aMethodNode parseNodesPathAt: usageInterval first ifAbsent: [self signalSelectedIntervalIsNotATemporary].
	oldVariableNodeAndUsageInterval := parseNodesPath
		detect: [:anAssociation |
			| parseNode |
			parseNode := anAssociation key.
			(parseNode isTemp and: [parseNode isNamed: temporaryName])]
		ifNone:[self signalSelectedIntervalIsNotATemporary].

	^{oldVariableNodeAndUsageInterval key. oldVariableNodeAndUsageInterval value}.
! !


!InlineTemporaryVariable class methodsFor: 'instance creation' stamp: 'FB 4/11/2021 20:18:52'!
named: tempVarToInlineName atUsageInterval: usageInterval inMethod: compiledMethodToRefactor

	| oldVariableNodeAndUsageInterval methodNode |
	methodNode := compiledMethodToRefactor methodNode.
	oldVariableNodeAndUsageInterval := self findTemporaryNamed: tempVarToInlineName atUsage: usageInterval
		inMethodNode: methodNode.

	^self new
		initializeOldVariableNode: oldVariableNodeAndUsageInterval first
		usage: oldVariableNodeAndUsageInterval second
		method: compiledMethodToRefactor methodNode: methodNode.! !

!InlineTemporaryVariableApplier methodsFor: 'refactoring - creation' stamp: 'FB 5/20/2021 20:54:28'!
createRefactoring

	^ self refactoringClass
		named: temporaryToInline
		atUsageInterval: usageInterval
		inMethod: methodToRefactor.! !

!InlineTemporaryVariableApplier methodsFor: 'refactoring - creation' stamp: 'FB 5/20/2021 20:28:41'!
refactoringClass

	^ InlineTemporaryVariable ! !


!InlineTemporaryVariableApplier methodsFor: 'refactoring - changes' stamp: 'FB 6/3/2021 19:22:49'!
requestRefactoringParameters! !

!InlineTemporaryVariableApplier methodsFor: 'refactoring - changes' stamp: 'FB 5/20/2021 20:55:04'!
showChanges

	codeProvider currentMethodRefactored! !


!InlineTemporaryVariableApplier methodsFor: 'initialization' stamp: 'FB 5/20/2021 20:52:16'!
initializeOn: aCodeProvider forTemporary: aTemporaryToInline at: aUsageInterval of: aMethodToRefactor

	codeProvider := aCodeProvider.
	temporaryToInline := aTemporaryToInline.
	usageInterval  := aUsageInterval.
	methodToRefactor := aMethodToRefactor.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineTemporaryVariableApplier class' category: #'Tools-Refactoring'!
InlineTemporaryVariableApplier class
	instanceVariableNames: ''!

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 12/21/2021 19:56:05'!
createAndValueHandlingExceptions: creationBlock


	[super createAndValueHandlingExceptions: creationBlock]
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].
! !

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/3/2021 19:20:00'!
on: anEditor for: usageInterval of: aMethodToRefactor

	| tempToInline |

	tempToInline := anEditor wordUnder: usageInterval first.

	^self new initializeOn: anEditor codeProvider forTemporary: tempToInline
		at: usageInterval of: aMethodToRefactor ! !

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 5/20/2021 20:27:02'!
refactoringClass

	^ InlineTemporaryVariable ! !

!InlineMethodMessageSendsSet methodsFor: 'initialization' stamp: 'HAW 6/11/2022 19:15:58'!
addMessageNodeReference: aMessageNodeReference ifIncluded: anIncludedBlock

	(messageList includes: aMessageNodeReference)
		ifTrue: [anIncludedBlock value]
		ifFalse: [messageList add: aMessageNodeReference]! !

!InlineMethodMessageSendsSet methodsFor: 'initialization' stamp: 'FB 10/31/2021 15:52:40'!
initialize

	selectedIndex := 0.
	super initialize ! !

!InlineMethodMessageSendsSet methodsFor: 'initialization' stamp: 'FB 10/31/2021 17:15:15'!
initializeApplier: anInlineMethodApplier

	applier := anInlineMethodApplier.! !


!InlineMethodMessageSendsSet methodsFor: 'message list' stamp: 'FB 10/31/2021 15:52:41'!
messageListIndex

	^selectedIndex ! !

!InlineMethodMessageSendsSet methodsFor: 'message list' stamp: 'FB 10/31/2021 15:52:41'!
messageListIndex: anIndex

	selectedIndex := anIndex.
	^super messageListIndex: anIndex ! !


!InlineMethodMessageSendsSet methodsFor: 'source code ranges' stamp: 'FB 11/24/2021 23:24:15'!
messageSendsRangesOf: aMessageNodeReference

	| messageSend |

	messageSend := applier messageSendAt: self messageListIndex ifAbsent: [ ^#()  ].
	messageSend completeSourceRange isEmpty
		ifTrue: [^#()]
		ifFalse: [^Array with: messageSend completeSourceRange]

	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'InlineMethodMessageSendsSet class' category: #'Tools-Refactoring'!
InlineMethodMessageSendsSet class
	instanceVariableNames: ''!

!InlineMethodMessageSendsSet class methodsFor: 'instance creation' stamp: 'FB 11/25/2021 00:25:16'!
applier: anInlineMethodApplier

	^(self messageList: anInlineMethodApplier messageSendsToInline) initializeApplier: anInlineMethodApplier! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 11/25/2021 00:12:18'!
displayStringOrText
	"To be used in the UI"

	^stringVersion ifNil: [String streamContents: [:aStream | self printOn: aStream ]].! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 3/19/2022 19:43:35'!
prefixStringVersionWith: aString

	"TODO: duplicated code from MethodReference"

	self removeStringVersionPrefix.
	stringVersion _ '[', aString, '] - ', (stringVersion ifNil: [self displayStringOrText]).! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 11/25/2021 00:13:41'!
printOn: aStream

	classReference printOn: aStream.
	aStream nextPutAll: '>>'.
	selector printOn: aStream.
	aStream newLine.
	messageNode printOn: aStream.
	aStream newLine.
	completeSourceRange printOn: aStream.! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 3/19/2022 19:41:12'!
removeStringVersionPrefix

	"TODO: duplicated code from MethodReference"

	stringVersion ifNotNil: [:string | | i prefixCoda |
		prefixCoda _ '] - '.
		i _ stringVersion findString: prefixCoda.
		i = 0 ifFalse: [stringVersion _ stringVersion copyFrom: i + prefixCoda size to: stringVersion size ].
	].

	! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 11/24/2021 23:26:59'!
stringVersion

	^stringVersion! !

!MessageNodeReference methodsFor: 'printing' stamp: 'FB 11/24/2021 23:26:47'!
stringVersion: aString

	stringVersion := aString.! !


!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/14/2021 18:04:33'!
actualClass

	^classReference! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/3/2021 19:51:41'!
compiledMethod

	^classReference >> selector ! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 10/31/2021 23:04:04'!
completeSourceRange

	^completeSourceRange! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 3/19/2022 19:13:52'!
isPossibleMessageSend

	"Implements polymorphism with PossbileMessageNodeReference"

	^false! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 10/31/2021 21:00:34'!
messageNode

	^messageNode ! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/14/2021 18:15:07'!
messageSendsRangesOf: aSelector

	"To use polimorphysm with MethodReference - Fernando"

	^completeSourceRange! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/3/2021 20:03:36'!
methodNode

	^methodNode! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/14/2021 17:19:18'!
methodReference

	^MethodReference class: classReference selector: 	selector.! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/14/2021 18:05:14'!
methodSymbol

	^selector! !

!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 11/14/2021 18:06:31'!
selectorOfMessageNode

	^messageNode selector key! !


!MessageNodeReference methodsFor: 'comparing' stamp: 'HAW 6/11/2022 19:24:06'!
asPossibleMessageNodeReference

	^PossibleMessageNodeReference
		messageNode: messageNode
		selector: selector
		class: classReference
		completeSourceRange: completeSourceRange.! !

!MessageNodeReference methodsFor: 'comparing' stamp: 'HAW 6/11/2022 19:24:24'!
equivalentTo: anObject

	^(anObject isKindOf: MessageNodeReference)
		and: [anObject actualClass = classReference]
		and: [anObject methodNode sourceText = methodNode sourceText ]
		and: [anObject messageNode equivalentTo: messageNode ]
		and: [anObject completeSourceRange = completeSourceRange		]
		! !


!MessageNodeReference methodsFor: 'testing' stamp: 'FB 11/25/2021 00:30:20'!
isMessageListTitle

	^completeSourceRange = #()
! !


!MessageNodeReference methodsFor: 'initialization' stamp: 'FB 3/19/2022 19:41:58'!
messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange

	messageNode := aMessageNode.
	selector := aSelector.
	classReference := aClass.
	completeSourceRange := aCompleteSourceRange.
	"When no source range is passed it means this is a dummy message node reference that works as a title in the message list window
	and the selector is not even defined in the class - Fernando"
	self isMessageListTitle ifFalse: [methodNode := (classReference >> selector) methodNode.].
	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MessageNodeReference class' category: #'Tools-Refactoring'!
MessageNodeReference class
	instanceVariableNames: ''!

!MessageNodeReference class methodsFor: 'initialization' stamp: 'FB 11/13/2021 17:39:03'!
messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange

	^self new messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.! !

!PossibleMessageNodeReference methodsFor: 'accesing' stamp: 'FB 3/19/2022 19:13:17'!
isPossibleMessageSend

	^true! !


!PossibleMessageNodeReference methodsFor: 'initialization' stamp: 'FB 3/19/2022 19:23:16'!
messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange

	super messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.
	self prefixStringVersionWith: self class possibleMessageSendStringVersionHeader.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'PossibleMessageNodeReference class' category: #'Tools-Refactoring'!
PossibleMessageNodeReference class
	instanceVariableNames: ''!

!PossibleMessageNodeReference class methodsFor: 'printing' stamp: 'FB 3/19/2022 19:10:06'!
possibleMessageSendStringVersionHeader

	"TODO: duplicated code from PossibleSender"

	^'Possible'! !


!PossibleMessageNodeReference class methodsFor: 'instance creation' stamp: 'FB 3/19/2022 19:15:35'!
messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange

	^self new messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.! !

!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isGetterOf:at: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile methodNode methodNode: parserClass selectorAndArgumentsAsString)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asMethodReference asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('source code ranges' messageSendsRangesOf:)
('ometa2preload' createMethodNode)
('user interface support' browse)
('copying' flattenTo:)
!


!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment isEmpty last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('enumerating' do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:count: species)
('testing' includes: includesAllOf: isInterval)
('arithmetic' + -)
('converting' asSourceCodeInterval)
!


!SourceCodeInterval reorganize!
('converting' asSourceCodeInterval)
('source code' codeAfterIntervalOn: expandToMatchExpressionOn: expandUntilNextNonBlankCharacterOn: expandUntilStartOfNextStatementOn: expandUntilStatementEndOn: lastCharacterOfRangeOn: nextNonBlankCharacterIs:on: nextNonBlankCharacterIsDotOn: trimToMatchExpressionOn:)
('private - source code' canBeExpandedStartingWith:endingWith: canBeTrimmed: expandBy: trimLeft:right:)
('testing' endsOnDotOn: intervalEndsOnEndOfStatementIn: isEndOfLastStatementOn: isEndOfTemporariesDeclarationOn: isLastStatementOfBlockOn:)
!


!SourceCodeInterval class reorganize!
('error messages' canNotExpandIncompleteStatementError)
!


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('comment')
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printSourceCode printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRange:basedOn: expandRange:basedOnChildRange: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBacktickNode isBlockNode isBraceNode isCascadeNode isComplex isConstantCharacter isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isMethodNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine referencesSelf referencesSuper toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('accessing' comment comment:)
!


!AssignmentNode reorganize!
('code generation' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value:from: variable:value:from:sourceRange:)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: isAssignmentNode isAssignmentToTemporary isComplex referencesSelf referencesSuper)
('visiting' accept:)
('initialize-release' variable:value:)
!


!BlockNode reorganize!
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('code generation' code)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('enumerating' statementsDo:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine referencesSelf referencesSuper returns)
('visiting' accept:)
('private' hasEquivalentStatementsWith:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body numberOfStatements primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries: temporariesDeclaration temporariesDeclaration:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector ifPrimitivePrintOn: methodClass printCommentOn: printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printSelectorAndArgumentsOn: printTemporariesOn: printWithClosureAnalysisOn: selectorAndArgumentsAsString sourceText tempNames)
('source mapping' addPositionTo:of:inside: completeSourceRanges completeSourceRangesDo: completeSourceRangesOf:ifAbsent: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: parseNodesPathAt:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition singleCompleteSourceRangeOf: singleCompleteSourceRangeOf:ifPresent:ifAbsent: withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: selector:arguments:precedence:temporariesDeclaration:block:encoder:primitive:properties: selector:arguments:precedence:temporariesDeclaration:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' allParseNodesWithin:satisfy: anyParseNodeWithin:satisfy: equivalentTo: hasArgumentOrTemporaryNamed: hasLocalNamed: isMethodNode isMultipleRanges: referencesSelf referencesSuper)
('source ranges' definitionStartPosition)
!


!VariableNode reorganize!
('visiting' accept:)
('testing' assignmentCheck:at: equivalentTo: index isFalsePseudoVariable isNamed: isNilPseudoVariable isSelfPseudoVariable isSuperPseudoVariable isThisContextPseudoVariable isTruePseudoVariable isUndeclared isVariableNode isVariableReference referencesSelf referencesSuper varNodeType)
('code generation (closures)' beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools:)
('tiles' currentValueIn: variableGetterBlockIn:)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('code generation' emitCodeForLoad:forValue:encoder: fieldOffset sizeCodeForReturn:)
('accessing' name)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' asStorableNode: name: name:index:type: name:key:code: name:key:index:type: nameAndKey:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval originalArguments originalReceiver receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isKeywordMessageSend isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend referencesSelf referencesSuper toDoIncrement: toDoLimit:)
('cascading' receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: compare:with: hasEquivalentArgumentsWith: hasEquivalentReceiverWith: ifNilReceiver pvtCheckForPvtSelector: transform:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
('initialize-release' receiver:selector:arguments:precedence:from:sourceRange:)
('expression types' cascadeReceiver)
!


!ReturnNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('converting' asReturnNode)
('code generation' code)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForValue:encoder: sizeCodeForReturn: sizeCodeForValue:)
('printing' expr printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' equivalentTo: isImplicitSelfReturnIn: isReturn isReturnSelf isSpecialConstant isVariableReference referencesSelf referencesSuper)
('initialization')
('initialize-release' expr: expr:encoder:sourceRange:)
!


!SmalltalkEditor reorganize!
('accessing-selection')
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu: openSmalltalkEditorRefactoringMenu refactoringMenu)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' actualImplementorsOfIt actualImplementorsOfIt: actualLocalImplementorsOfIt actualLocalImplementorsOfIt: actualSendersOfIt actualSendersOfIt: balloonTypeInfoAt: balloonTypeInfoInMethodAt: balloonTypeInfoOf:in:definedAt: balloonTypeInfoOfInstanceVariableAt: showSelectionTypeInfo showSelectionTypeInfo: showSelectionTypeInfoInMethod showTypeInfoOf:in:definedAt: showTypeInfoOfInstanceVariable typeCheckMethod typeCheckMethod: contextualInlineMethod contextualInlineMethod: contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable inlineTemporaryVariable:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' messageListKey:from: systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter changeKeywordOrder classRefactoringMenu inlineMethod messageRefactoringMenu moveToInstanceOrClassMethod openClassRefactoringMenu openMessageRefactoringMenu pushDownInstanceVariable pushDownSelector pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands' findClass)
!


!MethodReference reorganize!
('queries' actualClass actualClassIfAbsent: category classIsMeta classSymbol compiledMethod compiledMethodIfAbsent: isValid methodSymbol selector sourceCode sourceCodeIfAbsent:)
('setting' indentLevel: prefixStringVersionWith: removeStringVersionPrefix setClass:methodSymbol:stringVersion: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
('printing' displayStringOrText printClassAndSelectorOn: printOn:)
('auto complete' dynamicTypingAutoCompleterDocumentation methodClass)
('source code ranges' messageSendsRangesOf:)
('testing' hasVariableBindingTo: isMessageListTitle referencesParameterAt:)
('decompiling' methodNode)
!


!RefactoringApplier class reorganize!
('value handling exceptions' createAndValueHandlingExceptions:)
('initialization' initialize)
('appliers - registering' registerAddParameterApplier: registerExtractAsParameterApplier: registerInlineMethodApplier: registerRemoveParameterApplier: registerRenameSelectorApplier:)
('appliers - getting' addParameterApplier extractAsParameterApplier inlineMethodApplier removeParameterApplier renameSelectorApplier)
('appliers - id' addParameterApplierId extractAsParameterApplierId inlineMethodApplierId removeParameterApplierId renameSelectorApplierId)
('appliers - resetting' resetAddParameterApplier resetExtractAsParameterApplier resetInlineMethodApplier resetRemoveParameterApplier resetRenameSelectorApplier)
('appliers - private' applierAt:ifAbsent: registerApplierAt:with: resetApplierAt:)
!


!RefactoringMenues class reorganize!
('editor menus' messageRefactoringMenuOptions smalltalkEditorMenuOptions smalltalkEditorRefactoringMenuOptions)
('browser menues' classListMenuOptions classRefactoringMenuOptions messageListMenuOptions)
('initialization' initialize)
('shortcuts' smalltalkEditorCmdShortcutsSpec)
!
