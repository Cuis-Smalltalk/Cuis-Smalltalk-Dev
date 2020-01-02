'From Cuis 5.0 [latest update: #3997] on 2 January 2020 at 7:05:52 am'!
!classDefinition: #ChangeSelectorApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ChangeSelectorApplier
	instanceVariableNames: 'oldSelector scopeChoice implementors senders implementingClass wizardStepWindow shouldShowChanges browser selectedClass '
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
	instanceVariableNames: 'newParameter newParameterValue newKeyword originalMethod parameterToRemove parameterToRemoveIndex parameterToRemoveName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MethodReference methodsFor: 'testing' stamp: 'HAW 1/2/2020 06:45:07'!
referencesParameterAt: parameterIndex

	^(self compiledMethodIfAbsent: [ ^false ]) referencesParameterAt: parameterIndex ! !


!RemoveParameter class methodsFor: 'instance creation' stamp: 'HAW 1/1/2020 23:34:04'!
atIndex: parameterIndex named: aParameterToRemove from: oldSelector implementors: implementorsCollection senders: sendersCollection

	| newSelector |

	self assertCanRemoveParameterFrom: oldSelector.
	self assert: parameterIndex isValidFor: oldSelector.

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

!RemoveParameter class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 23:39:59'!
assert: parameterIndex isValidFor: oldSelector

	parameterIndex isInteger ifFalse: [ self signalInvalidParameterIndex ].
	(parameterIndex between: 1 and: oldSelector numArgs) ifFalse: [ self signalInvalidParameterIndex ].! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 1/1/2020 23:35:34'!
invalidParameterIndexErrorMessage

	^'Invalid parameter index'! !

!RemoveParameter class methodsFor: 'exceptions' stamp: 'HAW 1/1/2020 23:35:19'!
signalInvalidParameterIndex

	self refactoringError: self invalidParameterIndexErrorMessage! !


!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 1/2/2020 00:05:51'!
on: aMessageNode createAndValueHandlingExceptionsOn: aModel in: aSelectedClass at: aSelectedSelector

	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass
! !


!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:52:39'!
addAsLastParameterLabel

	^ 'Add as last parameter'! !

!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:52:39'!
askInsertionIndexUsingKeywords

	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword to add parameter before'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	! !


!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:19:25'!
askKeywordToRemove

	| keywords |

	keywords := oldSelector keywords.

	keywords size = 1
		ifTrue: [ parameterToRemoveIndex := 1 ]
		ifFalse: [ parameterToRemoveIndex := self selectKeywordIndexToRemoveFrom: keywords ].

	"Because I do not know the parameter name, I'll use this one as explanation - Hernan"
	parameterToRemoveName := 'Parameter related to keyword ', (keywords at: parameterToRemoveIndex) ! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:18:18'!
selectKeywordIndexToRemoveFrom: keywords

	| keywordIndex |

	keywordIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword related to parameter to remove'.
	keywordIndex = 0 ifTrue: [self endRequest ].

	^keywordIndex! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:21:19'!
selectParameterIndexToRemoveFrom: parameterNames

	| parameterIndex |

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Select parameter to remove'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	^parameterIndex! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 1/2/2020 00:04:31'!
renameSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector

	RefactoringApplier renameSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 1/2/2020 00:04:31'!
changeSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

	aChangeSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector! !


!ChangeSelectorWizardStepWindow methodsFor: 'compile methods' stamp: 'HAW 1/1/2020 19:45:32'!
compiledMethodsFrom: methodReferences

	"If the method is not implemented, I leave the not implemented reference because actual senders of it
	should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan"
	^ methodReferences collect: [:aMethodReference |
		aMethodReference compiledMethodIfAbsent: [ aMethodReference ]]! !


!MethodReference methodsFor: 'queries' stamp: 'HAW 1/1/2020 19:46:02'!
compiledMethodIfAbsent: ifAbsentBlock

	^ self actualClass
		ifNil: ifAbsentBlock
		ifNotNil: [ :aClass | aClass compiledMethodAt: methodSymbol ifAbsent: ifAbsentBlock ] ! !

!MethodReference methodsFor: 'testing' stamp: 'HAW 1/2/2020 06:45:39'!
hasVariableBindingTo: aClass

	^(self compiledMethodIfAbsent: [ ^false ]) hasVariableBindingTo: aClass
! !


!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 1/1/2020 19:44:25'!
createNewImplementors

	implementors
		select: [ :anImplementor | anImplementor isValid ]
	 	thenDo: [:anImplementor | self createNewImplementorOf: anImplementor ]
	! !


!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 1/2/2020 07:00:51'!
assertNewParameter: aNewParameter isNotDefinedAsInstanceVariableInAny: implementors

	| classesDefiningInsVars |

	classesDefiningInsVars := self classesDefiningInstanceVariable: aNewParameter inAny: implementors.
	classesDefiningInsVars ifNotEmpty: [
		self signalNewParameter: aNewParameter definedAsInstanceVariableIn: classesDefiningInsVars ]! !

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 1/2/2020 07:00:58'!
assertNewParameter: aNewParameter isNotDefinedAsLocalInAny: implementors

	| implementorsDefiningNewParameterAsLocal |

	implementorsDefiningNewParameterAsLocal := implementors select: [ :implementor |
		implementor isValid and: [ implementor methodNode hasLocalNamed: aNewParameter ]].
	implementorsDefiningNewParameterAsLocal ifNotEmpty: [
		self signalNewParameter: aNewParameter isDefinedAsLocalIn: implementorsDefiningNewParameterAsLocal ]! !


!RemoveParameter class methodsFor: 'instance creation' stamp: 'HAW 1/2/2020 00:23:12'!
named: aParameterToRemove from: aMethod implementors: implementorsCollection senders: sendersCollection

	| parameterIndex |

	parameterIndex := self assert: aParameterToRemove isDefinedIn: aMethod methodNode.

	^self
		atIndex: parameterIndex
		named: aParameterToRemove
		from: aMethod selector
		implementors: implementorsCollection
		senders: sendersCollection ! !


!RefactoringApplier class methodsFor: 'value handling exceptions' stamp: 'HAW 12/30/2019 16:46:47'!
createAndValueHandlingExceptions: creationBlock

	| applier |

	applier := creationBlock
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

	applier value ! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 21:52:03'!
implementorsAndSendersForClass

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		forClassAndMetaOf: selectedClass! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 21:52:03'!
implementorsAndSendersForHierarchy

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: selectedClass! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 21:52:03'!
implementorsAndSendersInCategory

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inCategory: selectedClass category
		organizedBy: SystemOrganization! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 21:52:02'!
implementorsAndSendersInCategoryAndHierarchy

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inCategoriesAndHierarchyOf: selectedClass
		organizedBy: SystemOrganization ! !

!ChangeSelectorApplier methodsFor: 'initialization' stamp: 'HAW 1/1/2020 21:54:19'!
initializeOn: aBrowser for: aSelector in: aSelectedClass

	oldSelector := aSelector.
	selectedClass := aSelectedClass.
	browser := aBrowser.
	shouldShowChanges := true.! !


!ChangeSelectorApplier class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 21:55:17'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass

	self subclassResponsibility ! !

!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 1/1/2020 21:54:30'!
on: aBrowser for: aSelector in: aSelectedClass

	self assertCanApplyRefactoringFor: aSelector in: aSelectedClass.

	^self new initializeOn: aBrowser for: aSelector in: aSelectedClass
	! !


!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/2/2020 07:02:14'!
askInsertionIndex

	| methodNode originalMethod parameterNames |

	"See RemoveParameterApplier#askParameterToRemove to understand why I ask for the index using
	the keywords when no method is found - Hernan"
	originalMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askInsertionIndexUsingKeywords ].

	methodNode := originalMethod methodNode.
	parameterNames := methodNode argumentNames.
	parameterNames add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Add parameter before?'.
	parameterIndex = 0 ifTrue: [self endRequest ].

		! !


!AddParameterApplier class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 21:55:05'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass

	AddParameter assertCanAddParameterTo: aSelector.


	! !


!ChangeKeywordsSelectorOrderApplier class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 21:55:12'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass

	ChangeKeywordsSelectorOrder assertIsKeywordWithMoreThanOneParameter: aSelector ! !


!RenameSelectorApplier class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 21:55:26'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass

	! !


!RemoveParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 1/1/2020 23:25:59'!
createRefactoring

	^self refactoringClass
		atIndex: parameterToRemoveIndex
		named: parameterToRemoveName
		from: oldSelector
		implementors: implementors
		senders: senders ! !

!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/1/2020 23:28:37'!
askParameterToRemove

	| methodNode parameterNames selectedMethod |

	"If the compiled method does not exist it means that the remove is being executed from the
	editor, in a message send therefore we can not ask for the parameter name unless we look for implementors or
	use LiveTyping to look for actual implementors, etc.
	To make it simpler, when we can know the parameter names, we use that. When we can not, we use the keyword
	names. I tried to used only keyword names but it is not so intuitive. I decided to use two different ways of asking
	instead of one (asking for keyword names) becuase I think the programmer prefers to see parameter names.

	It could happen that the selected class implements the message to remove the parameter but that the remove
	is executed from the editor (not sending to self), in that case the parameters of selected class implementation
	will be use... it is a rare case and I think it will not confuse the programmer  - Hernan"

	selectedMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askKeywordToRemove ].

	methodNode := selectedMethod methodNode.
	parameterNames := methodNode argumentNames.

	parameterToRemoveIndex := parameterNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ self selectParameterIndexToRemoveFrom: parameterNames ].

	parameterToRemoveName := parameterNames at: parameterToRemoveIndex.


	! !


!RemoveParameterApplier class methodsFor: 'pre-conditions' stamp: 'HAW 1/1/2020 21:55:21'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass

	RemoveParameter assertCanRemoveParameterFrom: aSelector.


	! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 1/2/2020 07:21:15'!
isValid

	"To be polimorphic with MethodReference, important for refactorings - Hernan"
	^true! !

!methodRemoval: RemoveParameterApplier class #createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:02:46'!
RemoveParameterApplier class removeSelector: #createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: RemoveParameterApplier class #on:createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:04:31'!
RemoveParameterApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: RemoveParameterApplier class #on:createAndValueHandlingExceptionsOn:in:at: stamp: 'HAW 1/2/2020 00:08:23'!
RemoveParameterApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:in:at:!
!methodRemoval: RemoveParameterApplier #selectKeywordToRemoveFrom: stamp: 'HAW 1/1/2020 23:17:59'!
RemoveParameterApplier removeSelector: #selectKeywordToRemoveFrom:!
!methodRemoval: RemoveParameterApplier #selectParameterToRemoveForm: stamp: 'HAW 1/1/2020 23:13:22'!
RemoveParameterApplier removeSelector: #selectParameterToRemoveForm:!
!methodRemoval: RemoveParameterApplier #selectParameterToRemoveFrom: stamp: 'HAW 1/1/2020 23:21:19'!
RemoveParameterApplier removeSelector: #selectParameterToRemoveFrom:!
!methodRemoval: RenameSelectorApplier class #createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:02:46'!
RenameSelectorApplier class removeSelector: #createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: RenameSelectorApplier class #on:createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:04:31'!
RenameSelectorApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: RenameSelectorApplier class #on:createAndValueHandlingExceptionsOn:in:at: stamp: 'HAW 1/2/2020 00:07:45'!
RenameSelectorApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:in:at:!
!methodRemoval: AddParameterApplier class #createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:02:46'!
AddParameterApplier class removeSelector: #createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: AddParameterApplier class #on:createAndValueHandlingExceptionsOn:of:in:at: stamp: 'HAW 1/2/2020 00:04:31'!
AddParameterApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:of:in:at:!
!methodRemoval: AddParameterApplier class #on:createAndValueHandlingExceptionsOn:in:at: stamp: 'HAW 1/2/2020 00:05:51'!
AddParameterApplier class removeSelector: #on:createAndValueHandlingExceptionsOn:in:at:!

!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
('extract method' extractMethod extractMethod:)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
!

!classDefinition: #ChangeSelectorApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ChangeSelectorApplier
	instanceVariableNames: 'oldSelector scopeChoice implementors senders selectedClass wizardStepWindow shouldShowChanges browser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ChangeSelectorApplier reorganize!
('refactoring - parameters request' askForImplementosAndSenders askScope calculateImplementorsAndSenders createImplementors createSenders implementorsAndSendersForClass implementorsAndSendersForHierarchy implementorsAndSendersInCategory implementorsAndSendersInCategoryAndHierarchy implementorsAndSendersInSystem initializeImplementorsAndSenders openChangeSelectorSendersStepWindow scopeOptionLabels startWizard)
('accessing' implementors implementors: oldSelector senders senders: wizardStepWindow:)
('refactoring - changes' closeBrowser doNotShowChanges informChangesToBrowser messageSetWindowClass showChanges showChangesInMessageSetWindow)
('refactoring - creation' createAndApplyRefactoring createAndApplyRefactoringWhenNoSendersAndOneImplementor: refactoringClass)
('initialization' initializeOn:for:in:)
('evaluation' ifHasNoSendersAndOneImplementor:ifNot: value wizardEnded)
!


!AddParameterApplier reorganize!
('refactoring - creation' createRefactoring createRefactoringForKeywordSelector createRefactoringForUnarySelector refactoringClass)
('refactoring - parameters request' addAsLastParameterLabel askInsertionIndex askInsertionIndexIfNecessary askInsertionIndexUsingKeywords askNewKeyword askNewKeywordIfNecessary askNewParameter askNewParameterValue requestRefactoringParameters)
!


!AddParameterApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!

!classDefinition: #RemoveParameterApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #RemoveParameterApplier
	instanceVariableNames: 'parameterToRemoveIndex parameterToRemoveName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RemoveParameterApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' askKeywordToRemove askParameterToRemove requestRefactoringParameters selectKeywordIndexToRemoveFrom: selectParameterIndexToRemoveFrom:)
!

!CompiledMethod reorganize!
('accessing' bytecodeSetName clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isTestMethod isValid readsInstanceVariable: referencesParameterAt: sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreambleFrom:at: getSource getSourceFor:in: getSourceFromFile linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceClass sourceCode sourceFileStream sourcePointer)
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
('converting' asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('source code ranges' messageSendsRangesOf:)
('ometa2preload' createMethodNode)
('user interface support' browse)
!
