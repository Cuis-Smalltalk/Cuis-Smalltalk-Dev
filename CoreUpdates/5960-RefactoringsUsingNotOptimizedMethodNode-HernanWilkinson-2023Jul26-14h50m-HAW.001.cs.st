'From Cuis 6.0 [latest update: #5950] on 28 July 2023 at 2:52:10 pm'!
!classDefinition: #RenameObject category: #'Tools-Refactoring'!
Refactoring subclass: #RenameObject
	instanceVariableNames: 'system newName oldName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameClass category: #'Tools-Refactoring'!
RenameObject subclass: #RenameClass
	instanceVariableNames: 'classToRename undeclared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameGlobal category: #'Tools-Refactoring'!
RenameObject subclass: #RenameGlobal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Behavior methodsFor: 'private' stamp: 'HAW 7/26/2023 18:43:26'!
basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode := self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock
				optimizeSpecialSends: doOptimizeSpecialSends.
	methodNode encoder requestor: requestor.
	^ CompiledMethodWithNode generateMethodFromNode: methodNode trailer: bytes.! !


!CompiledMethod methodsFor: 'method node' stamp: 'HAW 7/26/2023 17:22:49'!
notOptimizedMethodNode

	"Returns the method node without any optimization applied.
	 Heavely used in refactorings - Hernan"
	^self createMethodNodeOptimizeSpecialSends: false! !


!Compiler methodsFor: 'public access' stamp: 'HAW 7/26/2023 18:40:45'!
compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends
	"Answer a MethodNode for the argument, textOrStream. If the
	MethodNode can not be created, notify the argument, aRequestor; if
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root
	of a parse tree. It can be told to generate a CompiledMethod to be
	installed in the method dictionary of the argument, aClass."

	| methodNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	category := aCategory.
	methodNode := self translate: sourceStream noPattern: false ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends.
	methodNode encoder requestor: requestor.
	^methodNode! !

!Compiler methodsFor: 'public access' stamp: 'HAW 7/26/2023 18:42:00'!
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends
	^self compile: textOrStream in: aClass classified: nil notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends ! !

!Compiler methodsFor: 'private' stamp: 'HAW 7/26/2023 18:36:27'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends

	^self parser
		optimizeSpecialSends: doOptimizeSpecialSends;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !

!Compiler methodsFor: 'private' stamp: 'HAW 7/26/2023 18:38:32'!
translate: aStream noPattern: noPattern ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends

	^self translate: aStream noPattern: noPattern doIt: noPattern ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends! !


!MethodReference methodsFor: 'method node' stamp: 'HAW 7/26/2023 17:19:53'!
notOptimizedMethodNode

	^self compiledMethod notOptimizedMethodNode! !


!InlineMethod class methodsFor: 'preconditions' stamp: 'HAW 7/27/2023 17:35:36'!
assertCanInlineWithReferencesToSelf: aCompiledMethodToInline from: aCollectionOfMessageSends

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
				self signalMethodToInlineReferencesSelf]].
! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'HAW 7/27/2023 17:36:33'!
assertCanInlineWithReferencesToSuper: aCompiledMethodToInline from: aCollectionOfMessageSends


	"Do not allow inlining implementors that reference super unless all the senders belong to the same class,
	since otherwise it's not possible to replace the super keyword with something that maintains the semantics"
	(aCompiledMethodToInline referencesSuper) ifTrue: [
		self assertSenders: aCollectionOfMessageSends belongToClassOf: aCompiledMethodToInline
			ifFalse: [self signalMethodToInlineReferencesSuper]].

! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'HAW 7/27/2023 17:32:20'!
assertSenders: aSendersCollection haveAccessToPrivateVariablesOf: aCompiledMethod methodNode: aMethodNode

	| methodReferencesInstanceVariables |

	methodReferencesInstanceVariables := false.
	aCompiledMethod notOptimizedMethodNode
		nodesDo: [:aNode | aNode isInstanceVariableNode ifTrue: [methodReferencesInstanceVariables := true]].

	methodReferencesInstanceVariables ifTrue: [
		(aSendersCollection allSatisfy: [:aSenderClass | aSenderClass = aCompiledMethod methodClass])
			ifFalse: [self signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage]
	].
	 ! !


!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:16:04'!
newSourceCodeOf: aCompiledMethod

	| newSource |

	newSource := aCompiledMethod sourceCode copyReplacing: (self rangesToReplaceOf: aCompiledMethod) with: newName.

	^newSource! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:16:43'!
rangesForLiteralOf: methodNode

	| literalRanges |

	literalRanges := methodNode positionsForLiteralNode: oldName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].

	^literalRanges ! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:17:00'!
rangesForLiteralVariableOf: methodNode

	^methodNode positionsForLiteralVariableNode: oldName ifAbsent: [ #() ]
	! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:17:44'!
rangesToReplaceOf: aCompiledMethod

	| methodNode ranges |

	methodNode := aCompiledMethod notOptimizedMethodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].

	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).

	^ranges ! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:22:24'!
references: aMethodReference classVarNamed: aName

	^(aMethodReference actualClass theNonMetaClass definesClassVariableNamedInHierarchy: aName)
		or: [ aMethodReference actualClass theMetaClass definesClassVariableNamedInHierarchy: aName ]! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:24:57'!
referencesOldName: aMethodReference

	^self references: aMethodReference classVarNamed: oldName! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:36:44'!
referencesToRename

	self subclassResponsibility! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:17:28'!
renameReference: aReferencingMethod

	| newSource |

	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource ! !

!RenameObject methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:35:41'!
renameReferences

	| references |

	references := self referencesToRename.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].

	^references! !

!RenameObject methodsFor: 'accessing' stamp: 'HAW 7/27/2023 18:33:33'!
newName

	^newName ! !

!RenameObject methodsFor: 'accessing' stamp: 'HAW 7/27/2023 18:26:06'!
referencesToOldName

	^system allCallsOn: oldName! !


!RenameClass methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:34:43'!
referencesToRename

	^ (self referencesToOldClass asSet, self referencesToOldName asSet) asOrderedCollection! !


!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:35:32'!
referencesToRename

	^ self referencesToOldName! !


!Behavior methodsFor: 'private' stamp: 'HAW 7/26/2023 18:44:16'!
basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"

	^self basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock optimizeSpecialSends: true! !


!Compiler methodsFor: 'public access' stamp: 'HAW 7/26/2023 18:42:30'!
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock
	^self compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock optimizeSpecialSends: true ! !

!Compiler methodsFor: 'private' stamp: 'HAW 7/26/2023 18:36:58'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock optimizeSpecialSends: true! !

!Compiler methodsFor: 'private' stamp: 'HAW 7/26/2023 18:38:49'!
translate: aStream noPattern: noPattern ifFail: failBlock

	^self translate: aStream noPattern: noPattern ifFail: failBlock optimizeSpecialSends: true! !


!Parser methodsFor: 'expression types' stamp: 'HAW 7/26/2023 18:02:25'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |

	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: [
			start := self startOfNextToken.
			selector := source collectionSpecies writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue:
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [

			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue:
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue:
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
				optimizeSpecialSends: optimizeSpecialSends;
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true! !


!MessageNode methodsFor: 'private' stamp: 'HAW 7/28/2023 12:35:40'!
transform: encoder
	optimizeSpecialSends == false ifTrue: [ special := 0 ].
	special = 0 ifTrue: [^false].

	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue:
			[^true]
		ifFalse:
			[special := 0. ^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'HAW 7/26/2023 18:22:09'!
noteSpecialSelector: selectorSymbol
	"special > 0 denotes specially treated (potentially inlined) messages. "

	special := optimizeSpecialSends == false
		ifTrue: [ 0 ]
		ifFalse: [ MacroSelectors indexOf: selectorSymbol ]
! !


!MessageNodeReference methodsFor: 'initialization' stamp: 'HAW 7/27/2023 16:58:53'!
messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange

	messageNode := aMessageNode.
	selector := aSelector.
	classReference := aClass.
	completeSourceRange := aCompleteSourceRange.
	"When no source range is passed it means this is a dummy message node reference that works as a title in the message list window
	and the selector is not even defined in the class - Fernando"
	self isMessageListTitle ifFalse: [methodNode := (classReference >> selector) notOptimizedMethodNode ].
	! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 7/27/2023 16:58:59'!
rangesToKeywordsOf: aMethod

	| methodNode rangesToKeywords |

	methodNode := aMethod notOptimizedMethodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right | left key first < right key first ].

	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.

	^rangesToKeywords	! !


!AddParameter methodsFor: 'create new implementors - private' stamp: 'HAW 7/27/2023 16:59:29'!
implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource insertionPoint |

	implementorMethodNode := anImplementor notOptimizedMethodNode.
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


!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/27/2023 16:59:22'!
assertNewParameter: aNewParameter isNotDefinedAsLocalInAny: implementors

	| implementorsDefiningNewParameterAsLocal |

	implementorsDefiningNewParameterAsLocal := implementors select: [ :implementor |
		implementor isValid and: [ implementor notOptimizedMethodNode hasLocalNamed: aNewParameter ]].
	implementorsDefiningNewParameterAsLocal ifNotEmpty: [
		self signalNewParameter: aNewParameter isDefinedAsLocalIn: implementorsDefiningNewParameterAsLocal ]! !


!ChangeSelectorKeepingParameters methodsFor: 'create new implementors - private' stamp: 'HAW 7/27/2023 16:59:35'!
implementorNewSourceCodeOf: anImplementor

	|  newSource rangesToNewKeywords |

	rangesToNewKeywords := OrderedCollection new.
	currentImplementorMethodNode := anImplementor notOptimizedMethodNode.

	currentImplementorMethodNode selectorKeywordsPositions withIndexDo: [ :aKeywordRange :index |
		self addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords ].

	newSource := anImplementor sourceCode copyReplacing: rangesToNewKeywords.
	^newSource! !


!RemoveParameter methodsFor: 'create new implementors - private' stamp: 'HAW 7/27/2023 16:59:40'!
implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource parameterToRemovePosition selectorToRemovePosition |

	implementorMethodNode := anImplementor notOptimizedMethodNode.
	selectorToRemovePosition := implementorMethodNode selectorKeywordPositionAt: parameterIndex.
	parameterToRemovePosition := implementorMethodNode parameterDefinitionPositionAt: parameterIndex.

	originalSource := anImplementor sourceCode.
	newSource := String streamContents: [ :newSourceStream |
		self writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition.
		self writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition ].

	^newSource! !


!RemoveParameter class methodsFor: 'instance creation' stamp: 'HAW 7/27/2023 16:59:47'!
named: aParameterToRemove from: aMethod implementors: implementorsCollection senders: sendersCollection

	| parameterIndex |

	parameterIndex := self assert: aParameterToRemove isDefinedIn: aMethod notOptimizedMethodNode.

	^self
		atIndex: parameterIndex
		named: aParameterToRemove
		from: aMethod selector
		implementors: implementorsCollection
		senders: sendersCollection ! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/27/2023 17:22:01'!
assertCanExtractedFromAll: allIntervals for: aSourceMethod

	| sourceCode methodNode lastIntervalsAndNode |

	allIntervals isEmpty ifTrue: [ self signalNoExpressionToExtract ].

	sourceCode := aSourceMethod sourceCode.
	methodNode := aSourceMethod notOptimizedMethodNode.
	lastIntervalsAndNode := nil.

	^allIntervals collect: [ :anInterval |
		lastIntervalsAndNode := self
			assertCanExtractFrom: anInterval
			for: sourceCode
			methodNode: methodNode
			last: lastIntervalsAndNode ]	! !

!ExtractAsParameter class methodsFor: 'intervals' stamp: 'HAW 7/27/2023 17:22:20'!
intervalsForEquivalentExpressionIn: method at: interval

	| methodNode node sourceCode trimmedIntervalToReplace rangeOrRanges |

	sourceCode := method sourceCode.
	methodNode := method notOptimizedMethodNode.
	trimmedIntervalToReplace := interval asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: interval.

	rangeOrRanges := methodNode rangeForNode: node ifAbsent: [ self shouldNotHappenBecause: 'node already exist' ].
	^(methodNode isMultipleRanges: rangeOrRanges)
		ifTrue: [ rangeOrRanges ]
		ifFalse: [ self intervalsForEquivalentNodesTo: node in: methodNode ]
				! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'HAW 7/27/2023 17:25:24'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments

	| trimmedIntervalToExtract newSourceCode |

	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom notOptimizedMethodNode.

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


!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 7/26/2023 17:20:19'!
shouldBeEnclosedWithParens

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode := methodToExtractFrom notOptimizedMethodNode.
	initialNodeAncestors  := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon := initialNodeAncestors intersection: finalNodeAncestors.

	initialNode := (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode := (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions := initialNode isMessageNode and: [ finalNode isMessageNode ].

	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 7/27/2023 17:26:41'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor notOptimizedMethodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		methodNode: methodNodeToRefactor
		declaringTempIn: codeNodeForNewVariable! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 7/27/2023 17:42:40'!
declareNecessaryTemporaries

	temporariesToDeclareByInsertionPoint keysAndValuesDo: [:aSenderAndPosition :temporariesToDeclare |
		| sender positionToInsertTemporaries temporariesString |
		sender := aSenderAndPosition key.
		positionToInsertTemporaries := aSenderAndPosition value.
		temporariesString := temporariesToDeclare reduce: [:t1 : t2 | t1 value, ' ', t2 value].
		((sender notOptimizedMethodNode parseNodesPathAt: positionToInsertTemporaries ifAbsent: 	[self shouldNotHappen ]) first key isTemporariesDeclaration)
			ifTrue: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: temporariesString, ' |'
				inMethod: sender.
			]
			ifFalse: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: '| ',  temporariesString, ' |', Character newLineCharacter asString, Character tab asString
				inMethod: sender
			]
	]! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 7/27/2023 17:43:19'!
declareTemporaries: temporariesToDeclare in: aMethodOrBlockNode of: aSenderMethod

	| positionToInsertNewVariablesDeclarations renamedTemporaries temporariesToDeclareInPosition|

	renamedTemporaries := self renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod.

	aMethodOrBlockNode hasTemporaryVariables
		ifTrue: [
			positionToInsertNewVariablesDeclarations := (aSenderMethod notOptimizedMethodNode
				singleCompleteSourceRangeOf: (aMethodOrBlockNode temporariesDeclaration)) last.
		]
		ifFalse: [
			| firstStatementRange |
			firstStatementRange := aSenderMethod notOptimizedMethodNode singleCompleteSourceRangeOf: (aMethodOrBlockNode statements first).
			positionToInsertNewVariablesDeclarations := firstStatementRange first.
		].
	temporariesToDeclareInPosition := self temporariesToDeclareByInsertionPoint: (aSenderMethod -> positionToInsertNewVariablesDeclarations).
	temporariesToDeclareInPosition addAll: (renamedTemporaries collect: [:aReplacementAssociation | aReplacementAssociation value]).
	^renamedTemporaries.
	! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 7/27/2023 17:51:05'!
enclosingBlockOf: aMessageNodeReferenceWithReturnValue

	| parseNodeSourceRange methodNode |

	methodNode := aMessageNodeReferenceWithReturnValue methodNode.
	parseNodeSourceRange := aMessageNodeReferenceWithReturnValue completeSourceRange.
	^self enclosingBlockOf: parseNodeSourceRange in: methodNode.! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 7/27/2023 17:44:15'!
findSourceRangeOf: aMessageNodeWithReturnValue in: aSenderMethod

	^ (aSenderMethod notOptimizedMethodNode completeSourceRangesOf: aMessageNodeWithReturnValue ifAbsent: [self shouldNotHappen]) anyOne! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 7/27/2023 17:48:16'!
temporariesAndArgsInScopeOf: aParseNode ofSender: aSender

	| enclosingNodes parseNodeRange methodNode temporariesInScope |

	temporariesInScope := Set new.
	methodNode := aSender notOptimizedMethodNode.
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

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 7/27/2023 17:51:13'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements senderCompleteSourceRanges argumentsAndTemporariesReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	self calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.

	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	indentation := self indentationUpTo: aMessageNodeReference completeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.
	inlinedStatements := (statementsToInline size > 1)
		ifTrue: [((statementsToInline allButLast
			 collect: [:aStatementToInline |
				self sourceCodeOfImplementorStatement: aStatementToInline withReplacements: argumentsAndTemporariesReplacements
					removingReturn: removeReturn.])
				reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
					aStatementWithReplacements, Character newLineCharacter asString, indentation,
						(nextStatementWithReplacements copyReplaceAll: String newLineString with: String newLineString asTokens: false)]),
					Character newLineCharacter asString]
		ifFalse: [''].
	inlinedStatements := inlinedStatements, (self sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements removingReturn: removeReturn).

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 7/27/2023 17:52:58'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference usedInNode: aParseNode

	| inlinedStatements argumentsAndTemporariesReplacements senderCompleteSourceRanges usageNodeSourceRange
	 inlinedUsageStatement statementsWithReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	usageNodeSourceRange := (self findRangesOf: aParseNode in: senderCompleteSourceRanges) first.

	self calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.

	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	statementsWithReplacements := statementsToInline allButLast collect: [:aStatementToInline |
		self sourceCodeOfImplementorStatement: aStatementToInline withReplacements: argumentsAndTemporariesReplacements removingReturn: true].
	statementsWithReplacements add: (self sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements removingReturn: true).

	indentation := self indentationUpTo: usageNodeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.

	statementsWithReplacements size > 1
		ifTrue: [inlinedStatements := (statementsWithReplacements allButLast)
			reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
				aStatementWithReplacements, String newLineString, indentation, (nextStatementWithReplacements
					copyReplaceAll: String newLineString with: String newLineString, indentation asTokens: false) ]]
		 ifFalse: [inlinedStatements := ''].
	inlinedUsageStatement := self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
		lastStatement: statementsToInline last replacement: statementsWithReplacements last.

	statementsWithReplacements size > 1
		ifTrue: [inlinedStatements := inlinedStatements, String newLineString, indentation, inlinedUsageStatement]
		ifFalse: [inlinedStatements := inlinedUsageStatement ].

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 7/27/2023 17:46:07'!
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
	methodNodeToInline arguments withIndexDo: [:anArgumentNode :argIndex | | passedArgument |
		passedArgument := aMessageNodeReference messageNode arguments at: argIndex.
		(self findRangesOf: anArgumentNode in: 	implementorCompleteSourceRanges) do: [:aRange |
			replacements at: aRange put: (self sourceCodeOfNode: passedArgument ofSender: aMessageNodeReference compiledMethod using: senderRanges).]

	].

	methodNodeToInline nodesDo: [:aParseNode | (aParseNode isVariableNode and: [aParseNode referencesSelf]) ifTrue: [
		(self findRangesOf: aParseNode in: 	implementorCompleteSourceRanges) do: [:rangeOfSelfReference |
			"Given previous validations, we can be sure that the receiver is a variable node"
			replacements at: rangeOfSelfReference put: aMessageNodeReference messageNode receiver name.
		]
	]].
	^replacements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 7/27/2023 17:53:57'!
messageSendIsInsideMessageNode: aMessageNodeReference

	| completeSourceRanges methodNode |

	methodNode := aMessageNodeReference methodNode.
	completeSourceRanges := methodNode completeSourceRanges.
	methodNode nodesDo: [:aNode |
		(aNode isMessageNode and: 	[(self findRangesOf: aNode in: completeSourceRanges)
			anySatisfy: [:aMessageNodeRange |
				| messageSendRange |
				messageSendRange := aMessageNodeReference completeSourceRange.
				(aMessageNodeRange includesAllOf: messageSendRange) and: [aMessageNodeRange ~= messageSendRange]]
			])
				ifTrue: [^true]
	].

	^false! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 7/27/2023 17:53:10'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression addingParentheses: addParentheses

	| sourceRangeOfUsage enclosingNodeSourceRange normalizedMessageSendRange expression|

	enclosingNodeSourceRange := (self findRangesOf: aParseNode in: aMessageNodeReference methodNode completeSourceRanges) first.
	sourceRangeOfUsage := aMessageNodeReference completeSourceRange.
	normalizedMessageSendRange := (sourceRangeOfUsage first - enclosingNodeSourceRange first + 1)
					to: (sourceRangeOfUsage last - enclosingNodeSourceRange first + 1).
	"If the expression we are replacing is not at the end of the enclosing node, and it ends with a dot, we must remove it in order for it to be a valid statement"
	expression := ((sourceRangeOfUsage last < enclosingNodeSourceRange last) and: [anExpression endsWith: '.'])
		ifTrue: [anExpression allButLast] ifFalse: [anExpression].
	expression := addParentheses ifTrue: [self addParenthesesIfNeededTo: expression] ifFalse: [expression].

	^(aMessageNodeReference compiledMethod sourceCode copyFrom: enclosingNodeSourceRange first to: enclosingNodeSourceRange last)
		copyReplacing: (Array with: ((normalizedMessageSendRange -> expression)))! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 7/27/2023 17:52:47'!
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

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 7/27/2023 17:45:28'!
inlineMessageSend: aMessageNodeReference

	| statementsToInline |

	statementsToInline := self removeReturnSelfStatementIfNeeded: methodNodeToInline block statements.
	statementsToInline isEmpty ifTrue: [^self].
	statementsToInline last isReturn
		ifTrue: [self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReference.]
		ifFalse: [self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference
			 removingReturn: true].


	! !

!InlineMethod methodsFor: 'initialization' stamp: 'HAW 7/27/2023 17:45:12'!
methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	updatedSendersCode := Dictionary ofSize: ((messageSendsToInline groupBy: [:aMessageNodeReference | aMessageNodeReference methodReference]) keys) size.
	methodNodeToInline := methodToInline notOptimizedMethodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.! !


!InlineMethod class methodsFor: 'preconditions' stamp: 'HAW 7/27/2023 17:36:19'!
assertCanInlineMethod: aCompiledMethodToInline intoMessageSends: aCollectionOfMessageSends

	| senders methodNode |

	methodNode := aCompiledMethodToInline notOptimizedMethodNode.
	self assertNoCascadingMessages: aCollectionOfMessageSends.
	senders := aCollectionOfMessageSends collect: [:aMessageNodeReference | aMessageNodeReference actualClass].
	self assertSenders: senders haveAccessToPrivateVariablesOf: aCompiledMethodToInline methodNode: methodNode.
	self assertMethodToInlineHasOnlyOneReturn: methodNode.
	self assertCanInlineWithReferencesToSelf: aCompiledMethodToInline from: aCollectionOfMessageSends.
	self assertCanInlineWithReferencesToSuper: aCompiledMethodToInline from: aCollectionOfMessageSends.
! !

!InlineMethod class methodsFor: 'preconditions' stamp: 'HAW 7/27/2023 17:29:39'!
assertMethodToInlineHasOnlyOneReturn: aMethodNode

	| returnsCount |

	returnsCount := 0.
	aMethodNode nodesDo: [:aNode | aNode isReturn ifTrue: [
			returnsCount := returnsCount  + 1.
			returnsCount > 1 ifTrue: [self signalMethodHasMultipleReturnsErrorMessage ]
		]
	]! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'HAW 7/27/2023 17:44:05'!
findReferencesToSelector: aSelectorToInline in: aMethodReference

	| references |

	references := Set new.
	aMethodReference notOptimizedMethodNode completeSourceRangesDo: [:aNode :ranges |
		(aNode isMessageNode and: [aNode selector key = aSelectorToInline]) ifTrue: [
			ranges do: [:range  | references add: (MessageNodeReference messageNode: aNode
				selector: aMethodReference methodSymbol class: aMethodReference methodClass completeSourceRange: range)]
		]
	].

	^references.

	! !


!InlineTemporaryVariable class methodsFor: 'instance creation' stamp: 'HAW 7/27/2023 18:03:17'!
named: tempVarToInlineName atUsageInterval: usageInterval inMethod: compiledMethodToRefactor

	| oldVariableNodeAndUsageInterval methodNode |

	methodNode := compiledMethodToRefactor notOptimizedMethodNode.
	oldVariableNodeAndUsageInterval := self findTemporaryNamed: tempVarToInlineName atUsage: usageInterval
		inMethodNode: methodNode.

	^self new
		initializeOldVariableNode: oldVariableNodeAndUsageInterval first
		usage: oldVariableNodeAndUsageInterval second
		method: compiledMethodToRefactor
		methodNode: methodNode.! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:39:23'!
keepMethodToChangeNamed: aSelector in: aClass

	| methodToChange rangesToChange |

	methodToChange := aClass >> aSelector.
	rangesToChange :=  methodToChange notOptimizedMethodNode positionsForInstanceVariable: oldVariable ifAbsent: [ #() ].

	methodsAndRangesToChange add: methodToChange -> rangesToChange ! !


!RenameClass methodsFor: 'applying' stamp: 'HAW 7/27/2023 18:14:16'!
apply

	classToRename safeRenameTo: newName.
	^self renameReferences.

	! !

!RenameClass methodsFor: 'initialization' stamp: 'HAW 7/27/2023 18:14:24'!
initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary

	classToRename := aClass.
	oldName := aClass name.
	newName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary.

	! !

!RenameClass methodsFor: 'accessing' stamp: 'HAW 7/27/2023 18:14:16'!
referencesToOldClass

	^system allCallsOn: newName! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:14:16'!
referencesNewClassName: aMethodReference

	^self references: aMethodReference classVarNamed: newName ! !

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 7/27/2023 18:24:36'!
rejectReferencesToClassVariablesFrom: references

	^references reject: [ :aMethodReference | (self referencesOldName: aMethodReference) or: [ self referencesNewClassName: aMethodReference ] ].! !


!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 7/27/2023 18:39:54'!
assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove

	| newClass |

	aMethodToMove methodClass isMeta
		ifTrue: [newClass := aMethodToMove methodClass soleInstance]
		ifFalse: [newClass := aMethodToMove methodClass class].

	newClass instVarNames
		do: [ :instVarName |
			(aMethodToMove notOptimizedMethodNode hasLocalNamed: instVarName)
				ifTrue: [self signalLocalVariableConflictsWithInstanceVariable]
			]! !


!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/27/2023 19:05:02'!
askInsertionIndex

	| methodNode originalMethod parameterNames |

	"See RemoveParameterApplier#askParameterToRemove to understand why I ask for the index using
	the keywords when no method is found - Hernan"
	originalMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askInsertionIndexUsingKeywords ].

	methodNode := originalMethod notOptimizedMethodNode.
	parameterNames := methodNode argumentNames.
	parameterNames add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Add parameter before?'.
	parameterIndex = 0 ifTrue: [self endRequest ].

		! !


!RemoveParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/27/2023 19:05:23'!
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

	methodNode := selectedMethod notOptimizedMethodNode.
	parameterNames := methodNode argumentNames.

	parameterToRemoveIndex := parameterNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ self selectParameterIndexToRemoveFrom: parameterNames ].

	parameterToRemoveName := parameterNames at: parameterToRemoveIndex.


	! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/27/2023 19:05:43'!
parseNodesToParameterize

	^ ExtractMethodParametersDetector
		valueFor: methodToExtractCodeFrom notOptimizedMethodNode
		at: intervalToExtract! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'HAW 7/27/2023 19:06:07'!
on: anEditor for: aSelectionInterval of: aMethodToRefactor

	| noTempFoundBlock tempToInline usageInterval |

	noTempFoundBlock := [
		"This case will usually end in a failure because a temporary variable is not related to the selected interval"
		tempToInline := anEditor wordUnder: aSelectionInterval first.
		usageInterval := aSelectionInterval.
	].

	anEditor
		withNodeUnderCursorDo: [:aNode | | methodNode sourceRanges |
			methodNode := aMethodToRefactor notOptimizedMethodNode.
			sourceRanges := methodNode completeSourceRanges.
			(aNode isVariableNode and: [methodNode argumentNames noneSatisfy: [:argument | argument = aNode name]])
				ifTrue: [
					tempToInline := aNode name.
					usageInterval := self findSourceRangeOfNode: aNode containingIndex: anEditor startIndex using: sourceRanges.
				]
				ifFalse: [ | assignmentNodeAndRange |
					assignmentNodeAndRange := self searchAssignmentInStatementContaining: aSelectionInterval first
						in: methodNode.
					assignmentNodeAndRange 	ifNil: [noTempFoundBlock value].
					tempToInline ifNil: [
						tempToInline := assignmentNodeAndRange key variable name.
						usageInterval := self findLeftmostSourceRangeOfNode: assignmentNodeAndRange key variable
							inside: assignmentNodeAndRange value using: sourceRanges.
					]
				]
		]
		ifAbsent: [
			noTempFoundBlock value.
		].

	^self new initializeOn: anEditor codeProvider forTemporary: tempToInline
		at: usageInterval of: aMethodToRefactor ! !


!SourceCodeIntervalPrecondition methodsFor: 'initialization' stamp: 'HAW 7/26/2023 17:20:28'!
initializeFor: anIntervalToExtract of: aMethodUnderValidation

	intervalToExtract := anIntervalToExtract.
	method := aMethodUnderValidation.
	methodNode := method notOptimizedMethodNode.
	sourceCode := method sourceCode.! !

!methodRemoval: RenameGlobal #newName stamp: 'HAW 7/27/2023 18:33:33'!
RenameGlobal removeSelector: #newName!
!methodRemoval: RenameGlobal #rangesToReplaceOf: stamp: 'HAW 7/27/2023 18:17:44'!
RenameGlobal removeSelector: #rangesToReplaceOf:!
!methodRemoval: RenameGlobal #references:classVarNamed: stamp: 'HAW 7/27/2023 18:22:24'!
RenameGlobal removeSelector: #references:classVarNamed:!
!methodRemoval: RenameGlobal #newSourceCodeOf: stamp: 'HAW 7/27/2023 18:16:04'!
RenameGlobal removeSelector: #newSourceCodeOf:!
!methodRemoval: RenameGlobal #rangesForLiteralOf: stamp: 'HAW 7/27/2023 18:16:43'!
RenameGlobal removeSelector: #rangesForLiteralOf:!
!methodRemoval: RenameGlobal #renameReference: stamp: 'HAW 7/27/2023 18:17:28'!
RenameGlobal removeSelector: #renameReference:!
!methodRemoval: RenameGlobal #rangesForLiteralVariableOf: stamp: 'HAW 7/27/2023 18:17:00'!
RenameGlobal removeSelector: #rangesForLiteralVariableOf:!
!methodRemoval: RenameGlobal #referencesToOldName stamp: 'HAW 7/27/2023 18:26:06'!
RenameGlobal removeSelector: #referencesToOldName!
!methodRemoval: RenameGlobal #referencesOldName: stamp: 'HAW 7/27/2023 18:24:57'!
RenameGlobal removeSelector: #referencesOldName:!
!methodRemoval: RenameGlobal #renameReferences stamp: 'HAW 7/27/2023 18:35:41'!
RenameGlobal removeSelector: #renameReferences!
!methodRemoval: RenameClass #newName stamp: 'HAW 7/27/2023 18:33:33'!
RenameClass removeSelector: #newName!
!methodRemoval: RenameClass #references:classVarNamed: stamp: 'HAW 7/27/2023 18:22:24'!
RenameClass removeSelector: #references:classVarNamed:!
!methodRemoval: RenameClass #newSourceCodeOf: stamp: 'HAW 7/27/2023 18:16:04'!
RenameClass removeSelector: #newSourceCodeOf:!
!methodRemoval: RenameClass #rangesForLiteralVariableOf: stamp: 'HAW 7/27/2023 18:17:00'!
RenameClass removeSelector: #rangesForLiteralVariableOf:!
!methodRemoval: RenameClass #referencesToOldName stamp: 'HAW 7/27/2023 18:26:06'!
RenameClass removeSelector: #referencesToOldName!
!methodRemoval: RenameClass #renameReferences stamp: 'HAW 7/27/2023 18:35:41'!
RenameClass removeSelector: #renameReferences!
!methodRemoval: RenameClass #rangesToReplaceOf: stamp: 'HAW 7/27/2023 18:17:44'!
RenameClass removeSelector: #rangesToReplaceOf:!
!methodRemoval: RenameClass #referencesToOldClassName stamp: 'HAW 7/27/2023 18:25:53'!
RenameClass removeSelector: #referencesToOldClassName!
!methodRemoval: RenameClass #rangesForLiteralOf: stamp: 'HAW 7/27/2023 18:16:43'!
RenameClass removeSelector: #rangesForLiteralOf:!
!methodRemoval: RenameClass #renameReference: stamp: 'HAW 7/27/2023 18:17:28'!
RenameClass removeSelector: #renameReference:!
!methodRemoval: RenameClass #newClassName stamp: 'HAW 7/27/2023 18:33:22'!
RenameClass removeSelector: #newClassName!
!methodRemoval: RenameClass #referencesOldName: stamp: 'HAW 7/27/2023 18:24:57'!
RenameClass removeSelector: #referencesOldName:!
!methodRemoval: RenameClass #referencesOldClassName: stamp: 'HAW 7/27/2023 18:24:36'!
RenameClass removeSelector: #referencesOldClassName:!
!methodRemoval: InlineMethod class #assertSendersHaveAccessToPrivateVariables:of: stamp: 'HAW 7/27/2023 17:28:01'!
InlineMethod class removeSelector: #assertSendersHaveAccessToPrivateVariables:of:!
!methodRemoval: InlineMethod class #assertSenders:haveAccessToPrivateVariablesOf: stamp: 'HAW 7/27/2023 17:32:20'!
InlineMethod class removeSelector: #assertSenders:haveAccessToPrivateVariablesOf:!
!methodRemoval: InlineMethod #calculateParameterReplacementsFrom:withRanges: stamp: 'HAW 7/27/2023 17:39:32'!
InlineMethod removeSelector: #calculateParameterReplacementsFrom:withRanges:!
!methodRemoval: MessageNodeReference #notOptimizedMethodNode stamp: 'HAW 7/27/2023 18:02:08'!
MessageNodeReference removeSelector: #notOptimizedMethodNode!
!methodRemoval: MethodReference #createMethodNodeOptimizeSpecialSends: stamp: 'HAW 7/26/2023 17:21:32'!
MethodReference removeSelector: #createMethodNodeOptimizeSpecialSends:!
!methodRemoval: Compiler #compile:in:classified:notifying:ifFail: stamp: 'HAW 7/26/2023 18:40:10'!
Compiler removeSelector: #compile:in:classified:notifying:ifFail:!

!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isGetterOf:at: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: regularLiterals sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: notOptimizedMethodNode)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asMethodReference asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('source code ranges' messageSendsRangesOf:)
('user interface support' browse)
('copying' flattenTo:)
!


!Compiler reorganize!
('public access' compile:in:classified:notifying:ifFail:optimizeSpecialSends: compile:in:notifying:ifFail: compile:in:notifying:ifFail:optimizeSpecialSends: compileDoIt:in:context:notifying:ifFail: compileNoPattern:in:context:notifying:ifFail: evaluate:in:to: evaluate:in:to:notifying:ifFail: evaluate:in:to:notifying:ifFail:logged:profiled: evaluateMethod:to:logged:profiled: format:in:notifying: parser parser: parserClass)
('private' format:noPattern:ifFail: from:class:context:notifying: interactive translate:noPattern:doIt:ifFail: translate:noPattern:doIt:ifFail:optimizeSpecialSends: translate:noPattern:ifFail: translate:noPattern:ifFail:optimizeSpecialSends:)
('error handling' notify: notify:at:)
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
('method node' methodNode notOptimizedMethodNode)
!


!MessageNodeReference reorganize!
('printing' displayStringOrText prefixStringVersionWith: printOn: removeStringVersionPrefix stringVersion stringVersion:)
('accesing' actualClass compiledMethod completeSourceRange isPossibleMessageSend messageNode messageSendsRangesOf: methodNode methodReference methodSymbol selectorOfMessageNode)
('comparing' asPossibleMessageNodeReference equivalentTo:)
('testing' isMessageListTitle)
('initialization' messageNode:selector:class:completeSourceRange:)
!


!InlineMethod class reorganize!
('initialization' from:intoSendersAndUsages:removeMethod:)
('preconditions' assertCanInlineMethod:intoMessageSends: assertCanInlineWithReferencesToSelf:from: assertCanInlineWithReferencesToSuper:from: assertMethodToInlineHasOnlyOneReturn: assertNoCascadingMessages: assertSenders:belongToClassOf:ifFalse: assertSenders:haveAccessToPrivateVariablesOf:methodNode: methodToInlineReferencesSelfErrorMessage sender:belongsToClassOf: signalMethodToInlineReferencesSelf)
('errors' messageSendCanNotBeCascadeErrorMessage methodAccessPrivateVariablesNotVisibleToSenderErrorMessage methodHasMultipleReturnsErrorMessage methodToInlineReferencesSuperErrorMessage signalMessageSendCanNotBeCascade signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage signalMethodHasMultipleReturnsErrorMessage signalMethodToInlineReferencesSuper)
('implementors and senders' addImplementorsOf:to:andUsagesTo:forClassAndMetaOf: addImplementorsOf:to:andUsagesTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andUsagesTo:inCategory:organizedBy: addImplementorsOf:to:andUsagesTo:inHierarchyOf: addImplementorsOf:to:andUsagesTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andUsagesTo:inSystem: addImplementorsOf:to:andUsagesTo:of: convertToSender:of:referencing: findReferencesToSelector:in:)
!

!classDefinition: #RenameClass category: #'Tools-Refactoring'!
RenameObject subclass: #RenameClass
	instanceVariableNames: 'classToRename undeclared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameClass reorganize!
('applying' apply)
('initialization' initializeFrom:to:in:undeclared:)
('accessing' referencesToOldClass)
('applying - private' referencesNewClassName: referencesToRename rejectReferencesToClassVariablesFrom:)
!

!classDefinition: #RenameGlobal category: #'Tools-Refactoring'!
RenameObject subclass: #RenameGlobal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameGlobal reorganize!
('applying' apply)
('initialization' initializeFrom:to:in:)
('applying - private' referencesToRename rejectReferencesToClassVariablesFrom:)
!
