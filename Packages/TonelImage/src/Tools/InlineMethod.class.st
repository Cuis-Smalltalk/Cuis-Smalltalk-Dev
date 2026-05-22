"
I am a refactoring that replaces message sends in the sender with the body of the target method , replacing the parameters as needed.
Implementation notes:

- I can refactor multiple message sends at the same time.
- I can delete the target method.
"
Class {
	#name : 'InlineMethod',
	#superclass : 'ScopedRefactoring',
	#instVars : [
		'methodToInline',
		'messageSendsToInline',
		'updatedSendersCode',
		'methodNodeToInline',
		'replacementsByMessageSend',
		'temporariesDeclarationsByNode',
		'temporariesToDeclareByInsertionPoint',
		'implementorCompleteSourceRanges',
		'removeMethod'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'implementors and senders' }
InlineMethod class >> addTo: senders referencesTo: aSelectorToInline from: aSelector in: aPotentialClassToRefactor [

	| methodReference references |

	methodReference := MethodReference class: aPotentialClassToRefactor selector: aSelector.
	references := self findReferencesToSelector: aSelectorToInline in: methodReference.

	senders addAll: references
]

{ #category : 'preconditions' }
InlineMethod class >> assertCanInlineMethod: aCompiledMethodToInline intoMessageSends: aCollectionOfMessageSends [

	| senders methodNode |

	methodNode := aCompiledMethodToInline notOptimizedMethodNode.
	self assertNoCascadingMessages: aCollectionOfMessageSends.
	senders := aCollectionOfMessageSends collect: [:aMessageNodeReference | aMessageNodeReference actualClass].
	self assertSenders: senders haveAccessToPrivateVariablesOf: aCompiledMethodToInline methodNode: methodNode.
	self assertMethodToInlineHasOnlyOneReturn: methodNode.
	self assertCanInlineWithReferencesToSelf: aCompiledMethodToInline from: aCollectionOfMessageSends.
	self assertCanInlineWithReferencesToSuper: aCompiledMethodToInline from: aCollectionOfMessageSends.

]

{ #category : 'preconditions' }
InlineMethod class >> assertCanInlineWithReferencesToSelf: aCompiledMethodToInline from: aCollectionOfMessageSends [

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

]

{ #category : 'preconditions' }
InlineMethod class >> assertCanInlineWithReferencesToSuper: aCompiledMethodToInline from: aCollectionOfMessageSends [


	"Do not allow inlining implementors that reference super unless all the senders belong to the same class,
	since otherwise it's not possible to replace the super keyword with something that maintains the semantics"
	(aCompiledMethodToInline referencesSuper) ifTrue: [
		self assertSenders: aCollectionOfMessageSends belongToClassOf: aCompiledMethodToInline
			ifFalse: [self signalMethodToInlineReferencesSuper]].


]

{ #category : 'preconditions' }
InlineMethod class >> assertMethodToInlineHasOnlyOneReturn: aMethodNode [

	| returnsCount |

	returnsCount := 0.
	aMethodNode nodesDo: [:aNode | aNode isReturn ifTrue: [
			returnsCount := returnsCount  + 1.
			returnsCount > 1 ifTrue: [self signalMethodHasMultipleReturnsErrorMessage ]
		]
	]
]

{ #category : 'preconditions' }
InlineMethod class >> assertNoCascadingMessages: aCollectionOfMessageSends [

	(aCollectionOfMessageSends anySatisfy: [:aMessageNodeReference |
		aMessageNodeReference messageNode isCascade ]) ifTrue: [self signalMessageSendCanNotBeCascade]
]

{ #category : 'preconditions' }
InlineMethod class >> assertSenders: aCollectionOfSenders belongToClassOf: aCompiledMethod ifFalse: ifFalseBlock [

	(aCollectionOfSenders allSatisfy: [:aSender | self sender: aSender belongsToClassOf: aCompiledMethod])
		ifFalse: [^ifFalseBlock value].
]

{ #category : 'preconditions' }
InlineMethod class >> assertSenders: aSendersCollection haveAccessToPrivateVariablesOf: aCompiledMethod methodNode: aMethodNode [

	| methodReferencesInstanceVariables |

	methodReferencesInstanceVariables := false.
	aCompiledMethod notOptimizedMethodNode
		nodesDo: [:aNode | aNode isInstanceVariableNode ifTrue: [methodReferencesInstanceVariables := true]].

	methodReferencesInstanceVariables ifTrue: [
		(aSendersCollection allSatisfy: [:aSenderClass | aSenderClass = aCompiledMethod methodClass])
			ifFalse: [self signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage]
	].
	 
]

{ #category : 'implementors and senders' }
InlineMethod class >> convertToSender: aSenderSelector of: aPotentialClassToRefactor referencing: aSelector [

	^self
		findReferencesToSelector: aSelector
		in: (MethodReference class: aPotentialClassToRefactor selector: aSenderSelector)
]

{ #category : 'implementors and senders' }
InlineMethod class >> findReferencesToSelector: aSelectorToInline in: aMethodReference [

	| references |

	references := Set new.
	aMethodReference notOptimizedMethodNode completeSourceRangesDo: [:aNode :ranges |
		(aNode isMessageNode and: [aNode selector key = aSelectorToInline]) ifTrue: [
			ranges do: [:range  | | referenceToAdd |
				referenceToAdd := MessageNodeReference
					messageNode: aNode
					selector: aMethodReference methodSymbol
					class: aMethodReference methodClass
					completeSourceRange: range.
					references add: referenceToAdd]]].

	^references.

	
]

{ #category : 'instance creation' }
InlineMethod class >> from: aCompiledMethodToInline intoSendersAndUsages: aCollectionOfMessageSends removeMethod: removingMethod [

	self assertCanInlineMethod: aCompiledMethodToInline intoMessageSends: aCollectionOfMessageSends.

	^self new
		methodToInline: aCompiledMethodToInline
		usages: aCollectionOfMessageSends
		removingMethod: removingMethod.
]

{ #category : 'errors' }
InlineMethod class >> messageSendCanNotBeCascadeErrorMessage [

	^'Refactoring a cascade message send is not supported'.
]

{ #category : 'errors' }
InlineMethod class >> methodAccessPrivateVariablesNotVisibleToSenderErrorMessage [

	^'Method to inline is accessing private variables that are not visible to at least one sender'
]

{ #category : 'errors' }
InlineMethod class >> methodHasMultipleReturnsErrorMessage [

	^'Method to inline has more than one possible return value'
]

{ #category : 'preconditions' }
InlineMethod class >> methodToInlineReferencesSelfErrorMessage [

	^'The method to inline references self, but at least one sender does not have a variable as the receiver and it
	 does not belong to the same class'
]

{ #category : 'errors' }
InlineMethod class >> methodToInlineReferencesSuperErrorMessage [

	^'The method to inline references super but there is at least one sender that does not belong to the same class'
]

{ #category : 'preconditions' }
InlineMethod class >> sender: aSender belongsToClassOf: aCompiledMethod [

	^(aSender actualClass = aCompiledMethod methodClass)
]

{ #category : 'errors' }
InlineMethod class >> signalMessageSendCanNotBeCascade [

	^self refactoringError: self messageSendCanNotBeCascadeErrorMessage.
]

{ #category : 'errors' }
InlineMethod class >> signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage [

	^self refactoringError: self methodAccessPrivateVariablesNotVisibleToSenderErrorMessage.
]

{ #category : 'errors' }
InlineMethod class >> signalMethodHasMultipleReturnsErrorMessage [

	^self refactoringError: self methodHasMultipleReturnsErrorMessage.
]

{ #category : 'preconditions' }
InlineMethod class >> signalMethodToInlineReferencesSelf [

	self refactoringError: self methodToInlineReferencesSelfErrorMessage
]

{ #category : 'errors' }
InlineMethod class >> signalMethodToInlineReferencesSuper [

	self refactoringError: self methodToInlineReferencesSuperErrorMessage
]

{ #category : 'source code building - private' }
InlineMethod >> addParenthesesIfNeededTo: anExpression [

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |
	
 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) == $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) == $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [^anExpression]
		ifFalse: [ (anExpression endsWith: '.')
			ifTrue: [^('(', anExpression copyUpToLast: $.), ').']
	 		ifFalse: [^'(', anExpression, ')']
		].
]

{ #category : 'applying' }
InlineMethod >> apply [

	messageSendsToInline do: [:aMessageNodeReference | self inlineMessageSend: aMessageNodeReference].
	self declareNecessaryTemporaries.
	removeMethod ifTrue: [self removeMethodToInline].
	self compileChanges.

	^self changedMethods
]

{ #category : 'source code building - private' }
InlineMethod >> buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn [

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

	^inlinedStatements.
]

{ #category : 'source code building - private' }
InlineMethod >> buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference usedInNode: aParseNode [

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

	^inlinedStatements.
]

{ #category : 'source code building - private' }
InlineMethod >> calculateReplacementsFrom: aMessageNodeReference usingRanges: senderRanges [

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
	^replacements.
]

{ #category : 'changes' }
InlineMethod >> changedMethods [

	| changedMethods |

	changedMethods := (messageSendsToInline collect: [:aMessageNodeReference | aMessageNodeReference methodReference ]) asSet.
	removeMethod ifTrue: [changedMethods add: methodToInline methodReference].
	^changedMethods.
]

{ #category : 'applying primitives - private' }
InlineMethod >> compileChanges [

	updatedSendersCode keysAndValuesDo: [:aMethodReference :aCollectionOfChanges |
		| updatedSenderSourceCode |

		updatedSenderSourceCode := aMethodReference sourceCode copyReplacing: 	aCollectionOfChanges.
		aMethodReference methodClass compile: updatedSenderSourceCode classified: aMethodReference category
	].
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> declareNecessaryTemporaries [

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
	]
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> declareTemporaries: temporariesToDeclare in: aMethodOrBlockNode of: aSenderMethod [

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
	
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> declareTemporaries: temporariesToDeclare inBlockOf: aMessageNodeReference [

	| methodOrBlockNodeDeclaringNewVariables renamedTemporaries replacements |

	methodOrBlockNodeDeclaringNewVariables := self enclosingBlockOf: aMessageNodeReference.
	renamedTemporaries := self declareTemporaries: temporariesToDeclare in: methodOrBlockNodeDeclaringNewVariables of: aMessageNodeReference compiledMethod.

	replacements := self replacementsMapForMessageSend: aMessageNodeReference .
	renamedTemporaries do: [:oldVariableAndNewName |
			(self findRangesOf: oldVariableAndNewName key in: implementorCompleteSourceRanges) do: [:range |
				replacements at: range put: oldVariableAndNewName value.
			]
	]
	
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> declareTemporariesIfNeededFor: aMessageNodeReference [

	| implementorTemporaries |

	implementorTemporaries := methodNodeToInline temporariesDeclaration allDeclaredVariableNodes.
	implementorTemporaries isEmpty ifFalse: [self declareTemporaries: implementorTemporaries
		inBlockOf: aMessageNodeReference].
	
]

{ #category : 'applying primitives - private' }
InlineMethod >> declaredTemporariesFor: aParseNode ofSender: aSender [

	^temporariesDeclarationsByNode keys
		detect: [:senderAndNode | senderAndNode key = aSender and: [senderAndNode value equivalentTo: aParseNode]]
		ifFound: [ :key | temporariesDeclarationsByNode at: key]
		ifNone: [| temporariesAndArgsDict |

			temporariesAndArgsDict := self temporariesAndArgsInScopeOf: aParseNode ofSender: aSender.
			temporariesDeclarationsByNode at: (aSender -> aParseNode) put: temporariesAndArgsDict.
			temporariesAndArgsDict].


	
]

{ #category : 'applying primitives - private' }
InlineMethod >> enclosingBlockOf: aMessageNodeReferenceWithReturnValue [

	| parseNodeSourceRange methodNode |

	methodNode := aMessageNodeReferenceWithReturnValue methodNode.
	parseNodeSourceRange := aMessageNodeReferenceWithReturnValue completeSourceRange.
	^self enclosingBlockOf: parseNodeSourceRange in: methodNode.
]

{ #category : 'applying primitives - private' }
InlineMethod >> enclosingBlockOf: aParseNodeSourceRange in: aMethodNode [

	aMethodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode
			and: [ parseNode ~= aMethodNode ]
			and: [ sourceRanges anySatisfy: [ :sourceRange |
					sourceRange first < aParseNodeSourceRange first and: [ sourceRange last > aParseNodeSourceRange last ] ] ])
				ifTrue: [ ^ parseNode ]
		].
	^aMethodNode.
]

{ #category : 'inlining - private' }
InlineMethod >> findOutermostStatementContaining: aMessageNodeReference enclosedBy: enclosingBlock ifFound: ifFoundBlock ifNone: ifNoneBlock [

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




	
]

{ #category : 'applying primitives - private' }
InlineMethod >> findRangesOf: aNode in: completeSourceRanges [
	"This is needed because the map of source ranges compares nodes by memory
	instead of using equivalentTo: -- Fernando"

	completeSourceRanges keysAndValuesDo: [:aParseNode :sourceRanges |
		(aParseNode equivalentTo: aNode) ifTrue: [^sourceRanges]
	].
	^#().
		
]

{ #category : 'applying primitives - private' }
InlineMethod >> findSourceRangeOf: aMessageNodeWithReturnValue in: aSenderMethod [

	^ (aSenderMethod notOptimizedMethodNode completeSourceRangesOf: aMessageNodeWithReturnValue ifAbsent: [self shouldNotHappen]) anyOne
]

{ #category : 'source code building - private' }
InlineMethod >> indentationUpTo: anIndex in: aSourceCode [

	| indentation currentIndex character|

	currentIndex := anIndex - 1.
	((aSourceCode at: currentIndex) == $^) ifTrue: [currentIndex := currentIndex  -1]. "Skip the return character if present"
	indentation := ''.

	[currentIndex > 0 and: [
		character := (aSourceCode at: currentIndex).
		(character = Character tab) or: [character = Character space]
	]] whileTrue: [
		indentation := indentation, character asString.
		currentIndex := currentIndex - 1.
	].

	^(character = Character newLineCharacter) ifFalse: [^''] ifTrue: [^indentation reversed].
]

{ #category : 'inlining - private' }
InlineMethod >> inlineMessageSend: aMessageNodeReference [

	| statementsToInline |

	statementsToInline := self removeReturnSelfStatementIfNeeded: methodNodeToInline block statements.
	statementsToInline isEmpty ifTrue: [^self].
	statementsToInline last isReturn
		ifTrue: [self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReference.]
		ifFalse: [self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference
			 removingReturn: true].


	
]

{ #category : 'inlining - private' }
InlineMethod >> inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReferenceWithReturnValue [

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
		ifNone: [ 
			self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
				removingReturn: true
		]




	
]

{ #category : 'inlining - private' }
InlineMethod >> inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeWithReturnValueReference usedInParseNode: aParseNode [

	| inlinedStatements sourceRangeOfUsage senderMethod|
	
	inlinedStatements := self buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeWithReturnValueReference
		usedInNode: aParseNode.

	senderMethod := aMessageNodeWithReturnValueReference compiledMethod.
	sourceRangeOfUsage := self findSourceRangeOf: aParseNode in: senderMethod.
	
	self replaceRange: sourceRangeOfUsage withNewSourceCode: inlinedStatements inMethod: senderMethod.


	
]

{ #category : 'inlining - private' }
InlineMethod >> inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference removingReturn: removeReturn [

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



	
]

{ #category : 'source code building - private' }
InlineMethod >> messageSendIsInsideMessageNode: aMessageNodeReference [

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

	^false
]

{ #category : 'initialization' }
InlineMethod >> methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod [

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	updatedSendersCode := Dictionary new.
	methodNodeToInline := methodToInline notOptimizedMethodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.
]

{ #category : 'inlining - private' }
InlineMethod >> parseNode: aParseNode returns: aMessageNode [

	aParseNode nodesDo: [:aNode | (aNode isReturn and: [aNode expr equivalentTo: aMessageNode])
		ifTrue: [^true]].
	^false.




	
]

{ #category : 'applying primitives - private' }
InlineMethod >> removeMethodToInline [

	methodToInline methodClass removeSelector: methodToInline selector.
]

{ #category : 'source code building - private' }
InlineMethod >> removeReturnSelfStatementIfNeeded: aCollectionOfStatements [

	(aCollectionOfStatements last isReturnSelf)
		ifTrue: [aCollectionOfStatements removeLast].

	^aCollectionOfStatements
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod [

	"If a temporary to declare already exists in the scope, a new fresh name
	will be found for it. The method returns an OrderedCollection of (variableNode -> newVariableName)
	replacements. If the variable does not need a rename, it will still be added to the collection.
	This method also adds the new name to the map that tracks temporaries
	names in scope for the requested method or block node across the whole refactoring - Fernando"

	| alreadyDeclaredTemporaries newTemporaries |

	newTemporaries := OrderedCollection new.
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

	^newTemporaries.
]

{ #category : 'applying primitives - private' }
InlineMethod >> replaceRange: aSourceCodeInterval withNewSourceCode: newSourceCode inMethod: aCompiledMethod [

	| currentUpdates sourceCodeReplacement|
	
	sourceCodeReplacement := newSourceCode.
	((aSourceCodeInterval last < aCompiledMethod sourceCode size) and: [((aCompiledMethod sourceCode at: aSourceCodeInterval last + 1) == $.)]
		and: [newSourceCode endsWith: '.']) ifTrue: [sourceCodeReplacement := newSourceCode copyUpToLast: $.].
	"It's important to use < and not <= because temporaries declarations are added to the changes collection
	after the changes concerning the inlining of the implementor code, but they need to be replaced first - Fernando"
	currentUpdates := updatedSendersCode at: aCompiledMethod methodReference ifAbsent: (SortedCollection 
		sortBlock: [ :left :right | left key first < right key first ]).
	currentUpdates add: (aSourceCodeInterval -> sourceCodeReplacement).
	updatedSendersCode at: aCompiledMethod methodReference put: currentUpdates.
	
]

{ #category : 'applying primitives - private' }
InlineMethod >> replacementsMapForMessageSend: aMessageNodeReference [

	^replacementsByMessageSend keys
		detect: [:messageNodeReference | messageNodeReference equivalentTo: aMessageNodeReference]
		ifFound: [ :key | replacementsByMessageSend at: key	]
		 ifNone: [| replacementsDict |

			replacementsDict := Dictionary new.
			replacementsByMessageSend at: aMessageNodeReference put: replacementsDict.
			replacementsDict ].

	
]

{ #category : 'source code building - private' }
InlineMethod >> sourceCodeOfImplementorStatement: aStatementToInline withReplacements: replacementsDict removingReturn: removeReturn [

	| sourceRange originalSourceCode statementReplacements implementorSourceCode |

	implementorSourceCode := methodToInline sourceCode.
	sourceRange := (self findRangesOf: aStatementToInline in: implementorCompleteSourceRanges) anyOne
		asSourceCodeInterval expandUntilStatementEndOn: implementorSourceCode.
	originalSourceCode := implementorSourceCode copyFrom: sourceRange first to: sourceRange last.
	"Replacements must be adjusted because the statement string begins at index 1 instead of the
	original position on the source code"
	statementReplacements := ((replacementsDict associations select: [:anAssociation | sourceRange includesAllOf: (anAssociation key)])
		collect: [:anAssociation | ((anAssociation key first - sourceRange first + 1) to: (anAssociation key last - sourceRange first + 1)) -> anAssociation value])
		asSortedCollection: [ :left :right | left key first < right key first ].

	"Remove the return character if present"
	(removeReturn and: [aStatementToInline isReturn]) ifTrue: [statementReplacements add: (1 to: 1) -> ''].
	^(originalSourceCode copyReplacing: statementReplacements).
]

{ #category : 'applying primitives - private' }
InlineMethod >> sourceCodeOfNode: aParseNode ofSender: aSender using: completeSourceCodeRanges [

	completeSourceCodeRanges keysAndValuesDo: [:aNode :aCollectionOfRanges |
		(aNode equivalentTo: aParseNode) ifTrue: [ |range|
			range := aCollectionOfRanges anyOne.
			^aSender sourceCode copyFrom: range first to: range last
		]
	].
	self shouldNotHappen.
]

{ #category : 'source code building - private' }
InlineMethod >> statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression addingParentheses: addParentheses [

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
		copyReplacing: (Array with: ((normalizedMessageSendRange -> expression)))
]

{ #category : 'source code building - private' }
InlineMethod >> statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline [
	replacement: lastStatementWithReplacements

	| requiresParenthesis |

	requiresParenthesis := (implementorLastStatementToInline expr isMessageNode) 
		and:[ self messageSendIsInsideMessageNode: aMessageNodeReference].
	
	^self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
				inlinedWith: lastStatementWithReplacements addingParentheses: requiresParenthesis.
		
]

{ #category : 'applying primitives - private' }
InlineMethod >> temporariesAndArgsInScopeOf: aParseNode ofSender: aSender [

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



	
]

{ #category : 'temporaries declaration - private' }
InlineMethod >> temporariesToDeclareByInsertionPoint: aSenderAndPosition [

	^temporariesToDeclareByInsertionPoint keys
		detect: [:senderAndPosition | senderAndPosition = aSenderAndPosition]
		ifFound: [ :key | temporariesToDeclareByInsertionPoint at: key ]
		ifNone: [| temporariesToDeclareAtInsertionPoint |
			temporariesToDeclareAtInsertionPoint := OrderedCollection new.
			temporariesToDeclareByInsertionPoint at: aSenderAndPosition put: temporariesToDeclareAtInsertionPoint.
			temporariesToDeclareAtInsertionPoint].
	
]
