'From Cuis 6.0 [latest update: #5324] on 4 July 2022 at 9:21:16 pm'!

!SourceCodeInterval methodsFor: 'source code' stamp: 'FB 7/4/2022 21:16:00'!
expandUntilStatementEndOn: aSourceCode

	| nextDotIdx |
	
	nextDotIdx := aSourceCode findDelimiters: '.' startingAt: self last.
	^SourceCodeInterval
		from: self first
		to: (nextDotIdx min: aSourceCode size)
	! !


!InlineMethod methodsFor: 'applying primitives - private' stamp: 'FB 7/4/2022 21:18:15'!
replaceRange: aSourceCodeInterval withNewSourceCode: newSourceCode inMethod: aCompiledMethod

	| currentUpdates sourceCodeReplacement|
	
	sourceCodeReplacement := newSourceCode.
	((aSourceCodeInterval last < aCompiledMethod sourceCode size) and: [((aCompiledMethod sourceCode at: aSourceCodeInterval last + 1) = $.)]
		and: [newSourceCode endsWith: '.']) ifTrue: [sourceCodeReplacement := newSourceCode copyUpToLast: $.].
	"It's important to use < and not <= because temporaries declarations are added to the changes collection
	after the changes concerning the inlining of the implementor code, but they need to be replaced first - Fernando"
	currentUpdates := updatedSendersCode at: aCompiledMethod methodReference ifAbsent: (SortedCollection 
		sortBlock: [ :left :right | left key first < right key first ]).
	currentUpdates add: (aSourceCodeInterval -> sourceCodeReplacement).
	updatedSendersCode at: aCompiledMethod methodReference put: currentUpdates.
	! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 20:15:49'!
addParenthesesIfNeededTo: anExpression

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |

 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) = $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) = $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [^anExpression]
		ifFalse: [ (anExpression endsWith: '.')
			ifTrue: [^('(', anExpression copyUpToLast: $.), ').']
	 		ifFalse: [^'(', anExpression, ')']
		].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 21:17:27'!
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

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 20:30:03'!
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

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 19:52:35'!
sourceCodeOfImplementorStatement: aStatementToInline withReplacements: replacementsDict removingReturn: removeReturn

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
	^(originalSourceCode copyReplacing: statementReplacements).! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 20:51:41'!
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

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 7/4/2022 20:29:50'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline
	replacement: lastStatementWithReplacements

	| inlinedStatementContainingMessageSend |

	implementorLastStatementToInline expr isMessageNode
		ifTrue: [ | senderSourceRange enclosingNode |
			senderSourceRange := aMessageNodeReference completeSourceRange.
			(aMessageNodeReference methodNode parseNodesPathAt: senderSourceRange first
				ifAbsent: [self shouldNotHappen])
					detect: [:aNodeAndRange | (aNodeAndRange value includesAllOf: senderSourceRange)
						and: [aNodeAndRange value first <= senderSourceRange first]]
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

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 7/4/2022 20:36:29'!
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
		ifNone: [ 
			self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
				removingReturn: true
		]




	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 7/4/2022 20:47:31'!
inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeWithReturnValueReference usedInParseNode: aParseNode

	| inlinedStatements sourceRangeOfUsage senderMethod|
	
	inlinedStatements := self buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeWithReturnValueReference
		usedInNode: aParseNode.

	senderMethod := aMessageNodeWithReturnValueReference compiledMethod.
	sourceRangeOfUsage := self findSourceRangeOf: aParseNode in: senderMethod.
	
	self replaceRange: sourceRangeOfUsage withNewSourceCode: inlinedStatements inMethod: senderMethod.


	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 7/4/2022 20:22:13'!
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

