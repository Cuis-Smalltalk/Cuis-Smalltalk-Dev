'From Cuis 6.0 [latest update: #5466] on 2 September 2022 at 5:47:03 pm'!

!InlineMethodUsagesStepWindow class methodsFor: 'instance creation' stamp: 'FB 9/2/2022 17:43:18'!
openFrom: anInlineMethodApplier

	| window messageSendsSet |

	messageSendsSet := InlineMethodMessageSendsSet applier: anInlineMethodApplier.
	
	messageSendsSet autoSelectString: anInlineMethodApplier selectorToInline.

	window := self open: messageSendsSet label: 'Message sends of #', anInlineMethodApplier selectorToInline, ' to Refactor'.
	window initializeFrom: anInlineMethodApplier.

	^window.! !


!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 8/28/2022 21:49:46'!
messageSendsRangesOf: aSelector

	"To use polimorphysm with MethodReference - Fernando"

	^self isMessageListTitle ifTrue: [#()] ifFalse: [Array with: completeSourceRange].! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 8/28/2022 23:12:50'!
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

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 9/2/2022 17:45:26'!
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

