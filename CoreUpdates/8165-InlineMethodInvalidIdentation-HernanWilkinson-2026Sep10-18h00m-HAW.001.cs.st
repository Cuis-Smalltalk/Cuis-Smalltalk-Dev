'From Cuis7.9 [latest update: #8163] on 10 September 2026 at 7:00:06 pm'!

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 10/Sep/2026, 9:29:51 pm (UTC)'!
declarationOf: aTemporariesString insertedAt: aPosition in: aSenderMethodNode

	| declaration |

	declaration := '| ', aTemporariesString, ' |', String newLineString.

	^(self enclosingBlockOf: (aPosition to: aPosition) in: aSenderMethodNode) isBlockNode
		ifTrue: [ declaration, Character tab asString ]
		ifFalse: [ declaration, String newLineString, Character tab asString ]! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 10/Sep/2026, 9:34:24 pm (UTC)'!
declareNecessaryTemporaries

	temporariesToDeclareByInsertionPoint keysAndValuesDo: [:aSenderAndPosition :temporariesToDeclare |
		| sender positionToInsertTemporaries temporariesString senderMethodNode nodeAtTempPosition temporariesDeclarationString |
	
		sender := aSenderAndPosition key.
		positionToInsertTemporaries := aSenderAndPosition value.
		temporariesString := temporariesToDeclare reduce: [:t1 : t2 | t1 value, ' ', t2 value].
		senderMethodNode := sender notOptimizedMethodNode.
		nodeAtTempPosition := (senderMethodNode 
			parseNodesPathAt: positionToInsertTemporaries 
			ifAbsent: [self shouldNotHappen ]) first key.
		temporariesDeclarationString := nodeAtTempPosition isTemporariesDeclaration
			ifTrue: [ temporariesString, ' |' ]
			ifFalse: [self declarationOf: temporariesString insertedAt: positionToInsertTemporaries in: senderMethodNode].
		self 
			replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
			withNewSourceCode: temporariesDeclarationString
			inMethod: sender
	]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 10/Sep/2026, 9:49:20 pm (UTC)'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements senderCompleteSourceRanges argumentsAndTemporariesReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	self calculateReplacementsFrom: aMessageNodeReference usingRanges: senderCompleteSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.
	indentation := self 
		indentationUpTo: aMessageNodeReference completeSourceRange first 
		in: aMessageNodeReference compiledMethod sourceCode.
	inlinedStatements := statementsToInline size > 1
		ifTrue: [ | lines source |
			lines := statementsToInline allButLast collect: [:aStatementToInline |
				self 
					sourceCodeOfImplementorStatement: aStatementToInline 
					withReplacements: argumentsAndTemporariesReplacements
					removingReturn: removeReturn].
			source := lines reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
					aStatementWithReplacements, String newLineString, 
					indentation, nextStatementWithReplacements].
			source, String newLineString, indentation]
		ifFalse: [''].
	inlinedStatements := inlinedStatements, (self 
		sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements 
		removingReturn: removeReturn).

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 10/Sep/2026, 9:54:44 pm (UTC)'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference usedInNode: aParseNode

	| inlinedStatements argumentsAndTemporariesReplacements senderCompleteSourceRanges usageNodeSourceRange
	 inlinedUsageStatement statementsWithReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	usageNodeSourceRange := (self findRangesOf: aParseNode in: senderCompleteSourceRanges) first.
	self 
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	statementsWithReplacements := statementsToInline allButLast collect: [:aStatementToInline |
		self 
			sourceCodeOfImplementorStatement: aStatementToInline 
			withReplacements: argumentsAndTemporariesReplacements 
			removingReturn: true].
	statementsWithReplacements add: (self 
		sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements 
		removingReturn: true).

	indentation := self indentationUpTo: usageNodeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.

	inlinedStatements := statementsWithReplacements size > 1
		ifTrue: [ statementsWithReplacements allButLast
			reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
				aStatementWithReplacements, String newLineString, indentation, nextStatementWithReplacements ]]
		 ifFalse: [ ''].
		
	inlinedUsageStatement := self 
		statementWithMessageSend: aMessageNodeReference 
		usedIn: aParseNode
		lastStatement: statementsToInline last 
		replacement: statementsWithReplacements last.

	inlinedStatements :=statementsWithReplacements size > 1
		ifTrue: [ inlinedStatements, String newLineString, indentation, inlinedUsageStatement]
		ifFalse: [ inlinedUsageStatement ].

	^inlinedStatements.! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 10/Sep/2026, 9:52:21 pm (UTC)'!
inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeWithReturnValueReference usedInParseNode: aParseNode

	| inlinedStatements sourceRangeOfUsage senderMethod|
	
	inlinedStatements := self 
		buildInlinedSourceCodeStatementsFrom: statementsToInline 
		withParametersFrom: aMessageNodeWithReturnValueReference
		usedInNode: aParseNode.

	senderMethod := aMessageNodeWithReturnValueReference compiledMethod.
	sourceRangeOfUsage := self findSourceRangeOf: aParseNode in: senderMethod.
	
	self replaceRange: sourceRangeOfUsage withNewSourceCode: inlinedStatements inMethod: senderMethod.


	! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 10/Sep/2026, 9:52:10 pm (UTC)'!
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
			inlinedStatements := self 
				buildInlinedSourceCodeStatementsFrom: statementsToInline 
				withParametersFrom: aMessageNodeReference
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

