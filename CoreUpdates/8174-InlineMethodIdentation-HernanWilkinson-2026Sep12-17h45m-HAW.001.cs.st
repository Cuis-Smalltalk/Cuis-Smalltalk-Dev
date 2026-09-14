'From Cuis7.9 [latest update: #8170] on 12 September 2026 at 5:56:00 pm'!

!MessageNodeReference methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 6:23:50 pm (UTC)'!
lineIndentation

	^self compiledMethod sourceCode
		indentationOfLineAt: self completeSourceRange first! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 6:23:50 pm (UTC)'!
sourceCodeOfImplementorStatement: aStatementToInline withReplacements: replacementsDict removingReturn: removeReturn indentedAs: aSenderIndentation

	| sourceRange originalSourceCode statementReplacements implementorSourceCode inlinedStatement implementorIndentation |

	implementorSourceCode := methodToInline sourceCode.
	sourceRange := (self findRangesOf: aStatementToInline in: implementorCompleteSourceRanges) anyOne
		asSourceCodeInterval expandUntilStatementEndOn: implementorSourceCode.
	originalSourceCode := implementorSourceCode copyFrom: sourceRange first to: sourceRange last.
	"Replacements must be adjusted because the statement string begins at index 1 instead of the
	original position on the source code"
	statementReplacements := ((replacementsDict associations select: [:anAssociation | sourceRange includesAllOf: anAssociation key])
		collect: [:anAssociation | ((anAssociation key first - sourceRange first + 1) to: (anAssociation key last - sourceRange first + 1)) -> anAssociation value])
		asSortedCollection: [ :left :right | left key first < right key first ].

	"Remove the return character if present"
	(removeReturn and: [aStatementToInline isReturn]) ifTrue: [statementReplacements add: (1 to: 1) -> ''].
	inlinedStatement := originalSourceCode copyReplacing: statementReplacements.
	implementorIndentation := implementorSourceCode
		indentationOfLineAt: sourceRange first.

	^implementorIndentation
		ifEmpty: [ inlinedStatement ]
		ifNotEmpty: [
			inlinedStatement
				withIndentationOfLinesAfterTheFirst: implementorIndentation
				replacedBy: aSenderIndentation ]! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements senderCompleteSourceRanges argumentsAndTemporariesReplacements indentation senderIndentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	self calculateReplacementsFrom: aMessageNodeReference usingRanges: senderCompleteSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.
	indentation := self 
		indentationUpTo: aMessageNodeReference completeSourceRange first 
		in: aMessageNodeReference compiledMethod sourceCode.
	senderIndentation := aMessageNodeReference lineIndentation.
	inlinedStatements := statementsToInline size > 1
		ifTrue: [ | lines source |
			lines := statementsToInline allButLast collect: [:aStatementToInline |
				self 
					sourceCodeOfImplementorStatement: aStatementToInline 
					withReplacements: argumentsAndTemporariesReplacements
					removingReturn: removeReturn
					indentedAs: senderIndentation].
			source := lines reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
					aStatementWithReplacements, String newLineString, 
					indentation, nextStatementWithReplacements].
			source, String newLineString, indentation]
		ifFalse: [''].
	inlinedStatements := inlinedStatements, (self 
		sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements 
		removingReturn: removeReturn
		indentedAs: senderIndentation).

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference usedInNode: aParseNode

	| inlinedStatements argumentsAndTemporariesReplacements senderCompleteSourceRanges usageNodeSourceRange
	 inlinedUsageStatement statementsWithReplacements indentation senderIndentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	usageNodeSourceRange := (self findRangesOf: aParseNode in: senderCompleteSourceRanges) first.
	self 
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.
	senderIndentation := aMessageNodeReference lineIndentation.

	statementsWithReplacements := statementsToInline allButLast collect: [:aStatementToInline |
		self 
			sourceCodeOfImplementorStatement: aStatementToInline 
			withReplacements: argumentsAndTemporariesReplacements 
			removingReturn: true
			indentedAs: senderIndentation].
	statementsWithReplacements add: (self 
		sourceCodeOfImplementorStatement: statementsToInline last
		withReplacements: argumentsAndTemporariesReplacements 
		removingReturn: true
		indentedAs: senderIndentation).

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

!methodRemoval: InlineMethod #sourceCodeOfImplementorStatement:withReplacements:removingReturn: stamp: 'HAW 12/Sep/2026, 8:54:23 pm (UTC)'!
InlineMethod removeSelector: #sourceCodeOfImplementorStatement:withReplacements:removingReturn:!
