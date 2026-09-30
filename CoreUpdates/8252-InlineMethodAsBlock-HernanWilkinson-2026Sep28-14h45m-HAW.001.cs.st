'From Cuis7.9 [latest update: #8184] on 28 September 2026 at 4:50:43 pm'!

!MessageNodeReference methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 5:50:06 pm (UTC)'!
isAt: aSourceRange of: aMethodReference

	^self compiledMethod methodReference = aMethodReference
		and: [ aSourceRange includesAllOf: completeSourceRange ]! !


!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:49:10 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange by: aCollectionOfInlinedStatements lastWithPrecedence: aPrecedence

	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: aCollectionOfInlinedStatements last
		withPrecedence: aPrecedence.
	self
		insert: aCollectionOfInlinedStatements allButLast
		before: aStatementSourceRange
		in: aMessageNodeReference compiledMethod! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:49:33 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange byBlockEvaluating: aCollectionOfInlinedStatements

	| statements |

	statements := aCollectionOfInlinedStatements allButLast
		copyWith: (aCollectionOfInlinedStatements last withoutPossibleSuffix: '.').

	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: '[ ' , (' ' join: statements) , ' ] value'
		withPrecedence: ParseNode unaryPrecedence! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:50:06 pm (UTC)'!
sendsToInlineAt: aStatementSourceRange of: aMessageNodeReference

	| sender |

	sender := aMessageNodeReference compiledMethod methodReference.

	^messageSendsToInline select: [ :aMessageSend |
		aMessageSend isAt: aStatementSourceRange of: sender ]! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:50:38 pm (UTC)'!
shouldEvaluateInBlock: aCollectionOfInlinedStatements at: aStatementSourceRange of: aMessageNodeReference

	| sendsInStatement |

	sendsInStatement := self
		sendsToInlineAt: aStatementSourceRange
		of: aMessageNodeReference.

	^aCollectionOfInlinedStatements size > 1 and: [ sendsInStatement size > 1 ]! !


!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:50:38 pm (UTC)'!
inlineStatements: aCollectionOfStatements ofMessageWithReturnValue: aMessageNodeReference usedInStatementAt: aStatementSourceRange

	| inlinedStatements evaluatesInBlock |

	inlinedStatements := self
		sourceCodeOf: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.
	evaluatesInBlock := self
		shouldEvaluateInBlock: inlinedStatements
		at: aStatementSourceRange
		of: aMessageNodeReference.

	evaluatesInBlock
		ifTrue: [
			self
				replace: aMessageNodeReference
				usedInStatementAt: aStatementSourceRange
				byBlockEvaluating: inlinedStatements ]
		ifFalse: [
			self
				replace: aMessageNodeReference
				usedInStatementAt: aStatementSourceRange
				by: inlinedStatements
				lastWithPrecedence: aCollectionOfStatements last expr expressionPrecedence ]! !

!methodRemoval: InlineMethod #hasSeveralSendsToInlineAt:of: stamp: 'HAW 28/Sep/2026, 5:50:07 pm (UTC)'!
InlineMethod removeSelector: #hasSeveralSendsToInlineAt:of:!
