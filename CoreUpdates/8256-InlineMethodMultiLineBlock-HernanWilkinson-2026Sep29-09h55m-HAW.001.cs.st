'From Cuis7.9 [latest update: #8184] on 29 September 2026 at 10:25:20 am'!

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
implementorStartOf: aStatement

	^(self implementorRangesOf: aStatement) anyOne first! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
areInDifferentLines: aCollectionOfStatements

	| sourceCode |

	sourceCode := methodToInline sourceCode
		copyFrom: (self implementorStartOf: aCollectionOfStatements first)
		to: (self implementorStartOf: aCollectionOfStatements last).

	^sourceCode includes: Character newLineCharacter! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:09:48 pm (UTC)'!
indentationOfBlockEvaluating: aCollectionOfStatements inlinedIn: aMessageNodeReference

	^(self areInDifferentLines: aCollectionOfStatements)
		ifTrue: [ aMessageNodeReference lineIndentation , String tab ]
		ifFalse: [ '' ]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
separatorOfBlockEvaluating: aCollectionOfStatements inlinedIn: aMessageNodeReference

	| indentation |

	indentation := self
		indentationOfBlockEvaluating: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.

	^indentation
		ifEmpty: [ ' ' ]
		ifNotEmpty: [ String newLineString , indentation ]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:09:48 pm (UTC)'!
sourceCodeOf: aCollectionOfStatements replacedFor: aMessageNodeReference indentedAs: anIndentation

	| replacements |

	replacements := self replacementsMapForMessageSend: aMessageNodeReference.

	^aCollectionOfStatements collect: [ :aStatementToInline |
		self
			sourceCodeOfImplementorStatement: aStatementToInline
			withReplacements: replacements
			removingReturn: true
			indentedAs: anIndentation ]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
statementsOfBlockEvaluating: aCollectionOfStatements inlinedIn: aMessageNodeReference

	| indentation |

	indentation := self
		indentationOfBlockEvaluating: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.

	^self
		sourceCodeOf: aCollectionOfStatements
		replacedFor: aMessageNodeReference
		indentedAs: indentation! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 29/Sep/2026, 1:09:48 pm (UTC)'!
temporariesDeclarationOfBlockInlinedIn: aMessageNodeReference

	^(self temporariesOfBlockInlinedIn: aMessageNodeReference)
		ifEmpty: [ '' ]
		ifNotEmpty: [ :temporaryNames | ' | ' , (' ' join: temporaryNames) , ' |' ]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:09:48 pm (UTC)'!
sourceCodeOf: aCollectionOfStatements replacedFor: aMessageNodeReference

	^self
		sourceCodeOf: aCollectionOfStatements
		replacedFor: aMessageNodeReference
		indentedAs: aMessageNodeReference lineIndentation! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
sourceCodeOfBlockEvaluating: aCollectionOfStatements inlinedIn: aMessageNodeReference

	| declaration separator statements |

	declaration := self
		temporariesDeclarationOfBlockInlinedIn: aMessageNodeReference.
	separator := self
		separatorOfBlockEvaluating: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.
	statements := self
		statementsOfBlockEvaluating: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.

	^'[' , declaration , separator ,
		((separator join: statements) withoutPossibleSuffix: '.') , ' ] value'! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange byBlockEvaluating: aCollectionOfStatements

	self
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: aMessageNodeReference methodNode completeSourceRanges.

	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: (self
			sourceCodeOfBlockEvaluating: aCollectionOfStatements
			inlinedIn: aMessageNodeReference)
		withPrecedence: ParseNode unaryPrecedence! !

!methodRemoval: InlineMethod #separatorOfStatementsIndentedAs: stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
InlineMethod removeSelector: #separatorOfStatementsIndentedAs:!
!methodRemoval: InlineMethod #blockDeclaring:evaluating:separatedBy: stamp: 'HAW 29/Sep/2026, 1:10:57 pm (UTC)'!
InlineMethod removeSelector: #blockDeclaring:evaluating:separatedBy:!
