'From Cuis7.9 [latest update: #8178] on 14 September 2026 at 10:52:20 am'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 14/Sep/2026, 1:44:44 pm (UTC)'!
withoutPossibleSuffix: aString
	"
	'Elvis' withoutPossibleSuffix: 'vis'
	'Elvis' withoutPossibleSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self allButLast: aString size ]
		ifFalse: [ self ]! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'jmv 14/Sep/2026, 1:49:20 pm (UTC)'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression withPrecedence: aPrecedence

	| enclosingNodeSourceRange replacement rangeInEnclosingNode expressionToWrite |

	enclosingNodeSourceRange := self
		sourceRangeOf: aParseNode
		enclosing: aMessageNodeReference.
	replacement := ExpressionReplacement
		of: aMessageNodeReference completeSourceRange
		in: aMessageNodeReference methodNode
		by: (anExpression withoutPossibleSuffix: '.')
		withPrecedence: aPrecedence.
	rangeInEnclosingNode := replacement intervalToReplace + (1 - enclosingNodeSourceRange first).
	expressionToWrite := replacement sourceCodeToWrite.
	(aMessageNodeReference isAtTheEndOf: enclosingNodeSourceRange) ifTrue: [
		anExpression last = $. ifTrue: [
			expressionToWrite := expressionToWrite copyWith: $. ]].

	^(aMessageNodeReference sourceCodeAt: enclosingNodeSourceRange)
		copyReplacing: { rangeInEnclosingNode -> expressionToWrite }! !

!methodRemoval: CharacterSequence #withoutTrailingPeriod stamp: 'jmv 14/Sep/2026, 1:52:00 pm (UTC)'!
CharacterSequence removeSelector: #withoutTrailingPeriod!
!methodRemoval: CharacterSequence #trailingPeriod stamp: 'jmv 14/Sep/2026, 1:52:00 pm (UTC)'!
CharacterSequence removeSelector: #trailingPeriod!
