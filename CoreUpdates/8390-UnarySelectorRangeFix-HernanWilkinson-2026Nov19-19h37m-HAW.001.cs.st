'From Cuis7.9 [latest update: #8340] on 19 November 2026 at 10:04:47 pm'!

!Parser methodsFor: 'scanning' stamp: 'HAW 19/Nov/2026, 6:49:34 pm (UTC)'!
startOfLastTokenWrittenAs: aWord

	"A token that is the last character of the source code is scanned as starting one character before,
	so its start is taken from where it ends. See #variable"

	^self endOfLastToken - aWord size + 1! !


!Parser methodsFor: 'expression types' stamp: 'HAW 19/Nov/2026, 6:49:34 pm (UTC)'!
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
					args := {parseNode}.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue:
							[selector := self advance.
							start := self startOfLastTokenWrittenAs: selector.
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

