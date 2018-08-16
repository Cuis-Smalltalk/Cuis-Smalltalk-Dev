'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 7:59:09 pm'!

!Parser methodsFor: 'scanning' stamp: 'HAW 8/11/2018 19:36:55'!
advanceWithRangeDo: aBlock

	| lexema start end |
	
	start := self startOfNextToken + requestorOffset.
	lexema := self advance.
	end _ self endOfLastToken + requestorOffset.
	
	^ aBlock value: lexema value: (start to: end)! !

!Parser methodsFor: 'temps' stamp: 'HAW 8/11/2018 19:45:20'!
bindArg: aName range: aRange

	^ self bindArg: aName! !


!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 19:48:40'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := WriteStream on: (String new: 32).
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
			
			(level >= 2 and: [hereType == #verticalBar or: [hereType == #upArrow]]) ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
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

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 19:58:35'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	| args selector end start argumentName |
	
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1. nil }]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]].

	hereType == #word ifTrue: [
		start _ self startOfNextToken + requestorOffset.
		selector _ self advance asSymbol.
		end _ self endOfLastToken + requestorOffset.
		^ {selector. {}. 1. {start to: end}}].

	(hereType == #verticalBar
			or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [
		start _ self startOfNextToken + requestorOffset.
		selector _ self advance asSymbol.
		end _ self endOfLastToken + requestorOffset.

		start _ self startOfNextToken + requestorOffset.
		argumentName _ self argumentName.
		end _ self endOfLastToken + requestorOffset.
	
		args _ Array with: (encoder bindArg: argumentName range: (start to: end)).
		^ {selector. args. 2. {start to: end}}].

	hereType == #keyword ifTrue: [ | ranges |
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		ranges := OrderedCollection new.
		[hereType == #keyword] whileTrue:[ | keywordAsString  |
			start _ self startOfNextToken + requestorOffset.
			keywordAsString _ self advance.
			end _ self endOfLastToken + requestorOffset.
			ranges add: (start to: end).
			selector nextPutAll: keywordAsString.
			
			start _ self startOfNextToken + requestorOffset.
			argumentName _ self argumentName.
			end _ self endOfLastToken + requestorOffset.
	
			args addLast: (encoder bindArg: argumentName range: (start to: end)).
		].
		^ {selector contents asSymbol. args. 3. ranges}].
	^self expected: 'Message pattern'
! !

