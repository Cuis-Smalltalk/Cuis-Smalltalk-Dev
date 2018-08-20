'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 8:32:22 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:30:00'!
addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments 
		
	self advanceWithRangeDo: [ :keywordAsString :range |
		selector nextPutAll: keywordAsString.
		keywordRanges add: range ].
			
	self argumentNameWithRangeDo: [ :argName :range |
		arguments addLast: (encoder bindArg: argName range: range)]
! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:25:57'!
binaryPattern

	^ self advanceWithRangeDo: [ :selectorAsString :selectorRange | | arguments |
		self argumentNameWithRangeDo: [ :argumentName :argumentRange |
		arguments _ Array with: (encoder bindArg: argumentName range: argumentRange).
		{selectorAsString asSymbol. arguments. 2. {selectorRange}}]]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:31:37'!
doitPatternInContext: context

	^context 
		ifNil: [{#DoIt. {}. 1. nil }]
		ifNotNil: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:29:32'!
keywordPattern

	| keywordRanges selector arguments |
	
	selector := WriteStream on: (String new: 32).
	arguments := OrderedCollection new.
	keywordRanges := OrderedCollection new.
	
	[hereType == #keyword] whileTrue:[ 
		self addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments ].
		
	^ {selector contents asSymbol. arguments. 3. keywordRanges}
		! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:23:06'!
unaryPattern

	^ self advanceWithRangeDo: [ :sel :range | {sel asSymbol. {}. 1. {range}} ]! !


!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:21:50'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	| args selector |
	
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1. nil }]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]].

	hereType == #word 
		ifTrue: [^ self advanceWithRangeDo: [ :sel :range | {sel asSymbol. {}. 1. {range}} ]].

	(hereType == #verticalBar or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [ | selectorRange |
		self advanceWithRangeDo: [ :sel :range |
			selector _ sel asSymbol.
			selectorRange _ range ].

		^ self argumentNameWithRangeDo: [ :argName :range |
			args _ Array with: (encoder bindArg: argName range: range).
			{selector. args. 2. {selectorRange}}]].

	hereType == #keyword ifTrue: [ | keywordRanges |
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		keywordRanges := OrderedCollection new.
		[hereType == #keyword] whileTrue:[ 
			self advanceWithRangeDo: [ :keywordAsString :range |
				selector nextPutAll: keywordAsString.
				keywordRanges add: range ].
			
			self argumentNameWithRangeDo: [ :argName :range |
				args addLast: (encoder bindArg: argName range: range)].
		].
		^ {selector contents asSymbol. args. 3. keywordRanges}]. 
	^self expected: 'Message pattern'
! !

