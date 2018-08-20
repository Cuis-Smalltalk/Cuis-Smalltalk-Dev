'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 16 August 2018 at 6:56:22 am'!

!Scanner methodsFor: 'expression types' stamp: 'HAW 8/16/2018 06:52:54'!
scanAllTokenPositionsInto: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments."

	| lastMark |
	lastMark := 1.
	[currentComment ifNotNil:
		[currentComment do:
			[:cmnt| | idx |
			 idx := source originalContents indexOfSubCollection: cmnt startingAt: lastMark.
			 (idx > 0 and: [idx < mark]) ifTrue:
				[aBlock value: idx - 1 value: (lastMark := idx + cmnt size)]].
		 currentComment := nil].
	mark ifNotNil:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue:
			[| savedMark |
			 savedMark := mark.
			 self scanToken.
			 token := token negated.
			 mark := savedMark].
		"Compensate for the fact that the parser uses two character lookahead.  Normally we must
		  remove the extra two characters.  But this mustn't happen for the last token at the end of stream."
		 aBlock
			value: mark
			value: (source position - (aheadChar = self class doItCharacterValue ifTrue: [hereChar = self doItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2]))].
	 (tokenType == #rightParenthesis
	  or: [tokenType == #doIt]) ifTrue:
		[^self].
	tokenType == #leftParenthesis
		ifTrue: 
			[self scanToken; scanAllTokenPositionsInto: aBlock]
		ifFalse: 
			[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
				ifTrue: 
					[self scanLitWord.
					 token == #true ifTrue: [token := true].
					 token == #false ifTrue: [token := false].
					 token == #nil ifTrue: [token := nil]]
				ifFalse:
					[(token == #- 
					  and: [(self typeTableAt: hereChar) == #xDigit])
						ifTrue: 
							[self scanToken.
							 token := token negated]]].
		self scanToken ] repeat! !

!Scanner methodsFor: 'expression types' stamp: 'HAW 8/16/2018 06:50:46'!
step

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := self doItCharacter "doit"]
		ifFalse: [aheadChar := source next].
	^c! !

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 8/16/2018 06:50:12'!
readUpToNextBacktick
	
	self step.
	buffer reset.
	
	[self isAtBacktick]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = self doItCharacter and: [source atEnd]) ifTrue: [^false]].
	self step.
	
	^true! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 06:51:11'!
xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar = self doItCharacter and: [source atEnd
			and:  [source skip: -1. source next ~= self doItCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 06:51:33'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := self doItCharacter.
    aStream := WriteStream on: (String new: 200).
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar == stopChar and: [source atEnd])
                ifTrue: [^self offEnd: 'Unmatched comment quote'].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 06:51:48'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [self doItCharacter "doit"]
							ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 06:52:09'!
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar == $' 
		and: [aheadChar == $' 
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar = self doItCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched string quote']].
	self step.
	token := buffer contents.
	tokenType := #string! !


!Scanner class methodsFor: 'cached class state' stamp: 'HAW 8/16/2018 06:52:18'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±×÷¬­¯,<=>”•–—˜™š›œ«»?@~€‚ƒ‘’“Ÿ°·' asByteArray put: #xBinary.

	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!Parser methodsFor: 'scanning' stamp: 'HAW 8/16/2018 06:49:54'!
advance
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := source position - (source atEnd ifTrue: [hereChar = self doItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2]).
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this! !

