'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 8 January 2019 at 5:13:05 pm'!

!Scanner methodsFor: 'expression types' stamp: 'HAW 1/8/2019 17:08:16'!
step

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := DoItCharacter "doit"]
		ifFalse: [aheadChar := source next].
	^c! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 1/8/2019 17:08:09'!
readUpToNext: aChar ifNotFound: aNotFoundBlock

	self step.
	buffer reset.
	
	[self isAt: aChar]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar == DoItCharacter and: [source atEnd]) ifTrue: [^aNotFoundBlock value ]].
	
	self step.
	token := buffer contents.
	! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 1/8/2019 17:10:01'!
xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar == DoItCharacter and: [source atEnd
			and:  [source skip: -1. source next ~~ DoItCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 1/8/2019 17:10:14'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
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

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 1/8/2019 17:10:25'!
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
							ifTrue: [DoItCharacter "doit"]
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


!Parser methodsFor: 'scanning' stamp: 'HAW 1/8/2019 17:07:55'!
compensateTwoCharacterLookahead

	^source position - (aheadChar == DoItCharacter ifTrue: [hereChar == DoItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2])
	! !

