'From Cuis 6.0 [latest update: #5107] on 21 April 2022 at 2:27:53 pm'!

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/21/2022 12:17:39'!
readFrom: aStream

	| offset date time ch |

	date _ Date readFrom: aStream.
	[aStream peek isDigit]
		whileFalse: [aStream next].
	time _ Time readFrom: aStream.
	aStream atEnd
		ifTrue: [ offset _ self localOffset ]
		ifFalse: [
			ch _ aStream next.
			ch = $+ ifTrue: [ch _ Character space].
			offset _ Duration fromString: (String with: ch), '0:', aStream upToEnd, ':0'].
		
	^ self
		year: date yearNumber
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset


	"
		DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.02+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.003+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0004+01:01' readStream
  		DateAndTime readFrom: ' 2002-05-16T17:20:45.00005' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000006+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000007+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.00000008-01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000000009+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000000001+01:01' readStream

 		DateAndTime readFrom: ' 2002-05-16T17:20' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45+01:57' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45-02:34' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45+00:00' readStream
		DateAndTime readFrom: ' 1997-04-26T01:02:03+01:02:3' readStream
 	"! !


!Character methodsFor: 'converting' stamp: 'jmv 4/21/2022 12:18:12'!
asText
	^ (String with: self) asText! !


!EventSensor methodsFor: 'test' stamp: 'jmv 4/21/2022 12:19:09'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			Transcript
				newLine;
				show: type;
				show: ' macRomanCode:', macRomanCode printString, '-', 
					(Character numericValue: (Character macRomanToLatin1: macRomanCode)) asString, '-';
				show: ' unicodeCodePoint:', unicodeCodePoint printString.
			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |
				Transcript show: '-', (String with: (Character numericValue: latin15)), '-' ].
			Transcript
				show: ' modifiers:', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].
			].! !


!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 12:19:48'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did inString |

	"Hack in some minimal workaround for Unicode stuff"
	inString _ self copyReplaceAll: 'â€™' with: (String with: $').
	"Check if we can handle this in #safeValue: in some way..."
	inString = self ifFalse: [ self halt ].
	
	in _ ReadStream on: inString.
	out _ WriteStream on: (String new: self size).
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
						ifTrue: [rest _ in upTo: $;.
								did _ out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $¡].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest findPositiveInteger) ].
								did = out position ifTrue: [
									out nextPut: $&; nextPutAll: rest.
									"self error: 'unknown encoded HTML char'."
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents! !

!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 12:20:42'!
surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ `(String with: $')`, self, `(String with: $')`! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 4/21/2022 12:22:42'!
internCharacter: aCharacter

	OneCharacterSymbols ifNil: [^self intern: (String with: aCharacter)].

	^OneCharacterSymbols at: aCharacter numericValue + 1
! !


!Text methodsFor: 'converting' stamp: 'jmv 4/21/2022 13:32:02'!
withCuisLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>"

	| newText wrongLineEnd |
	wrongLineEnd _ String crlfString detect: [ :char | (char = Character newLineCharacter) not ].
	(string includes: wrongLineEnd) ifFalse: [ ^self copy ].
	newText _ self copyReplaceAll: String crlfString with: String newLineString asTokens: false.
	(newText asString includes: wrongLineEnd) ifFalse: [ ^newText ].
	^newText copyReplaceAll: (String with: wrongLineEnd) with: String newLineString asTokens: false.! !


!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 4/21/2022 14:10:29'!
nextPut: aCharacter
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: (String with: aCharacter).
	^aCharacter! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/21/2022 11:50:00'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self addString: (String with: aKeyboardEvent keyCharacter).
	^false! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/21/2022 12:11:06'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	| rpt |
	self
		evaluateSelectionAndDo: [ :result |
			rpt _ model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			rpt _ rpt copyReplaceAll: String lfString with: String lfString, ' '.
			self afterSelectionInsertAndSelect: 
				((' ', rpt, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false.! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/21/2022 11:50:34'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| stopIndex startIndex string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	stopIndex _ self pointIndex - 1.
	(stopIndex between: 2 and: string size)
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	startIndex _ stopIndex.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	[startIndex > 0 and: [stopIndex - startIndex < 7 and: [(string at: startIndex) isLetter]]] whileTrue: [startIndex _ startIndex - 1].
	(startIndex > 0 and: [(string at: startIndex) = $\])
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	key _ string copyFrom: startIndex+1 to: stopIndex.
	(Character namedCharactersTable detect: [:one| key = one first] ifNone: [])
		ifNotNil: [:aPair| self selectFrom: startIndex to: stopIndex; replaceSelectionWith: (String with: aPair second)].
	^ super normalCharacter: aKeyboardEvent! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 4/21/2022 12:30:58'!
displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber printString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ String with: (Character numericValue: 96 + bulletNumber) ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ String with: (Character numericValue: 64 + bulletNumber) ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 4/21/2022 11:36:42'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := String with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (String with: d).
			self nextChar.
			^currentToken].
	(c == $| and: [d == $|])
		ifTrue: ["|| empty temp declaration"
			^currentToken].
	c _ d.
	[
		d _ self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken! !

