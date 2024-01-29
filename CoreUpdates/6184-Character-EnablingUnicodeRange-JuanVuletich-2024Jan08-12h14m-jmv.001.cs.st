'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 12:24:13 pm'!

!FileList methodsFor: 'private' stamp: 'jmv 1/8/2024 12:14:22'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData firstPartSize |
	firstPartSize := 1e3.
	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		stream useBytes.
		((size := stream size)) > (10*firstPartSize) & brevity
			ifTrue: [ data := stream next: firstPartSize. brevityState := #briefHex ]
			ifFalse: [ data := stream next: size. brevityState := #fullHex ]].

	hexData := String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			loc printOn: s base: 16 length: 8 padded: true.
			s
				space;
				nextPut: $(.
			loc printOn: s base: 10 length: 10 padded: true.
			s
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (Character codePoint: (data at: i)) hex; space ].
			s newLine ]].

	^ acceptedContentsCache := ((size > (10*firstPartSize)) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. size. firstPartSize. hexData}]
		ifFalse: [hexData])! !


!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:14:38'!
asUnaccented
	"
	$A asUnaccented
	$Á asUnaccented
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented  asLowercase]
	"
	^ Character
		codePoint: (self class unaccentedTable at: self numericValue + 1)! !

!Character methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:27'!
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self numericValue to: other numericValue) collect:
				[:ascii | Character codePoint: ascii]! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:16:02'!
digitValue: x 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self codePoint: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:16:43'!
separators
self flag: #unicodeNeedsImprovement.
	"Also see #isSeparator"
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [ :v | Character codePoint: v]

	
! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:15:30'!
backspace
	"Answer the Character representing a backspace."

	^self codePoint: 8! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:15:58'!
delete
	^ self codePoint: 127! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:06'!
end
	^ self codePoint: 4! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:12'!
enter
	"Answer the Character representing enter."

	^self codePoint: 3! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:15'!
escape
	"Answer the ASCII ESC character"

	^self codePoint: 27! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:18'!
home
	^ self codePoint: 1! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:22'!
insert
	^ self codePoint: 5! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:26'!
newPage
	"Answer the Character representing a form feed."

	^self codePoint: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:30'!
null
	"Answer the null Character."

	^self codePoint: 0! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:33'!
pageDown
	^ self codePoint: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:36'!
pageUp
	^ self codePoint: 11! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:16:49'!
shortUnderscore
	"Answer the Character representing very short (or invisible) underscore.
	Used to optionally mark subscript in code."

	^ Character codePoint: 127! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:20:40'!
smalltalkDownArrow
	"
	The use of code 31 for a down arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a down arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkDownArrow
	"
	^ Character codePoint: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:20:49'!
smalltalkLeftArrow
	"
	The use of code 28 for a left arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.

	Cuis holds a left arrow glyph at this code, that is only used to show $_ as a left arrow,
	when used for assignment of a Smalltalk variable.
	Note that it will be replaced by $_ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkLeftArrow
	"
	^ Character codePoint: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:20:56'!
smalltalkRightArrow
	"
	The use of code 30 for a right arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkRightArrow
	"
	^ Character codePoint: 30! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 1/8/2024 12:21:08'!
smalltalkUpArrow
	"
	The use of code 29 for an up arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.

	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkUpArrow
	"
	^ Character codePoint: 29! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:15:43'!
basicLookupTables
	| lowercaseMappingTable uppercaseMappingTable unaccentedTable |
	lowercaseMappingTable := Array new: 256.
	uppercaseMappingTable := Array new: 256.
	unaccentedTable := ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char := self codePoint: idx.
			lowercaseMappingTable
				at: idx + 1 put: char.
			uppercaseMappingTable
				at: idx + 1 put: char.
			unaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase := group first numericValue.
				lowercase := group second numericValue.
				lowercaseChar := self codePoint: lowercase.
				uppercaseChar := self codePoint: uppercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar;
					at: uppercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: uppercaseChar;
					at: uppercase + 1 put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase := group third numericValue.
						unaccentedLowercase := group fourth numericValue.
						unaccentedTable
							at: uppercase+1 put: unaccentedUppercase;
							at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase := group first numericValue.
				lowercaseChar := self codePoint: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar ]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:15:48'!
lowercaseMappingTable
	^`Character basicLookupTables at: 1`! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:15:51'!
unaccentedTable
	^`Character basicLookupTables at: 3`! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:15:53'!
uppercaseMappingTable
	^`Character basicLookupTables at: 2`! !


!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:32'!
asLowercase
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character codePoint: value) asLowercase codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:37'!
asUnaccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character codePoint: value) asUnaccented codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:41'!
asUppercase
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character codePoint: value) asUppercase codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:45'!
isAccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character codePoint: value) isAccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:48'!
isUnaccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character codePoint: value) isUnaccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:51'!
isValidInFilenames
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character codePoint: value) isValidInFilenames ].
	^true! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:54'!
isVowel
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^ (Character codePoint: value) isVowel ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:17:57'!
withDiacriticalMark: anUnicodeCodePoint
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (
			(Character codePoint: value) withDiacriticalMark: anUnicodeCodePoint)
				codePoint ].
	^self! !


!Date methodsFor: 'printing' stamp: 'jmv 1/8/2024 12:18:04'!
printOn: aStream format: formatArray 
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| gregorian twoDigits element monthFormat |
	gregorian := self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
	1 to: 3 do: 
		[ :i | 
			element := formatArray at: i.
			element = 1
				ifTrue: [twoDigits
						ifTrue: [aStream
								nextPutAll: (gregorian first printString
										padded: #left
										to: 2
										with: $0)]
						ifFalse: [gregorian first printOn: aStream]].
			element = 2
				ifTrue: [monthFormat := formatArray at: 5.
					monthFormat = 1
						ifTrue: [twoDigits
								ifTrue: [aStream
										nextPutAll: (gregorian middle printString
												padded: #left
												to: 2
												with: $0)]
								ifFalse: [gregorian middle printOn: aStream]].
					monthFormat = 2
						ifTrue: [aStream
								nextPutAll: ((Month nameOfMonth: gregorian middle)
										copyFrom: 1
										to: 3)].
					monthFormat = 3
						ifTrue: [aStream
								nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6)
							= 1
						ifTrue: [gregorian last printOn: aStream]
						ifFalse: [aStream
								nextPutAll: ((gregorian last \\ 100) printString
										padded: #left
										to: 2
										with: $0)]].
			i < 3
				ifTrue: [(formatArray at: 4)
							~= 0
						ifTrue: [
							aStream nextPut: ((formatArray at: 4) isNumber
								ifTrue: [ Character codePoint: (formatArray at: 4) ]
								ifFalse: [ (formatArray at: 4) ])
							]]]
! !


!InputSensor methodsFor: 'private' stamp: 'jmv 1/8/2024 12:18:08'!
characterForKeycode: keycode
	"Map the given keycode to a Smalltalk character object. Encoding:
		A keycode is 12 bits:   <4 modifer bits><8 bit ISO character>
		Modifier bits are:       <command><option><control><shift>"

	"NOTE: the command and option keys are specific to the Macintosh and may not have equivalents on other platforms."

	keycode ifNil: [ ^nil ].
	^ Character codePoint: (keycode bitAnd: 16rFF)! !


!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 1/8/2024 12:19:44'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd := Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 := eventBuffer third.
			u1 := eventBuffer sixth.
			pressType := eventBuffer fourth.
			typeS := 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS := #keystroke].
			dd >100 ifTrue: ['-' print. dd := 0 ].
			{ k1 < 256 ifTrue: [Character codePoint: k1] ifFalse: [ '[]']. UnicodeCodePoint codePoint: k1. k1. k1 hex. '-----'. 
				UnicodeCodePoint codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 1/8/2024 12:18:15'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in char rest did |
	in := self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek = $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [(char := in next) = $&
					ifTrue: [
						rest := in upTo: $;.
						did := out position.
						rest = 'lt' ifTrue: [out nextPut: $<].
						rest = 'gt' ifTrue: [out nextPut: $>].
						rest = 'amp' ifTrue: [out nextPut: $&].
						rest = 'deg' ifTrue: [out nextPut: $¡].
						rest = 'quot' ifTrue: [out nextPut: $"].
						rest first = $# ifTrue: [ out nextPut: (Character codePoint: rest findPositiveInteger) ].
						did = out position ifTrue: [
							out nextPut: $&; nextPutAll: rest.
							"self error: 'unknown encoded HTML char'."
							"Please add it to this method"]]
					ifFalse: [out nextPut: char]].
			]].! !


!String methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:18:20'!
at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character codePoint: (super at: index)! !

!String methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:18:25'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	

			aCharacter class == UnicodeCodePoint ifTrue: [
				aCharacter codePoint > 255 ifTrue: [self error: 'String only store Latin-1 Characters'].
				^self at: index put: (Character codePoint: aCharacter codePoint) ].

			aCharacter numericValue > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !


!String class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:15:04'!
percentEscapingNonAsciiAndThoseIn: aCharacterMap bytes: aByteArray
	"Percent encode any byte not in the ASCII range.
	Also encode any in aCharacterMap.
	Replace them with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	"

	^String streamContents: [ :strm |
		aByteArray do: [ :byte | | c |
			(byte > 127 or: [
						aCharacterMap isNil or: [
							c := Character codePoint: byte.
							aCharacterMap includes: c ]])
				ifTrue: [
					strm nextPut: $%.
					strm nextPut: (byte // 16) asHexDigit.
					strm nextPut: (byte \\ 16) asHexDigit ]
				ifFalse: [ strm nextPut: c ]]].! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:18:29'!
lowercasingTable
	"a table for translating to lower case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character codePoint: c) asLowercase ])`
! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:18:32'!
tokenishTable
	"a table for testing tokenish (for fast numArgs)"
	^`String withAll: (
		((0 to: 255) collect: [ :c | (Character codePoint: c) ])
			collect: 	[ :c | c tokenish ifTrue: [c] ifFalse: [$~]]	)`.! !

!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 12:18:38'!
uppercasingTable
	"a table for translating to upper case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character codePoint: c) asUppercase ])`
! !


!Base64MimeConverter class methodsFor: 'additional services' stamp: 'jmv 1/8/2024 12:15:13'!
initializeTables

	FromCharTable := Array new: 256.	"nils"
	ToCharTable := Array new: 64.
	($A asciiValue to: $Z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind-1.
		ToCharTable at: ind put: (Character codePoint: ascii) ].
	($a asciiValue to: $z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25.
		ToCharTable at: ind+26 put: (Character codePoint: ascii) ].
	($0 asciiValue to: $9 asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: (Character codePoint: ascii) ].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	! !


!RWBinaryOrTextStream methodsFor: 'writing' stamp: 'jmv 1/8/2024 12:18:41'!
nextPut: charOrByte

	super nextPut: 
		(charOrByte isCharacter
			ifTrue: [ charOrByte ]
			ifFalse: [ Character codePoint: charOrByte ])! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 1/8/2024 12:18:45'!
request: prompt
	| startPos char contents return |
	return := Character codePoint: InputSensor returnKey.
	self
		newLine;
		show: prompt.
	startPos := position.
	[
		[ Sensor keyboardPressed ] whileFalse.
		(char := Sensor keyboard) = return ] whileFalse: [
			char = Character backspace
				ifTrue: [ readLimit := position := position - 1 max: startPos ]
				ifFalse: [ self nextPut: char ].
			self endEntry ].
	contents := self contents.
	^ contents
		copyFrom: startPos + 1
		to: contents size! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 1/8/2024 12:18:49'!
readCharacter
    "PRIVATE -- Read a Character."

    ^ Character codePoint: (byteStream nextUint32BigEndian: true)! !


!Scanner class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 12:19:02'!
initialize
	"
	Scanner initialize.
	"
	self initTypeTable.
	self initializeDoitCharacter.! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 1/8/2024 12:18:55'!
initTypeTable
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character codePoint: index) isLetter ifTrue: [
			newTable at: index put: #xLetter ]].
	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
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
	newTable at: Character smalltalkLeftArrow numericValue put: #leftArrow.
	newTable at: Character smalltalkUpArrow numericValue put: #upArrow.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!TrieNode class methodsFor: 'constants' stamp: 'jmv 1/8/2024 12:19:08'!
characterForLeaf
	"A special character to mean leaf node. Must have lower code than alphabetic chars,
	so in the trie 'car' comes before 'cars'"
	^Character codePoint: 0! !


!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 1/8/2024 12:19:53'!
primLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.
	
	Warning: The 'name' field is an instance of String, but in reality it contains the bytes for its UTF-8 representation.
	For instance, if the real name is 'puño' we'll get  'puño', as
	(String withAll: ('puño' AsUtf8Bytes asArray collect: [ :n | Character codePoint: n ]))        =        'puño'
	Senders MUST do appropriate conversion.
	
	Consider calling #primFixedNameLookupEntryIn:index: instead.
	"

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !


!ByteStringReadAdaptor methodsFor: 'reading' stamp: 'jmv 1/8/2024 12:19:12'!
next
	"Stream bytes as Cuis / ISO 8859-15 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Cuis / ISO-8859-15 byte Characters."

	stream nextByte ifNotNil: [ :byte |
		^Character codePoint: byte ].
	^nil! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 1/8/2024 12:19:19'!
displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern := paragraphStyle listBulletPattern.
	bullet := pattern.
	(i := pattern indexOf: $%) > 0
		ifTrue: [ bullet := bulletNumber printString]
		ifFalse: [
			(i := pattern indexOf: $z) > 0
				ifTrue: [ bullet := String with: (Character codePoint: 96 + bulletNumber) ]
				ifFalse: [
					(i := pattern indexOf: $Z) > 0
						ifTrue: [ bullet := String with: (Character codePoint: 64 + bulletNumber) ]]].
	prefix := 0.
	i > 0 ifTrue: [
		c := pattern at: i.
		j := i.
		s := pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j := j + 1 ].
		j := j - 1.
		bulletSize := j-i+1.
		prefix := bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet := bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet := (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos := paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor! !


!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 1/8/2024 12:15:19'!
stringAt: stringOffset length: byteLength multiByte: aBoolean

	| string index stringLength |
	aBoolean ifFalse:[
		stringLength := byteLength.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character codePoint: (fontData at: index + i - 1))].
		^string
	] ifTrue:[
		"UTF-16BE ???
		https://docs.microsoft.com/en-us/typography/opentype/spec/name#enc3
		This code appears to assume only lower byte is needed, most likely only ascii is supported here (jmv)"
		stringLength := byteLength // 2.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character codePoint: (fontData at: index + 1)).
			index := index + 2].
		^string]! !

!methodRemoval: Character class #numericValue: stamp: 'jmv 1/8/2024 12:21:11'!
Character class removeSelector: #numericValue:!
!methodRemoval: Character class #asciiValue: stamp: 'jmv 1/8/2024 12:15:24'!
Character class removeSelector: #asciiValue:!
Scanner initialize!
