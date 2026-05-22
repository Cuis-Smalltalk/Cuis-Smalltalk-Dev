"
A String is an indexed collection of Characters. In Cuis, Characters are Unicode Characters, each one associated with a numerical Unicode Code Point. In instances of String, all the Characters must be in the two Unicode blocks ("Basic Latin" and "Latin-1 Supplement", meaning the code point is <= 255.
See also UnicodeString, that doesn't have this limitation.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See UnicodeString.
"
Class {
	#name : 'String',
	#superclass : 'CharacterSequence',
	#category : 'Kernel-Strings',
	#package : 'Kernel',
	#tag : 'Strings'
}

{ #category : 'static state' }
String class >> basicCaseInsensitiveOrder [
	"Only for the first 256 Unicode code points.
	Case insensitive
	Follow #collatesBefore:"

	| sorted answer order |
	sorted := (0 to: 255) asArray collect: [ :i | Character codePoint: i ] ::
		sort: [ :a :b |
			a asLowercase = b asLowercase
				ifTrue: [ a collatesBefore: b ]
				ifFalse: [	 	a asLowercase collatesBefore: b asLowercase ]].
	answer := Array new: 256.
	order := -1.
	sorted do: [ :c |
		c isUppercase ifFalse: [
			answer at: c codePoint + 1 put: (order := order+1)]].
	sorted do: [ :c |
		c isUppercase ifTrue: [
			answer at: c codePoint + 1 put: (answer at: c asLowercase codePoint + 1)]].
	^answer asByteArray.
]

{ #category : 'static state' }
String class >> basicCaseSensitiveOrder [
	"Only for the first 256 Unicode code points.
	Case sensitive
	Follow #collatesBefore:"

	| sorted answer order |
	sorted := (0 to: 255) asArray collect: [ :i | Character codePoint: i ] :: sort: [ :a :b | a collatesBefore: b ].
	answer := Array new: 256.
	order := -1.
	sorted do: [ :c | answer at: c codePoint + 1 put: (order := order+1)].
	order = 255 ifFalse: [self error: 'order problem'].
	^answer asByteArray.
]

{ #category : 'static state' }
String class >> caseInsensitiveOrder [
	^`String basicCaseInsensitiveOrder`.
]

{ #category : 'static state' }
String class >> caseSensitiveOrder [
	^`String basicCaseSensitiveOrder`.
]

{ #category : 'services' }
String class >> compare: string1 with: string2 [
	"Return 1, 2 or 3, if string1 is <, =, or > string2,
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: self caseSensitiveOrder
]

{ #category : 'services' }
String class >> compare: string1 with: string2 collated: order [
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>

	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 := order at: (string1 basicAt: i) + 1.
		c2 := order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].

]

{ #category : 'services' }
String class >> compareIgnoreCase: string1 with: string2 [
	"Return 1, 2 or 3, if string1 is <, =, or > string2, 
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: self caseInsensitiveOrder
]

{ #category : 'instance creation' }
String class >> crString [
	^ self with: Character cr
]

{ #category : 'instance creation' }
String class >> crlfString [
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf

]

{ #category : 'character collation' }
String class >> does: aCharacter caseInsensitiveCollateBefore: otherCharacter [
	"
	String does: $a caseInsensitiveCollateBefore: $b
	String does: $a caseInsensitiveCollateBefore: $B
	String does: $A caseInsensitiveCollateBefore: $b
	"
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseInsensitiveOrder at: aCharacter codePoint + 1) <
		(self caseInsensitiveOrder at: otherCharacter codePoint + 1)
]

{ #category : 'character collation' }
String class >> does: aCharacter collateAfter: otherCharacter [
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseSensitiveOrder at: aCharacter codePoint + 1) >
		(self caseSensitiveOrder at: otherCharacter codePoint + 1)
]

{ #category : 'character collation' }
String class >> does: aCharacter collateBefore: otherCharacter [
	"Only for Latin-1"
	self flag: #unicodeNeedsImprovement.
	^ (self caseSensitiveOrder at: aCharacter codePoint + 1) <
		(self caseSensitiveOrder at: otherCharacter codePoint + 1)
]

{ #category : 'accessing' }
String class >> findString: subString in: body startingAt: start [
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: body startingAt: start matchTable: self caseSensitiveOrder.
]

{ #category : 'services' }
String class >> findStringIgnoreCase: subString in: body startingAt: start [
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.
	Ignore case"

	^ self findSubstring: subString in: body startingAt: start matchTable: self caseInsensitiveOrder.
]

{ #category : 'services' }
String class >> findSubstring: key in: body startingAt: start matchTable: matchTable [
	"Answer the index in the string body at which the substring key first occurs, at or beyond start. The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>

	key isEmpty ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index := 1.
			[(matchTable at: (body basicAt: startIndex+index-1) + 1)
				= (matchTable at: (key basicAt: index) + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index := index+1]].
	^ 0
"
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"
]

{ #category : 'instance creation' }
String class >> fromUtf8Bytes: aByteArray [
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin-1 (ISO 8859-1).
	Discard any code points that can not be represented.
	Use with caution. If in doubt, send this message to CharacterSequence instead."

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self fromUtf8Bytes: aByteArray trimLastNull: false
]

{ #category : 'instance creation' }
String class >> fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar [
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin-1 (ISO 8859-1).
	Discard any code points that can not be represented."

	^self streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					codePoint < 256 ifTrue: [
						strm nextPut: (Character codePoint: codePoint) ]].
			byteIndex := byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].
]

{ #category : 'services' }
String class >> is: aStringOrByteArray equalTo: another [
	"Take advantage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: self caseSensitiveOrder) = 2
]

{ #category : 'services' }
String class >> isAscii: aStringOrByteArray [
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	| nonAsciiBytesSetBA |
	nonAsciiBytesSetBA := `(ByteArray new: 128), ((ByteArray new: 128) + 1)`.
	^ (ByteArray
		findFirstInBytes: aStringOrByteArray
		inSet: nonAsciiBytesSetBA
		startingAt: 1)
			= 0.
]

{ #category : 'instance creation' }
String class >> lfString [
	"Answer a string containing a single Lf character."

	^ self with: Character lf
]

{ #category : 'static state' }
String class >> lowercasingTable [
	"a table for translating to lower case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character codePoint: c) asLowercase ])`

]

{ #category : 'instance creation' }
String class >> new: sizeRequested  [
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  
]

{ #category : 'instance creation' }
String class >> new: size withAll: value [
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value
]

{ #category : 'instance creation' }
String class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray := self new: aCollection size.
	index := 0.
	aCollection do: [ :each | newArray at: (index := index + 1) put: each].
	^ newArray

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: UnicodeString
"
]

{ #category : 'instance creation' }
String class >> newLineString [
	"Answer a string containing a single newLine (i.e. Lf) character."

	^ self with: Character newLineCharacter
]

{ #category : 'instance creation' }
String class >> percentEscapingNonAscii: aByteArray [
	"Percent encode any byte not in the ASCII range:
	replace it with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	"

	^self percentEscapingNonAsciiAndThoseIn: nil bytes: aByteArray
]

{ #category : 'instance creation' }
String class >> percentEscapingNonAsciiAndThoseIn: aCharacterMapByteArray bytes: aByteArray [
	"Percent encode any byte not in the ASCII range.
	Also encode any in aCharacterMap.
	Replace them with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	"

	^self streamContents: [ :strm |
		aByteArray do: [ :byte | 
			(byte > 127 or: [
						aCharacterMapByteArray isNil or: [
							(aCharacterMapByteArray at: byte+1) = 1 ]])
				ifTrue: [
					strm nextPut: $%.
					strm nextPut: (byte // 16) asHexDigit.
					strm nextPut: (byte \\ 16) asHexDigit ]
				ifFalse: [ strm nextPut: (Character codePoint: byte) ]]].
]

{ #category : 'services' }
String class >> string: aString lineIndicesDo: aBlock [
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| charSetLineEnders start end endWithoutDelimiters |
	charSetLineEnders := `(ByteArray new: 256) at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	start := 1.
	[
		end := aString indexOfAnyIn: charSetLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters := end := aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters := end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf asciiValue`
			and: [(aString basicAt: end) = `Character cr asciiValue` ]])
				ifTrue: [ end := end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start := end + 1 ] repeat
]

{ #category : 'services' }
String class >> string: aByteStringOrByteArray withLineEndings: newLineEndingString [
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf charSetLineEnders inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr := Character cr.
	lf := Character lf.
	charSetLineEnders := `(ByteArray new: 256) at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	(aByteStringOrByteArray is: #ByteArray)
		ifTrue: [
			cr := cr asciiValue.
			lf := lf asciiValue ].

	inPos := 1.
	outPos := 1.
	lineEndingSize := newLineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString := aByteStringOrByteArray class new: aByteStringOrByteArray size * lineEndingSize.

	[
		lineEndPos := aByteStringOrByteArray indexOfAnyIn: charSetLineEnders startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aByteStringOrByteArray startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (newLineEndingString at: i).
				newOutPos := newOutPos + 1 ].
			outPos := newOutPos.
			((aByteStringOrByteArray at: lineEndPos) = cr and: [
					lineEndPos < aByteStringOrByteArray size and: [ (aByteStringOrByteArray at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (aByteStringOrByteArray size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aByteStringOrByteArray startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos
]

{ #category : 'services' }
String class >> substringsIn: aByteStringOrByteArray [
	"Answer an array of the substrings that compose the argument.
	If argument is an UTF-8 encoded ByteArray, answer is an array of same."

	| end beginning charSetNonSeparators charSetSeparators |
	charSetSeparators := `(ByteArray new: 256)
		at: Character space asciiValue + 1 put: 1;
		at: Character tab asciiValue + 1 put: 1;
		at: Character lf asciiValue + 1 put: 1;
		at: Character cr asciiValue + 1 put: 1;
		at: Character newPage asciiValue + 1 put: 1;
		yourself`.
	charSetNonSeparators := `(ByteArray new: 256) += 1;
		at: Character space asciiValue + 1 put: 0;
		at: Character tab asciiValue + 1 put: 0;
		at: Character lf asciiValue + 1 put: 0;
		at: Character cr asciiValue + 1 put: 0;
		at: Character newPage asciiValue + 1 put: 0;
		yourself`.
	end := 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning := aByteStringOrByteArray indexOfAnyIn: charSetNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end := aByteStringOrByteArray indexOfAnyIn: charSetSeparators startingAt: beginning ifAbsent: [ aByteStringOrByteArray size + 1 ].
			end := end - 1.
			result nextPut: (aByteStringOrByteArray copyFrom: beginning to: end).
		]
	].
]

{ #category : 'system' }
String class >> symbolClassToUse [
	"Answer the class to create symbols from us"
	^Symbol
]

{ #category : 'instance creation' }
String class >> tab [
	"Answer a string containing a single tab character."

	^ self with: Character tab

]

{ #category : 'static state' }
String class >> tokenishTable [
	"a table for testing tokenish (for fast numArgs)"
	^`String withAll: (
		((0 to: 255) collect: [ :c | (Character codePoint: c) ])
			collect: 	[ :c | c tokenish ifTrue: [c] ifFalse: [$~]]	)`.
]

{ #category : 'primitives' }
String class >> translate: aString from: start to: stop table: table [
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString basicAt: i) +1) ]
]

{ #category : 'services' }
String class >> translate: aStringOrByteArray table: table [
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: table
]

{ #category : 'services' }
String class >> translateToLowercase: aStringOrByteArray [
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: self lowercasingTable
]

{ #category : 'services' }
String class >> translateToUppercase: aStringOrByteArray [
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: self uppercasingTable
]

{ #category : 'static state' }
String class >> uppercasingTable [
	"a table for translating to upper case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character codePoint: c) asUppercase ])`

]

{ #category : 'instance creation' }
String class >> with: aCharacter  [
	"Answer a new instance of me, containing only aCharacter.
	String with: $a
	String with: $á
	String with: Character euro
	"

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: aCharacter.
	^newCollection
]

{ #category : 'instance creation' }
String class >> with: firstChar with: secondChar [
	"Answer a new instance of me, containing firstChar and secondChar.
	String with: $a
	String with: $á with: Character euro
	"

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstChar.
	newCollection at: 2 put: secondChar.
	^newCollection
]

{ #category : 'instance creation' }
String class >> with: firstObject with: secondObject with: thirdObject  [
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection
]

{ #category : 'instance creation' }
String class >> withAll: aSequenceableCollection [
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection
]

{ #category : 'stream creation' }
String class >> writeStream [
	"Generally prefer
		UnicodeString writeStream
	that offers better performance in most cases.
	Only use this form if:
	- Zero or close to zero chance of any Unicode Character (code point > 255)
	- You know what you are doing
	"
	^ WriteStream on: (self new: 100)
]

{ #category : 'copying' }
String >> , aStringOrText [
	^ aStringOrText appendToString: self
]

{ #category : 'comparing' }
String >> < aString [
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Comparing for equality may is faster in most casts. Try it first."
	self = aString ifTrue: [ ^false ].

	"Let them handle us (already knowing we are not equal)"
	aString isByteString ifFalse: [ ^(aString < self) not ].

	^ (String compare: self with: aString) = 1.
]

{ #category : 'comparing' }
String >> = aString [
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isByteString ifFalse: [
		^aString = self ].

	^String is: self equalTo: aString.
]

{ #category : 'copying' }
String >> appendToString: aString [

	| stringSize |
	stringSize := aString size.
	^ aString copyReplaceFrom: stringSize + 1
		 	    to: stringSize
		   	    with: self
]

{ #category : 'converting' }
String >> asByteString [
	"Answer an instance of the Byte oriented String class"
	^self
]

{ #category : 'converting' }
String >> asByteStringIfAscii [
	"Answer an instance of the Byte oriented String class"
	^self
]

{ #category : 'converting' }
String >> asCodePoints [
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin-1 (ISO 8859-1).
	Answer a WordArray."
	
	^WordArray streamContents: [ :outStream |
		self do: [ :c |
			outStream nextPut: c codePoint ]].
]

{ #category : 'converting' }
String >> asHex [

	^self species streamContents: [ :strm |
		self do: [ :ch | strm nextPutAll: ch hex ] separatedBy: [ strm space ]].
]

{ #category : 'converting' }
String >> asLowercase [
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asPlainString translateToLowercase
]

{ #category : 'converting' }
String >> asUppercase [
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^self copy asPlainString translateToUppercase
]

{ #category : 'converting' }
String >> asUtf8Bytes [
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin-1 (ISO 8859-1)
	Answer a ByteArray with a valid UTF-8 sequence."

	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		self do: [ :c |
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: c codePoint ]].
]

{ #category : 'converting' }
String >> asUtf8BytesOrByteString [
	"Similar to #asUtf8Bytes.
	May answer an instance of String as an optimization.
	See senders"

	"Fast lane."
	self isAscii ifTrue: [ ^self ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		self do: [ :c |
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: c codePoint ]].
]

{ #category : 'converting' }
String >> asUtf8BytesOrByteStringOrByteArray [
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self
]

{ #category : 'accessing' }
String >> at: index  [
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character codePoint: (super at: index)
]

{ #category : 'accessing' }
String >> at: index put: aCharacter  [
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	
			aCharacter codePoint > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']
]

{ #category : 'accessing' }
String >> byteSize [
	^self size
]

{ #category : 'testing' }
String >> canHoldAnyCodePoint [
	"Nope.
	We can only hold Code Points less than 256.
	Needs something like UnicodeString for others. Even an Array is better than us."
	^false
]

{ #category : 'converting' }
String >> capitalized [
	"Return a copy with the first letter capitalized"
	| cap |
	self isEmpty ifTrue: [ ^self copy ].
	cap := self copy.
	cap at: 1 put: (cap at: 1) asUppercase.
	^ cap
]

{ #category : 'comparing' }
String >> caseInsensitiveLessOrEqual: aString [
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString isUnicodeString and: [ aString isAscii ]) ifTrue: [
		^(String compareIgnoreCase: self with: aString bytes) <= 2 ].
	"Do conversions if needed"
	aString isByteString ifFalse: [
		^self asUnicodeString caseInsensitiveLessOrEqual: aString ].
	"Quick primitive if both String"
	^ (String compareIgnoreCase: self with: aString) <= 2
]

{ #category : 'enumerating' }
String >> collect: aBlock [
	"Refer to the comment in Collection >> #collect:."
	| array resultClass |
	array := self collectInArray: aBlock.
	resultClass := String.
	array do: [ :element |
		element isCharacter ifFalse: [ ^array ].
		"String can only hold ISO-8859-1 (ASCII + Latin 1 Suplement)"
		element codePoint > 255 ifTrue: [
			resultClass := UnicodeString ]].
	^resultClass newFrom: array
]

{ #category : 'copying' }
String >> copyReplaceFrom: start to: stop with: replacementCollection  [
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| rep |
	rep := replacementCollection string.
	rep isUnicodeString ifTrue: [
		^self asUnicodeString copyReplaceFrom: start to: stop with: rep  ].
	^super copyReplaceFrom: start to: stop with: rep.
]

{ #category : 'copying' }
String >> copyWith: newElement [
	newElement codePoint > 255 ifTrue: [
		^self asUnicodeString copyWith: newElement ].
	^super copyWith: newElement
]

{ #category : 'comparing' }
String >> crc16 [
	"Compute a 16 bit cyclic redundancy check."

	| crc |
	crc := 0.
	self do: [:c |
		crc := (crc bitShift: -8) bitXor: (
		 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
			16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
			16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
			16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
			16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
			16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
			16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
			16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
			16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
			16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
			16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
			16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
			16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
			16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
			16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
			16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
			16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
			16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
			16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
			16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
			16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
			16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
			16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
			16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
			16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
			16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
			16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
			16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
			16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
			16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
			16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
			16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c codePoint) bitAnd: 16rFF) + 1) ].
	^crc
]

{ #category : 'paragraph support' }
String >> encompassLine: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left := (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr := (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	rightNull := (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
]

{ #category : 'paragraph support' }
String >> encompassParagraph: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| left rightCr rightNull |
	left := (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr := (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	rightNull := (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
]

{ #category : 'accessing' }
String >> findString: subString startingAt: start [
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString isUnicodeString and: [subString isAscii]) ifTrue: [
		^ String findString: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^ self asUnicodeString findString: subString startingAt: start ].
	"Quick primitive if both String"
	^ String findString: subString in: self startingAt: start
]

{ #category : 'accessing' }
String >> findStringCaseInsensitive: subString startingAt: start [
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString isUnicodeString and: [ subString isAscii ]) ifTrue: [
		^String findStringIgnoreCase: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^self asUnicodeString findStringCaseInsensitive: subString startingAt: start ].
	"Quick primitive if both String"
	^String findStringIgnoreCase: subString in: self startingAt: start
]

{ #category : 'converting' }
String >> fixLineEndings [
	"Assume that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!
	('Hello ', String crlfString, 'World') fixLineEndings
	('Hello ', String crString, 'World') fixLineEndings
	('Hello ', String lfString, 'World') fixLineEndings
	"
	(self includes: `Character cr`) ifFalse: [ ^self ].
	(self includesSubString: `String crlfString`) ifTrue: [ "A new instance is needed"
		^ self withLineEndings: `String newLineString` ].
	self replaceAll: `Character cr` with: `Character lf`.
]

{ #category : 'comparing' }
String >> hash [
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self size >  ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: String hash ].
	^ByteArray
		hashBytes: self
		startingWith: String hash
]

{ #category : 'accessing' }
String >> indexOf: aCharacter startingAt: start ifAbsent: aBlock [
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	"String can only hold ASCII + Latin1"
	aCharacter codePoint >255 ifTrue: [ ^ aBlock value ].
	ans := ByteArray indexOfByte: aCharacter codePoint inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]
]

{ #category : 'accessing' }
String >> indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock [
	"returns the index of the first character in the given set, starting from start.
	aByteSetByteArray is a ByteArray of size 256 with a 1 for byte values considered to be included."

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aByteSetByteArray startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]
]

{ #category : 'testing' }
String >> is: aSymbol [
	"Note: Senders might prefer #isString for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#String = aSymbol or: [ super is: aSymbol]
]

{ #category : 'comparing' }
String >> is: subString substringAt: index [
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	(subString isAscii not and: [self isAscii]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes := nil.
	subString isByteString ifTrue: [
		subStringBytes := subString ].
	(subString isUnicodeString and: [ subString isAscii ]) ifTrue: [
		"Can use #bytes, because it has just ASCII bytes"
		subStringBytes := subString bytes ].
	subStringBytes ifNotNil: [
		^(String findString: subStringBytes in: self startingAt: index) = index ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to String"
	^super is: subString substringAt: index
]

{ #category : 'testing' }
String >> isAscii [
	"Return true when I am pure 7-bit ASCII.
	This means, for example, that my bytes are also my UTF-8 representation."

	^ String isAscii: self
]

{ #category : 'testing' }
String >> isByteString [
	^ true
]

{ #category : 'testing' }
String >> isEmpty [
	^self size = 0
]

{ #category : 'testing' }
String >> isInCanonicalCompositionForm [

	^true
]

{ #category : 'testing' }
String >> isIntegerDenotation [
	"Answer true if the receiver denotes an integer number."
	
	^(self size > 1 and: [
		self first == $- and: [ (self findFirst: [:each | each isDigit not ] startingAt: 2) = 0 ]
			]) or: [ self size > 0 and: [ self allSatisfy: [:each | each isDigit ] ] ]


]

{ #category : 'accessing' }
String >> isLastLineSeparator [
	^self last isLineSeparator
]

{ #category : 'accessing' }
String >> isLastSeparator [
	^self last isSeparator
]

{ #category : 'printing' }
String >> isLiteral [
	^true
]

{ #category : 'accessing' }
String >> lineIndicesDo: aBlock [
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	String string: self lineIndicesDo: aBlock
]

{ #category : 'services' }
String >> numArgs [
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar := self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(String
			findSubstring: `'~' asByteString`
			in: self
			startingAt: 1
			matchTable: String tokenishTable) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons := 0.
		start := 1.
		[ (ix := self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons := numColons + 1.
			start := ix + 1 ].
		^ numColons ].
	^ -1.
]

{ #category : 'accessing' }
String >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'comparing' }
String >> sameAs: aString [
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString isUnicodeString and: [aString isAscii]) ifTrue: [
		^ (String compareIgnoreCase: self with: aString bytes) = 2 ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (String compareIgnoreCase: self with: aString) = 2
]

{ #category : 'accessing' }
String >> size [
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize
]

{ #category : 'converting' }
String >> substrings [
	"Answer an array of the substrings that compose the receiver.
	Whitespace characters are considered separators, and not included in the result."

	^String substringsIn: self
]

{ #category : 'converting' }
String >> surroundedBySingleQuotes [
	"Answer the receiver with leading and trailing quotes.  "

	^ `(String with: $')`, self, `(String with: $')`
]

{ #category : 'converting' }
String >> thatCanHoldAnyCodePoint [
	"See senders.
	Also see #asStreamResult"
	^ArrayOfCharactersAndCodePoints withAll: self
]

{ #category : 'converting' }
String >> translateToLowercase [
	"Translate all characters to lowercase, in place"

	String translateToLowercase: self
]

{ #category : 'converting' }
String >> translateToUppercase [
	"Translate all characters to lowercase, in place"

	String translateToUppercase: self
]

{ #category : 'converting' }
String >> uncapitalized [
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' uncapitalized
	'mElViN' uncapitalized
	('Will', Random next mantissaPart printString) asSymbol uncapitalized
	('will', Random next mantissaPart printString) asSymbol uncapitalized
	"
	| answer |
	self isEmpty ifTrue: [^ self].
	self first isLowercase ifTrue: [ ^self ].
	answer := self asPlainString.
	answer == self ifTrue: ["don't modify receiver"
		answer := self copy ].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ answer
]

{ #category : 'accessing' }
String >> utf8BytesSize [
	"Avoid creating the actual byte array."

	| byteCount |
	"Fast lane."
	self isAscii ifTrue: [ ^self size ].

	"Need to do UTF-8 encoding."
	byteCount := 0.
	self do: [ :c |
		Character
			evaluate: [ :byte | byteCount := byteCount + 1 ]
			withUtf8BytesOf: c codePoint ].
	^byteCount
]

{ #category : 'converting' }
String >> withLineEndings: lineEndingString [
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	^ String string: self withLineEndings: lineEndingString
]
