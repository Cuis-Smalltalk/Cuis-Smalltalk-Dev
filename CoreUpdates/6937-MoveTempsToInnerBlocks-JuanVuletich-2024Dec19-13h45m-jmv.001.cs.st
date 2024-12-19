'From Cuis7.3 [latest update: #6935] on 19 December 2024 at 2:05:04 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 12/19/2024 13:47:41'!
findAnySubStr: delimiters startingAt: start
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min |
	min := self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		| ind |
		ind := delim isCharacter
			ifTrue: [self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
		min := min min: ind].
	^ min! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 12/19/2024 13:49:01'!
skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| ii |
	ii := start-1.
	[(ii := ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		| any |
		any := false.
		delimiters do: [ :delim |
			delim isCharacter
				ifTrue: [ (self at: ii) = delim ifTrue: [any := true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: [
						| ind this |
						ind := 0.
						this := true.
						delim do: [ :dd | 
							dd = (self at: ii+ind) ifFalse: [this := false].
							ind := ind + 1].
						this ifTrue: [ii := ii + delim size - 1.  any := true]]
							ifTrue: [any := false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 12/19/2024 13:49:31'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by any of the elements of separators.
	Answer will not include empty strings
	'Hello World' subStrings: ' '
	'1Hello12World1This1211Are31Words1' subStrings: '123'
	'1Hello12World1This1211Are31Words1' subStrings: #($1 $2 $3)
	"

	| delimiterCharacters |
	delimiterCharacters := separators asArray.
	^Array streamContents: [ :answer |
		| out |
		out := self species writeStream.
		self do: [ :char |
			(delimiterCharacters identityIncludes: char)
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					out isEmpty ifFalse: [
						answer nextPut: out contents.
						out := self species writeStream ]]].
		out isEmpty ifFalse: [
			answer nextPut: out contents ]].! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 12/19/2024 13:49:42'!
substringsSeparatedBy: delimiterCharacter
	"Find substrings delimited by aCharacter.
	Answer may include empty strings
	'0063;LATIN SMALL LETTER C;Ll;0;L;;;;;N;;;0043;;0043' substringsSeparatedBy: $;
	"

	^Array streamContents: [ :answer |
		| out |
		out := self species writeStream.
		self do: [ :char |
			char = delimiterCharacter
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					answer nextPut: out contents.
					out := self species writeStream ]].
		answer nextPut: out contents.
		out := nil ].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:50:36'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in |
	in := self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek = $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [
					| char |
					(char := in next) = $&
					ifTrue: [
						| rest did |
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

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:51:11'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| keywords |
	keywords := Array streamContents: [ :kwds |
		| kwd isAllValidInIdentifiers |
		kwd := self species writeStream.
		isAllValidInIdentifiers := true.
		1
			to: self size
			do: [ :i |
				| char |
				char := self at: i.
				kwd nextPut: char.
				char = $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers := true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers := false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:51:31'!
withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed as a single space."

	| trimmed |
	trimmed := self withBlanksTrimmed.
	^self species streamContents: [ :stream |
		| lastWasBlank |
		lastWasBlank := false.
		trimmed do: [ :c |
			c isSeparator
				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]
				ifFalse: [ stream nextPut: c ].
			lastWasBlank := c isSeparator ]].

	"
	' abc  d   ' withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:51:42'!
withDoubleColonsUndoubled
	"Do the inverse of #withDoubleColonsDoubled"

	^self species streamContents: [ :stream |
		| lastWasDoubleColon |
		lastWasDoubleColon := false.
		self do: [ :c |
			c = $"
				ifTrue: [
					lastWasDoubleColon
						ifFalse: [
							stream nextPut: c.
							lastWasDoubleColon := true ]
						ifTrue: [
							lastWasDoubleColon := false]]
				ifFalse: [
					stream nextPut: c.
					lastWasDoubleColon := false ]]].! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 12/19/2024 13:52:05'!
correctAgainstEnumerator: wordBlock continuedFrom: oldCollection
	"The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it."

	| choices scoreMin results maxChoices |
	scoreMin := self size // 2 min: 3.
	maxChoices := 10.
	choices := oldCollection
		ifNil: [ SortedCollection sortBlock: [ :x :y | x value > y value ] ].
	wordBlock
		ifNil: [
			results := OrderedCollection new.
			1 to: (maxChoices min: choices size) do: [ :i | results add: (choices at: i) key ] ]
		ifNotNil: [
			wordBlock value: [ :word |
				| score |
				(score := self alike: word) >= scoreMin ifTrue: [
					choices add: (Association key: word value: score).
						(choices size >= maxChoices) ifTrue: [ scoreMin := (choices at: maxChoices) value] ] ].
			results := choices ].
	^ results! !

!CharacterSequence methodsFor: 'formatting' stamp: 'jmv 12/19/2024 13:52:18'!
format: aCollection
	"format the receiver with aCollection  
	 
	simplest example:
	'foo {1} bar' format: {Date today}.
	 
	complete example:
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}..
	"
	^self species streamContents: [ :out |
		| stream |
		stream := self readStream.
		[stream atEnd]
			whileFalse: [| currentChar | 
				currentChar := stream next.
				currentChar = ${
					ifTrue: [| expression | 
						expression := self getEnclosedExpressionFrom: stream.
						out nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
					ifFalse: [
						currentChar = $\
							ifTrue: [stream atEnd ifFalse: [out nextPut: stream next]]
							ifFalse: [out nextPut: currentChar]]]].! !


!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 12/19/2024 13:52:49'!
readFrom: inStream
	"Answer a new String that is determined by reading the stream, inStream.  Embedded
	double quotes become the quote Character."

	^self streamContents: [ :outStream |
		| done |
		"go to first quote"
		inStream skipTo: $'.
		done := false.
		[done or: [inStream atEnd]]
			whileFalse: 
				[ |char|
				char := inStream next.
				char = $'
					ifTrue: 
						[char := inStream next.
						char = $'
							ifTrue: [outStream nextPut: char]
							ifFalse: [done := true]]
					ifFalse: [outStream nextPut: char]]].! !


!Symbol methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:54:20'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		| offs |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !


!UnicodeString methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:57:10'!
from: startIndex to: stopIndex do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex cpIndex |
	byteIndex := self byteIndexAt: startIndex.
	cpIndex := startIndex.
	[ cpIndex <= stopIndex ] whileTrue: [
		| n |
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (Character codePoint: codePoint).
			cpIndex := cpIndex + 1 ].
		byteIndex := byteIndex + n ].! !

!UnicodeString methodsFor: 'private' stamp: 'jmv 12/19/2024 13:57:44'!
computeCountAndIndexFull
	| strideIndexClass |
	count := 0.
	strideIndexClass := bytes size < 256
		ifTrue: [ ByteArray ]
		ifFalse: [ bytes size < 65536
			ifTrue: [ DoubleByteArray ]
			ifFalse: [ bytes size < 4294967296
				ifTrue: [ WordArray ]
				ifFalse: [DoubleWordArray ]]].
	strideIndex := strideIndexClass streamContents: [ :out |
		| byteIndex |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/19/2024 13:59:07'!
basicCanonicalComposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex combining |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				(Character combiningClass: codePoint) = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining := OrderedCollection new ].
				combining add: codePoint ].
			byteIndex := byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/19/2024 13:59:22'!
basicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex |
	byteIndex := 1.
	stopByteIndex := bytes size.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				Character canonicalDecomposition: codePoint on: strm ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/19/2024 14:00:26'!
basicCanonicallyOrdered
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex combining |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				| cls |
				cls := Character combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining := OrderedCollection new ]
					ifFalse: [
						| element |
						element := { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex := byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/19/2024 14:02:07'!
fromBytesStream: aByteStream
	"Handle UTF-8 bytes, but convert invalid UTF-8 assuming it means ISO-8859-1.
	Argument can be, for example, a InflateStream."

	| utf8 codePointCount buffer readBytes leftover byteIndex |
	buffer := ByteArray new: 10000.
	leftover := nil.
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[aByteStream atEnd] whileFalse: [ 
			readBytes := aByteStream nextInto: buffer.
			leftover ifNotNil: [
				readBytes := leftover, readBytes.
				leftover := nil ].
			byteIndex := 1.
			[ byteIndex <= (aByteStream atEnd
						ifFalse: [readBytes size-3] "To be sure we don't break a possible partially read multibyte code point."
						ifTrue: [readBytes size])   						"But do taks _all_ the bytes if they are the last ones."
			] whileTrue: [
				| n |
				n := Character utf8BytesAndCodePointAt: byteIndex in: readBytes
					into: [ :byte1 :byte2 :byte3 :byte4 |
						utf8Stream nextPut: byte1.
						byte2 ifNotNil: [
							utf8Stream nextPut: byte2.
							byte3 ifNotNil: [
								utf8Stream nextPut: byte3.
								byte4 ifNotNil: [
									utf8Stream nextPut: byte4 ]]].
						codePointCount := codePointCount + 1]
					into: nil.
				byteIndex := byteIndex + n ].
			leftover := byteIndex > readBytes size ifFalse: [
				readBytes copyFrom: byteIndex to: readBytes size ].
		].
	].
	^self fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 12/19/2024 13:54:35'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		| offs |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol.! !


!Text methodsFor: 'emphasis' stamp: 'jmv 12/19/2024 14:04:15'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		| font |
		font := defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
		familyName notNil | pointSize notNil ifTrue: [
			| fn ps |
			fn := familyName ifNil: [ defaultFont familyName ].
			ps := pointSize ifNil: [ defaultFont pointSize ].
			(FontFamily familyName: fn pointSize: ps) ifNotNil: [ :f |
				font := f emphasized: defaultFont emphasis]].
		^font emphasized: (defaultFont emphasis bitOr: emphasis) ].! !

