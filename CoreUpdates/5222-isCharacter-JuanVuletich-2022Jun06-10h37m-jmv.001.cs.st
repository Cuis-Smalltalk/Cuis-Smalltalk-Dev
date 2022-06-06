'From Cuis 6.0 [latest update: #5220] on 6 June 2022 at 10:42:16 am'!

!Object methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:40:12'!
isCharacter
	"Overridden to return true in Character and UnicodeCodePoint."
	^ false! !


!Character methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:40:24'!
isCharacter
	^true! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:40:28'!
isCharacter
	^true! !


!Character methodsFor: 'comparing' stamp: 'jmv 6/6/2022 10:40:45'!
= aCodePointOrCharacter

	self == aCodePointOrCharacter ifTrue: [ ^true ].
	aCodePointOrCharacter isCharacter ifFalse: [ ^false ].
	^self codePoint = aCodePointOrCharacter codePoint! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 6/6/2022 10:40:49'!
includes: aCharacter
	aCharacter isCharacter ifFalse: [ ^ false ].
	^aCharacter iso8859s15Code ifNotNil: [ :index | (map at: index + 1) > 0 ].! !


!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:40:54'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	
			(aCharacter is: #Character)
				ifFalse: [
					aCharacter asCharacter
						ifNotNil: [ :ch | ^self at: index put: ch ]
						ifNil: [self error: 'String only store code points in the ISO 8859-15 set']]
				ifTrue: [
					index isInteger
						ifTrue: [self errorSubscriptBounds: index]
						ifFalse: [self errorNonIntegerIndex]]]
		ifFalse: [self error: 'Strings only store Characters']! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:40:58'!
findAnySubStr: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min ind |
	min _ self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		delim isCharacter
			ifTrue: [ind _ self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [ind _ self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
			min _ min min: ind].
	^ min! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:41:02'!
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens _ OrderedCollection new.
	separators _ delimiters isCharacter
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: separators startingAt: keyStop.
		keyStop _ self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:41:06'!
indexOf: aCharacter

	aCharacter isCharacter ifFalse: [^ 0].
	^ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: 1.! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:41:10'!
indexOf: aCharacter  startingAt: start

	aCharacter isCharacter ifFalse: [^ 0].
	^ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: start.! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:41:15'!
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter iso8859s15Code inString: self  startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 10:41:19'!
skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| any this ind ii |
	ii _ start-1.
	[(ii _ ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any _ false.
		delimiters do: [ :delim |
			delim isCharacter
				ifTrue: [ (self at: ii) = delim ifTrue: [any _ true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: [ "Here's where the one-off error was."
						ind _ 0.
						this _ true.
						delim do: [ :dd | 
							dd = (self at: ii+ind) ifFalse: [this _ false].
							ind _ ind + 1].
						this ifTrue: [ii _ ii + delim size - 1.  any _ true]]
							ifTrue: [any _ false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1! !


!LiteralNode methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:41:22'!
isConstantCharacter
	^ key isCharacter! !


!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 6/6/2022 10:41:29'!
= aCodePointOrCharacter

	self == aCodePointOrCharacter ifTrue: [ ^true ].
	aCodePointOrCharacter isCharacter ifFalse: [ ^false ].
	^self codePoint = aCodePointOrCharacter codePoint! !


!Utf32String methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 10:41:33'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^Utf32String newFrom: array! !


!Utf8String methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 10:41:37'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^Utf8String newFrom: array! !

!methodRemoval: UnicodeCodePoint #isCharacterLike stamp: 'jmv 6/6/2022 10:41:47'!
UnicodeCodePoint removeSelector: #isCharacterLike!
!methodRemoval: Character #isCharacterLike stamp: 'jmv 6/6/2022 10:41:47'!
Character removeSelector: #isCharacterLike!
!methodRemoval: Object #isCharacterLike stamp: 'jmv 6/6/2022 10:41:46'!
Object removeSelector: #isCharacterLike!
