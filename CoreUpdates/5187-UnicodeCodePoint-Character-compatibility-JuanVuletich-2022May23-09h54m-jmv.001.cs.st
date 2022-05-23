'From Cuis 6.0 [latest update: #5183] on 23 May 2022 at 10:03:01 am'!

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:56:48'!
findAnySubStr: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min ind |
	min _ self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		delim isCharacterLike
			ifTrue: [ind _ self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [ind _ self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
			min _ min min: ind].
	^ min! !

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:57:00'!
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens _ OrderedCollection new.
	separators _ delimiters isCharacterLike
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: separators startingAt: keyStop.
		keyStop _ self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:57:34'!
indexOf: aCharacter

	aCharacter isCharacterLike ifFalse: [^ 0].
	^ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: 1.! !

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:57:56'!
indexOf: aCharacter  startingAt: start

	aCharacter isCharacterLike ifFalse: [^ 0].
	^ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: start.! !

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:58:17'!
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	aCharacter isCharacterLike ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter iso8859s15Code inString: self  startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:59:24'!
skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| any this ind ii |
	ii _ start-1.
	[(ii _ ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any _ false.
		delimiters do: [ :delim |
			delim isCharacterLike
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

