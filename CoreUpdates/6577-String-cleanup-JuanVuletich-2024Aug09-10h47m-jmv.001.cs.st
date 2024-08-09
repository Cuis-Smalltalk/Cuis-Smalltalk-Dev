'From Cuis7.1 [latest update: #6575] on 9 August 2024 at 10:53:08 am'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 8/8/2024 17:07:24'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by any of the elements of separators.
	Answer will not include empty strings
	'Hello World' subStrings: ' '
	'1Hello12World1This1211Are31Words1' subStrings: '123'
	'1Hello12World1This1211Are31Words1' subStrings: #($1 $2 $3)
	"

	| delimiterCharacters out |
	delimiterCharacters := separators asArray.
	^Array streamContents: [ :answer |
		out := self species writeStream.
		self do: [ :char |
			(delimiterCharacters identityIncludes:			char)
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					out isEmpty ifFalse: [
						answer nextPut: out contents.
						out := self species writeStream ]]].
		out isEmpty ifFalse: [
			answer nextPut: out contents ]].! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 8/8/2024 17:07:57'!
substringsSeparatedBy: delimiterCharacter
	"Find substrings delimited by aCharacter.
	Answer may include empty strings
	'0063;LATIN SMALL LETTER C;Ll;0;L;;;;;N;;;0043;;0043' substringsSeparatedBy: $;
	"

	| out |
	^Array streamContents: [ :answer |
		out := self species writeStream.
		self do: [ :char |
			char = delimiterCharacter
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					answer nextPut: out contents.
					out := self species writeStream ]].
		answer nextPut: out contents.
		out := nil ].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 8/8/2024 17:05:51'!
asCamelCase
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	"
	^ self species streamContents: [ :outStream | | capitalize wroteSome |
		wroteSome := false.
		capitalize := false.
		self do: [ :each | | c |
			c := each.
			c isSeparator
				ifTrue: [ capitalize := true ]
				ifFalse: [
					capitalize & wroteSome ifTrue: [ c := c asUppercase ].
					outStream nextPut: c.
					wroteSome := true.
					capitalize := false ]]].! !


!String methodsFor: 'converting' stamp: 'jmv 8/8/2024 17:10:33'!
asCodePoints
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin-1 (ISO 8859-1).
	Answer a WordArray."
	
	^WordArray streamContents: [ :outStream |
		self do: [ :c |
			outStream nextPut: c codePoint ]].! !

!String methodsFor: 'converting' stamp: 'jmv 8/8/2024 17:11:12'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin-1 (ISO 8859-1)
	Answer a ByteArray with a valid UTF-8 sequence."

	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		self do: [ :c |
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: c codePoint ]].! !

!String methodsFor: 'converting' stamp: 'jmv 8/8/2024 17:11:54'!
asUtf8BytesOrByteString
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
				withUtf8BytesOf: c codePoint ]].! !

