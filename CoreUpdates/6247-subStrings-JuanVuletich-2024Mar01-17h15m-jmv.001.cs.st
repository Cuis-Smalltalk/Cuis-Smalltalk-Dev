'From Cuis6.3 [latest update: #6246] on 1 March 2024 at 5:18:39 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 3/1/2024 17:14:14'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by any of the elements of separators.
	Answer will not include empty strings
	'Hello World' subStrings: ' '
	'1Hello12World1This1211Are31Words1' subStrings: '123'
	'1Hello12World1This1211Are31Words1' subStrings: #($1 $2 $3)
	"

	| delimiterCharacters char in out |
	delimiterCharacters := separators asArray.
	^Array streamContents: [ :answer |
		in := self readStream.
		out := self species writeStream.
		[ in atEnd ] whileFalse: [
			char := in next.
			(delimiterCharacters identityIncludes:			char)
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					out isEmpty ifFalse: [
						answer nextPut: out contents.
						out := self species writeStream ]]].
		out isEmpty ifFalse: [
			answer nextPut: out contents ]].! !


!String methodsFor: 'converting' stamp: 'jmv 3/1/2024 16:56:21'!
substrings
	"Answer an array of the substrings that compose the receiver.
	Whitespace characters are considered separators, and not included in the result."

	^String substringsIn: self! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 3/1/2024 17:17:14'!
substrings
	"Answer an array of the substrings that compose the receiver.
	Whitespace characters are considered separators, and not included in the result.

	'  Hello World!! Some  		1234 more  €s ' substrings

	'  Hello World!!     Some  		1234 more  €s 
	for you
	to play with ' substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes ]].! !

