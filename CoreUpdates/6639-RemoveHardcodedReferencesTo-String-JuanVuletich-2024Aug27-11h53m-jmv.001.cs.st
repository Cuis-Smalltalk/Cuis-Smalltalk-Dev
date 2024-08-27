'From Cuis7.1 [latest update: #6638] on 27 August 2024 at 11:58:30 am'!

!String methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:24:10'!
asHex

	^self species streamContents: [ :strm |
		self do: [ :ch | strm nextPutAll: ch hex ] separatedBy: [ strm space ]].! !


!String class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:27:36'!
fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
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
			strm position: strm position - 1 ]].! !

!String class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:34:17'!
percentEscapingNonAscii: aByteArray
	"Percent encode any byte not in the ASCII range:
	replace it with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	"

	^self percentEscapingNonAsciiAndThoseIn: nil bytes: aByteArray! !

!String class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:27:32'!
percentEscapingNonAsciiAndThoseIn: aCharacterMapByteArray bytes: aByteArray
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
				ifFalse: [ strm nextPut: (Character codePoint: byte) ]]].! !


!Symbol methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:29:50'!
asPlainString
	"Answer a plain String, not a Symbol."
	| newString |
	newString := self species new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString! !

!Symbol methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:25:02'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !

