'From Cuis7.1 [latest update: #6451] on 5 June 2024 at 5:21:51 pm'!

!String class methodsFor: 'instance creation' stamp: 'jmv 6/5/2024 17:20:27'!
percentEscapingNonAsciiAndThoseIn: aCharacterMapByteArray bytes: aByteArray
	"Percent encode any byte not in the ASCII range.
	Also encode any in aCharacterMap.
	Replace them with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	"

	^String streamContents: [ :strm |
		aByteArray do: [ :byte | 
			(byte > 127 or: [
						aCharacterMapByteArray isNil or: [
							(aCharacterMapByteArray at: byte+1) = 1 ]])
				ifTrue: [
					strm nextPut: $%.
					strm nextPut: (byte // 16) asHexDigit.
					strm nextPut: (byte \\ 16) asHexDigit ]
				ifFalse: [ strm nextPut: (Character codePoint: byte) ]]].! !

