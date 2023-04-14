'From Cuis 6.0 [latest update: #5764] on 14 April 2023 at 12:42:37 pm'!

!String methodsFor: 'converting' stamp: 'jmv 4/13/2023 14:40:50'!
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.
	This is the opposite of #percentEscapingNonAscii: and #percentEscapingNonAsciiAndThoseIn:bytes:
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	'%E2%8A%99' unescapePercents
	"

	| myBytes answerClass answerBytes c decodedByte pos oldPos specialChars |
	myBytes := self asByteArray.
	answerClass := String.
	answerBytes := ByteArray streamContents: [ :byteStream |
		oldPos := 1.
		specialChars := `'+%' asCharacterSet`.

		[pos := myBytes indexOfAnyOf: specialChars startingAt: oldPos ifAbsent: [ 0 ]. pos > 0 ]
		whileTrue: [
			byteStream nextPutAll: (myBytes copyFrom: oldPos to: pos - 1).
			c := self at: pos.
			c = $+ ifTrue: [byteStream nextPut: `$  asciiValue` ] ifFalse: [
				(c = $% and: [pos + 2 <= self size]) ifTrue: [
					decodedByte := (self at: pos+1) asUppercase digitValue * 16 +
						(self at: pos+2) asUppercase digitValue.
					pos := pos + 2.
					decodedByte > 255 ifTrue: [^self].	"not really an escaped string"
					decodedByte > 127 ifTrue: [ answerClass := UnicodeString ].
					byteStream nextPut: decodedByte ]
				ifFalse: [byteStream nextPut: c numericValue ]].
			oldPos := pos+1].
		byteStream nextPutAll: (myBytes copyFrom: oldPos to: self size).
	].
	^ answerClass fromUtf8Bytes: answerBytes! !


!String class methodsFor: 'instance creation' stamp: 'jmv 4/13/2023 14:42:11'!
percentEscapingNonAscii: aByteArray
	"Percent encode any byte not in the ASCII range:
	replace it with %XX where XX are the hexadecimal digits.
	See https://en.wikipedia.org/wiki/URL_encoding
	
	String percentEscapingNonAscii: '⊙' bytes
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	"

	^String percentEscapingNonAsciiAndThoseIn: nil bytes: aByteArray! !

!String class methodsFor: 'instance creation' stamp: 'jmv 4/13/2023 14:27:36'!
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
							c := Character asciiValue: byte.
							aCharacterMap includes: c ]])
				ifTrue: [
					strm nextPut: $%.
					strm nextPut: (byte // 16) asHexDigit.
					strm nextPut: (byte \\ 16) asHexDigit ]
				ifFalse: [ strm nextPut: c ]]].! !

