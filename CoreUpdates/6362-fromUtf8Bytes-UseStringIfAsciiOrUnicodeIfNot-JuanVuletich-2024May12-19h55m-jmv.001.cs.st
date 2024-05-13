'From Cuis6.3 [latest update: #6361] on 12 May 2024 at 7:58:00 pm'!

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 5/12/2024 19:03:40'!
fromUtf8Bytes: aByteArray
	"Answer will be an instance of String if aByteArray contains strictly ASCII characters.
	Answer will be an instance of UnicodeString if any byte is over 127, meaning UTF-8 encoded Code Points."

	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	^UnicodeString fromUtf8Bytes: aByteArray.! !


!String class methodsFor: 'instance creation' stamp: 'jmv 5/12/2024 19:57:48'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented.
	Use with caution. If in doubt, send this message to CharacterSequence instead."

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self fromUtf8Bytes: aByteArray trimLastNull: false! !

