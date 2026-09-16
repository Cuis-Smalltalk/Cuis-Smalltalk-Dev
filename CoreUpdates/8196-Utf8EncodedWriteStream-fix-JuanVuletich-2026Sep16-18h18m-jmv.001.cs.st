'From Cuis7.9 [latest update: #8193] on 16 September 2026 at 6:19:17 pm'!

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:09:00 pm (UTC)'!
nextPut: aCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character or a Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCharacterOrByte codePoint].! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:03:56 pm (UTC)'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	| n |
	"Save in a temp because #nextPutByte: smashes the ivar."
	n := numberOfCodePointsWritten ifNotNil: [ numberOfCodePointsWritten + 1 ].
	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber.
	numberOfCodePointsWritten := n.
	lastWrittenElement := Character codePoint: aNumber.! !

