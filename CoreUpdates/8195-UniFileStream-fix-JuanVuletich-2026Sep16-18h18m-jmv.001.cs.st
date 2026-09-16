'From Cuis7.9 [latest update: #8193] on 16 September 2026 at 6:18:57 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:09:03 pm (UTC)'!
nextPut: aCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character or a Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCharacterOrByte codePoint].! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:02:25 pm (UTC)'!
nextPutByte: aByte
	"Write the given byte to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: aByte.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	lastWrittenElement := aByte.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:03:06 pm (UTC)'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber.
	lastWrittenElement := Character codePoint: aNumber.! !

