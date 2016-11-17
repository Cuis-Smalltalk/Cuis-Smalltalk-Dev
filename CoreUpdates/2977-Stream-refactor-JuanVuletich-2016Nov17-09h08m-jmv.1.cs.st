'From Cuis 5.0 of 7 November 2016 [latest update: #2975] on 17 November 2016 at 9:18:37 am'!

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:42'!
nextDouble64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ])
			readStream nextDouble64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ])
			readStream nextDouble64BigEndian: true
	"
	| bytes |
	bytes _ self next: 8.
	^ bytes doubleAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:40'!
nextDouble64Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 8.
	bytes doubleAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:38'!
nextFloat32BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ])
			readStream nextFloat32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ])
			readStream nextFloat32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes floatAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:34'!
nextFloat32Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ]) hex 'DB0F4940'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ]) hex '40490FDB'
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes floatAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt16BigEndian: bigEndian
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) 
			readStream nextSignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) 
			readStream nextSignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes shortAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes shortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt32BigEndian: bigEndian
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) 
			readStream nextSignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) 
			readStream nextSignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes longAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextSignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes longAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:18'!
nextUnsignedInt16BigEndian: bigEndian
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) 
			readStream nextUnsignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) 
			readStream nextUnsignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes unsignedShortAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextUnsignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes unsignedShortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:19'!
nextUnsignedInt32BigEndian: bigEndian
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) 
			readStream nextUnsignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) 
			readStream nextUnsignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes unsignedLongAt: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextUnsignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes unsignedLongAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 6/14/2013 20:02'!
nextNumber
	"Answer a number from the stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $)]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 2/29/2016 11:04'!
nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger.
	Currently only for bigEndian. Consider following the convention in category 'normalized access'"
	| s |
	s _ 0.
	1 to: n do: 
		[:i | s _ (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 2/29/2016 11:04'!
nextNumber: n put: v 
	"Append to the receiver the argument, v, which is a positive 
	SmallInteger or a LargePositiveInteger, as the next n bytes.
	Possibly pad with leading zeros.
	Currently only for bigEndian. Consider following the convention in category 'normalized access'"

	1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
	^ v
! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'ls 9/14/1998 22:46'!
nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| aString length |

	"read the length in binary mode"
	self binary.
	length _ self next.		"first byte."
	length >= 192 ifTrue: [length _ length - 192.
		1 to: 3 do: [:ii | length _ length * 256 + self next]].
	aString _ String new: length.

	"read the characters in ASCII mode"
	self ascii.
	self nextInto: aString.
	^aString! !

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'yo 4/16/2001 17:56'!
nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length _ s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s! !

!methodRemoval: DummyStream #nextDouble64BigEndian:!
DummyStream removeSelector: #nextDouble64BigEndian:!
!methodRemoval: DummyStream #nextDouble64Put:bigEndian:!
DummyStream removeSelector: #nextDouble64Put:bigEndian:!
!methodRemoval: DummyStream #nextNumber:put:!
DummyStream removeSelector: #nextNumber:put:!
!methodRemoval: DummyStream #nextSignedInt16BigEndian:!
DummyStream removeSelector: #nextSignedInt16BigEndian:!
!methodRemoval: DummyStream #nextSignedInt16Put:bigEndian:!
DummyStream removeSelector: #nextSignedInt16Put:bigEndian:!
!methodRemoval: DummyStream #nextSignedInt32BigEndian:!
DummyStream removeSelector: #nextSignedInt32BigEndian:!
!methodRemoval: DummyStream #nextSignedInt32Put:bigEndian:!
DummyStream removeSelector: #nextSignedInt32Put:bigEndian:!
!methodRemoval: DummyStream #nextStringPut:!
DummyStream removeSelector: #nextStringPut:!
!methodRemoval: DummyStream #nextUnsignedInt16BigEndian:!
DummyStream removeSelector: #nextUnsignedInt16BigEndian:!
!methodRemoval: DummyStream #nextUnsignedInt16Put:bigEndian:!
DummyStream removeSelector: #nextUnsignedInt16Put:bigEndian:!
!methodRemoval: DummyStream #nextUnsignedInt32BigEndian:!
DummyStream removeSelector: #nextUnsignedInt32BigEndian:!
!methodRemoval: DummyStream #nextUnsignedInt32Put:bigEndian:!
DummyStream removeSelector: #nextUnsignedInt32Put:bigEndian:!
!methodRemoval: DataStream #readStringOld!
DataStream removeSelector: #readStringOld!
!methodRemoval: PositionableStream #nextDouble64BigEndian:!
PositionableStream removeSelector: #nextDouble64BigEndian:!
!methodRemoval: PositionableStream #nextDouble64Put:bigEndian:!
PositionableStream removeSelector: #nextDouble64Put:bigEndian:!
!methodRemoval: PositionableStream #nextFloat32BigEndian:!
PositionableStream removeSelector: #nextFloat32BigEndian:!
!methodRemoval: PositionableStream #nextFloat32Put:bigEndian:!
PositionableStream removeSelector: #nextFloat32Put:bigEndian:!
!methodRemoval: PositionableStream #nextNumber!
PositionableStream removeSelector: #nextNumber!
!methodRemoval: PositionableStream #nextNumber:!
PositionableStream removeSelector: #nextNumber:!
!methodRemoval: PositionableStream #nextNumber:put:!
PositionableStream removeSelector: #nextNumber:put:!
!methodRemoval: PositionableStream #nextSignedInt16BigEndian:!
PositionableStream removeSelector: #nextSignedInt16BigEndian:!
!methodRemoval: PositionableStream #nextSignedInt16Put:bigEndian:!
PositionableStream removeSelector: #nextSignedInt16Put:bigEndian:!
!methodRemoval: PositionableStream #nextSignedInt32BigEndian:!
PositionableStream removeSelector: #nextSignedInt32BigEndian:!
!methodRemoval: PositionableStream #nextSignedInt32Put:bigEndian:!
PositionableStream removeSelector: #nextSignedInt32Put:bigEndian:!
!methodRemoval: PositionableStream #nextString!
PositionableStream removeSelector: #nextString!
!methodRemoval: PositionableStream #nextStringOld!
PositionableStream removeSelector: #nextStringOld!
!methodRemoval: PositionableStream #nextStringPut:!
PositionableStream removeSelector: #nextStringPut:!
!methodRemoval: PositionableStream #nextUnsignedInt16BigEndian:!
PositionableStream removeSelector: #nextUnsignedInt16BigEndian:!
!methodRemoval: PositionableStream #nextUnsignedInt16Put:bigEndian:!
PositionableStream removeSelector: #nextUnsignedInt16Put:bigEndian:!
!methodRemoval: PositionableStream #nextUnsignedInt32BigEndian:!
PositionableStream removeSelector: #nextUnsignedInt32BigEndian:!
!methodRemoval: PositionableStream #nextUnsignedInt32Put:bigEndian:!
PositionableStream removeSelector: #nextUnsignedInt32Put:bigEndian:!
!methodRemoval: Stream #nextStringOld!
Stream removeSelector: #nextStringOld!

!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isCharacters isEmpty isFileStream isText notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: copyMethodChunkFrom: fileIn fileInAnnouncing: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
!

