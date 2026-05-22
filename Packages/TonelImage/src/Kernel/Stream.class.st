"
I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my "contents".
"
Class {
	#name : 'Stream',
	#superclass : 'Object',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'instance creation' }
Stream class >> new [
	self error: 'Streams are created with on: and with:'
]

{ #category : 'testing' }
Stream >> atEnd [
	"Answer whether the receiver can access any more objects."

	self subclassResponsibility
]

{ #category : 'reading' }
Stream >> basicNext [

	^ self next
]

{ #category : 'accessing' }
Stream >> binary [
	"Compatibility. If possible, store and retrieve bytes."
	self useBytes
]

{ #category : 'FileStream compatibility' }
Stream >> close [
	"Only some specific kinds of Streams such as FileStream need this"
]

{ #category : 'testing' }
Stream >> closed [
	"Answer true if we have been closed and are no longer usable.
	Meaningful, for example, for file streams."
	^ false
]

{ #category : 'reading' }
Stream >> contents [
	"Answer the contents of the receiver."
	self subclassResponsibility
]

{ #category : 'enumerating' }
Stream >> do: aBlock  [
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]
]

{ #category : 'writing' }
Stream >> flush [
	"Do nothing by default"
]

{ #category : 'testing' }
Stream >> is: aSymbol [
	"Return true if the receiver responds to the stream protocol (if that's what's asked)"
	^ aSymbol == #Stream or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Stream >> isReadOnly [
	"To be redefined in subclasses that can't do #nextPut:"
	^ false
]

{ #category : 'testing' }
Stream >> isWriteOnly [
	"To be redefined in subclasses that can't do #next"
	^ false
]

{ #category : 'accessing' }
Stream >> localName [
	^'a stream'
]

{ #category : 'reading' }
Stream >> next [
	"Answer the next object accessible by the receiver."

	self subclassResponsibility
]

{ #category : 'reading' }
Stream >> next: anInteger  [
	"Answer the next anInteger number of objects accessible by the receiver."

	| aCollection |
	aCollection := OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection
]

{ #category : 'writing' }
Stream >> next: anInteger put: anObject  [
	"Make anObject be the next anInteger number of objects accessible by the 
	receiver. Answer anObject."

	anInteger timesRepeat: [self nextPut: anObject].
	^anObject
]

{ #category : 'normalized read/write' }
Stream >> nextFloat32BigEndian: bigEndian [
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
	bytes := self next: 4.
	^ bytes float32At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextFloat32Put: aFloat bigEndian: bigEndian [
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
	bytes := ByteArray new: 4.
	bytes float32At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'normalized read/write' }
Stream >> nextFloat64BigEndian: bigEndian [
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ])
			readStream nextFloat64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ])
			readStream nextFloat64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ (bytes float64At: 1 bigEndian: bigEndian)
		+ 0.0 "Convert to SmallFloat64 if appropriate"
]

{ #category : 'normalized read/write' }
Stream >> nextFloat64Put: aFloat bigEndian: bigEndian [
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes float64At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'normalized read/write' }
Stream >> nextInt16BigEndian: bigEndian [
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) 
			readStream nextInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) 
			readStream nextInt16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes int16At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextInt16Put: aNumber bigEndian: bigEndian [
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes int16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'normalized read/write' }
Stream >> nextInt32BigEndian: bigEndian [
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) 
			readStream nextInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) 
			readStream nextInt32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes int32At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextInt32Put: aNumber bigEndian: bigEndian [
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes int32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'normalized read/write' }
Stream >> nextInt64BigEndian: bigEndian [
	"Answer the next signed, 64-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: false ]) 
			readStream nextInt64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: true ]) 
			readStream nextInt64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ bytes int64At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextInt64Put: aNumber bigEndian: bigEndian [
	"Store the given number as a signed, 64-bit integer on this (binary) stream.

	(16r10000000000000000-1234567890123456789) hex '16rEEDDEF0B82167EEB' 

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes int64At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'reading' }
Stream >> nextMatchAll: aColl [
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true
]

{ #category : 'nonhomogeneous read/write' }
Stream >> nextNumber [
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element == $- or: [element == $. or: [element == $)]]]]] whileFalse.
	element ifNil: [^nil].
	self skipBack.
	element == $) ifTrue: [^nil].
	^Number readFrom: self
]

{ #category : 'writing' }
Stream >> nextPut: anObject  [
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	self subclassResponsibility
]

{ #category : 'writing' }
Stream >> nextPut: anObject when: aCondition  [

	aCondition ifTrue: [ self nextPut: anObject ].
]

{ #category : 'writing' }
Stream >> nextPutAll: aCollection  [
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection
]

{ #category : 'writing' }
Stream >> nextPutAll: aCollection asCommaSeparated: aPrintBlock [

	aCollection asCommaSeparated: aPrintBlock on: self
]

{ #category : 'writing' }
Stream >> nextPutAll: aCollection when: aCondition [

	aCondition ifTrue: [ self nextPutAll: aCollection ].
	
]

{ #category : 'nonhomogeneous read/write' }
Stream >> nextString [
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length."

	| bytes length |

	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^bytes asString
]

{ #category : 'nonhomogeneous read/write' }
Stream >> nextStringPut: s  [
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length := s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s
]

{ #category : 'normalized read/write' }
Stream >> nextUint16BigEndian: bigEndian [
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) 
			readStream nextUint16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) 
			readStream nextUint16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes uint16At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextUint16Put: aNumber bigEndian: bigEndian [
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes uint16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'normalized read/write' }
Stream >> nextUint32BigEndian: bigEndian [
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) 
			readStream nextUint32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) 
			readStream nextUint32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes uint32At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextUint32Put: aNumber bigEndian: bigEndian [
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes uint32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'testing' }
Stream >> nextUint32WordsPutAll: aCollection [
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextUint32Put: (aCollection basicAt: i) bigEndian: true ].
	^aCollection
]

{ #category : 'normalized read/write' }
Stream >> nextUint64BigEndian: bigEndian [
	"Answer the next unsigned, 64-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: false ]) 
			readStream nextUint64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: true ]) 
			readStream nextUint64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ bytes uint64At: 1 bigEndian: bigEndian
]

{ #category : 'normalized read/write' }
Stream >> nextUint64Put: aNumber bigEndian: bigEndian [
	"Store the given number as a unsigned, 64-bit integer on this (binary) stream.

	1234567890123456789 hex '16r112210F47DE98115' 

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes uint64At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
]

{ #category : 'nonhomogeneous read/write' }
Stream >> nextUnicodeString [
	"Read a UnicodeString from the receiver.
	Data is UTF-8 bytes.
	The first byte is the length of the data, unless it is greater than 192,
	in which case the first four bytes encode the length."

	| bytes length |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^UnicodeString fromUtf8Bytes: bytes
]

{ #category : 'nonhomogeneous read/write' }
Stream >> nextUnicodeStringPut: s [
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| bytes length |
	bytes := s bytes.
	(length := bytes size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: bytes.
	^s
]

{ #category : 'accessing' }
Stream >> openReadOnly [
	^self
]

{ #category : 'writing' }
Stream >> print: anObject [
	anObject printOn: self
]

{ #category : 'printing' }
Stream >> print: anObject fractionDigits:  placesDesired [
	anObject printOn: self fractionDigits:  placesDesired
]

{ #category : 'printing' }
Stream >> print: anObject when: aCondition  [

	aCondition ifTrue: [self print: anObject].
]

{ #category : 'printing' }
Stream >> printOn: stream [

	super printOn: stream.
	stream space.
	self contents printOn: stream.

]

{ #category : 'reading' }
Stream >> readOnly [
	^self
]
