'From Cuis 6.0 [latest update: #5798] on 9 May 2023 at 2:59:32 pm'!

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/8/2023 17:37:04'!
nextInt64BigEndian: bigEndian
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
	^ bytes int64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/8/2023 17:37:55'!
nextInt64Put: aNumber bigEndian: bigEndian
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
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/8/2023 17:36:39'!
nextUint64BigEndian: bigEndian
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
	^ bytes uint64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/8/2023 17:36:03'!
nextUint64Put: aNumber bigEndian: bigEndian
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
	self nextPutAll: bytes! !

