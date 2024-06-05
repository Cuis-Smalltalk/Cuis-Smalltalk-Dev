'From Cuis7.1 [latest update: #6448] on 5 June 2024 at 10:02:45 am'!

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 6/5/2024 10:00:06'!
int8At: index bigEndian: dummyArgument
	"Return a 8-bit signed integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| result |
	result := self at: index.
	result < 16r80 ifTrue: [ ^result ].
	^result - 16r100! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 6/5/2024 10:00:50'!
int8At: index put: value bigEndian: dummyArgument
	"Store a 8-bit signed integer quantity at the given byte index
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r100 ].
	self at: index put: unsignedValue.
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 6/5/2024 10:00:58'!
uint8At: index bigEndian: dummyArgument
	"Return a 8-bit unsigned integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 6/5/2024 10:01:02'!
uint8At: index put: value bigEndian: dummyArgument
	"Store a 8-bit unsigned integer quantity at the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index put: value! !

!methodRemoval: ByteArray #uint8At: stamp: 'jmv 6/5/2024 10:01:37'!
ByteArray removeSelector: #uint8At:!
!methodRemoval: ByteArray #int8At: stamp: 'jmv 6/5/2024 10:01:31'!
ByteArray removeSelector: #int8At:!
!methodRemoval: ByteArray #uint8At:put: stamp: 'jmv 6/5/2024 10:01:40'!
ByteArray removeSelector: #uint8At:put:!
!methodRemoval: ByteArray #int8At:put: stamp: 'jmv 6/5/2024 10:01:34'!
ByteArray removeSelector: #int8At:put:!
