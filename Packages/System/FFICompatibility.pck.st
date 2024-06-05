'From Cuis7.1 [latest update: #6426] on 4 June 2024 at 4:20:06 pm'!
'Description This package implements the obsolete accessor methods for ByteArray and ExternalAddress, ffi[U]Int[8|16|32|64]At:[put:], & ExternalAddress>>clone. These have been superceded by [u]int[8|16|32|64]At:[put:].'!
!provides: 'FFICompatibility' 1 0!



!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:29'!
ffiInt16At: byteOffset
	"Return a 16bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 2 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:09:53'!
ffiInt16At: byteOffset put: value
	"Store a 16bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 2 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:00:49'!
ffiInt3264At: byteOffset
	"32 or 64 bits, depending on the platform"
	^Smalltalk wordSize = 8
		ifTrue: [ self ffiInt64At: byteOffset ]
		ifFalse: [ self ffiInt32At: byteOffset ]! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:01:03'!
ffiInt3264At: byteOffset put: value
	"32 or 64 bits, depending on the platform"
	^Smalltalk wordSize = 8
		ifTrue: [ self ffiInt64At: byteOffset put: value ]
		ifFalse: [ self ffiInt32At: byteOffset put: value ]! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:33'!
ffiInt32At: byteOffset
	"Return a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 4 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:41'!
ffiInt32At: byteOffset put: value
	"Store a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 4 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:04:56'!
ffiInt64At: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| int |
	int := self ffiUint64At: byteOffset.
	int > 16r7FFFFFFFFFFFFFFF ifTrue: [^int - 16r10000000000000000].
	^int! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:04:31'!
ffiInt64At: byteOffset put: value
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	self ffiUint64At: byteOffset put: (value < 0
		ifTrue: [ value + 16r10000000000000000 ]
		ifFalse: [ value ]).
	^value! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:35'!
ffiInt8At: byteOffset
	"Return a 8bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 1 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:45'!
ffiInt8At: byteOffset put: value
	"Store a 8bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 1 signed: true! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:12:59'!
ffiPointer32At: byteOffset
	"Answer a 4-byte pointer object stored at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| addr |
	addr := ExternalAddress basicNew: 4.
	1 to: 4 do:
		[:i|
		addr basicAt: i put: (self ffiUint8At: byteOffset+i-1)].
	^addr! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:13:02'!
ffiPointer32At: byteOffset put: value
	"Store a 4-byte pointer object at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 4 do:
		[:i|
		self ffiUint8At: byteOffset+i-1 put: (value basicAt: i)].
	^value! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:13:06'!
ffiPointer64At: byteOffset
	"Answer an 8-byte pointer object stored at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| addr |
	addr := ExternalAddress basicNew: 8.
	1 to: 8 do:
		[:i|
		addr basicAt: i put: (self ffiUint8At: byteOffset+i-1)].
	^addr! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:13:10'!
ffiPointer64At: byteOffset put: value
	"Store an 8-byte pointer object at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 8 do:
		[:i|
		self ffiUint8At: byteOffset+i-1 put: (value basicAt: i)].
	^value! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:38'!
ffiUint16At: byteOffset
	"Return a 16bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 2 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:10:03'!
ffiUint16At: byteOffset put: value
	"Store a 16bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 2 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:06:25'!
ffiUint3264At: byteOffset
	"32 or 64 bits, depending on the platform"
	^Smalltalk wordSize = 8
		ifTrue: [ self ffiUint64At: byteOffset ]
		ifFalse: [ self ffiUint32At: byteOffset ]! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:05:56'!
ffiUint3264At: byteOffset put: value
	"32 or 64 bits, depending on the platform"
	^Smalltalk wordSize = 8
		ifTrue: [ self ffiUint64At: byteOffset put: value ]
		ifFalse: [ self ffiUint32At: byteOffset put: value ]! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:41'!
ffiUint32At: byteOffset
	"Return a 32bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 4 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:48'!
ffiUint32At: byteOffset put: value
	"Store a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 4 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:44'!
ffiUint64At: byteOffset
	"Answer a 64-bit integer in Smalltalk order (little-endian)."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 8 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:51'!
ffiUint64At: byteOffset put: value
	"I store 64-bit integers in Smalltalk (little-endian) order."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 8 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:47'!
ffiUint8At: byteOffset
	"Return a 8bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset size: 1 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:54'!
ffiUint8At: byteOffset put: value
	"Store a 8bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self primFFIIntegerAt: byteOffset put: value size: 1 signed: false! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:07:29'!
primFFIIntegerAt: byteOffset put: value size: nBytes signed: aBoolean
	"Primitive. Store the given value as integer of nBytes size
	in the receiver. Fail if the value is out of range.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive: 'primitiveFFIIntegerAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFICompatibility-Kernel' stamp: 'jmv 11/14/2023 14:08:20'!
primFFIIntegerAt: byteOffset size: nBytes signed: aBoolean
	"Primitive. Return an integer of nBytes size from the receiver.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive: 'primitiveFFIIntegerAt' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ExternalAddress methodsFor: '*FFICompatibility-Kernel' stamp: 'tbn 4/26/2012 14:10'!
clone

	<primitive: 148>
	self primitiveFailed! !
