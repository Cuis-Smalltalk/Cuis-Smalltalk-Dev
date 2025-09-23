'From Cuis7.5 [latest update: #7601] on 22 September 2025 at 3:25:36 pm'!

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 09:57:17'!
readInteger32Bits
    "PRIVATE -- Read the contents of a SmallInteger that fits in 32 bits.
	See #writeInteger32Bits:
	See #readInteger64Bits and #writeInteger64Bits:"

    ^ byteStream nextInt32BigEndian: true	"signed!!!!!!"! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 09:57:44'!
readInteger64Bits
    "PRIVATE -- Read the contents of a SmallInteger.
	See #writeInteger64Bits:
	See #readInteger32Bits and #writeInteger32Bits:"

	| highBits lowBits |
	highBits := byteStream nextInt32BigEndian: true.	"signed!!!!!!"
	lowBits := byteStream nextUint32BigEndian: true.	"unsigned!!!!!!"
	^(highBits bitShift: 32) + lowBits.! !

