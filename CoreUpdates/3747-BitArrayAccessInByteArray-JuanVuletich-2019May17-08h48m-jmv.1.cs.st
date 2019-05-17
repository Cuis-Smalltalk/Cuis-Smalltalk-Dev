'From Cuis 5.0 of 7 November 2016 [latest update: #3746] on 17 May 2019 at 9:03:11 am'!

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/16/2019 16:01:00'!
bitAt: bitIndex
	"Answer the bit (0 or 1) at a bit index.
	This way, the receiver behaves as a BitArray.
	Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8."
	| bitPosition index |
	index _ bitIndex - 1 // 8 + 1.
	bitPosition _ bitIndex - 1 \\ 8 + 1.
	^ self bitAt: index bitPosition: bitPosition! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/17/2019 08:52:33'!
bitAt: bitIndex put: aBit
	"Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray
	Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8.
	#[1 0 0 ] bitAt: 1
	#[0 1  0 ] bitAt: 9
	#[0 0 128 ] bitAt: 24
	"
	| bitPosition index |
	index _ bitIndex - 1 // 8 + 1.
	bitPosition _ bitIndex - 1 \\ 8 + 1.
	self bitAt: index bitPosition: bitPosition put: aBit! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/17/2019 08:51:43'!
bitBooleanAt: bitIndex
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	^ (self bitAt: bitIndex) = 1! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/17/2019 08:52:49'!
bitBooleanAt: bitIndex put: aBoolean
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	self bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])! !

!ByteArray methodsFor: 'private' stamp: 'jmv 5/16/2019 15:49:51'!
bitAt: index bitPosition: bitPosition
	"Answer the bit (0 or 1) at byte at index, at bitPosition.
	The bits are indexed starting at 1 for the least significant bit"
	^(self at: index) bitAt: bitPosition! !

!ByteArray methodsFor: 'private' stamp: 'jmv 5/16/2019 15:58:53'!
bitAt: index bitPosition: bitPosition put: aBit
	"Set the bit (0 or 1) at byte at index, at bitPosition. 
	The bit value should be 0 or 1, otherwise raise an Error.
	The bits are indexed starting at 1 for the least significant bit"
	self at: index put: ((self at: index) bitAt: bitPosition put: aBit)! !


!ByteArray class methodsFor: 'instance creation' stamp: 'jmv 5/16/2019 15:59:16'!
newBits: bitCount
	"
	(ByteArray newBits: 8) bitAt: 8 put: 1; bitAt: 8
	(ByteArray newBits: 9) bitAt: 9 put: 1; bitAt: 9
	"
	^self new: bitCount + 7 // 8! !


!ByteArray reorganize!
('accessing' atAllPut: byteAt: byteAt:put: byteSize bytesPerElement indexOf:startingAt:)
('platform independent access' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: doubleAt:bigEndian: doubleAt:put:bigEndian: floatAt:bigEndian: floatAt:put:bigEndian: long64At:bigEndian: long64At:put:bigEndian: longAt:bigEndian: longAt:put:bigEndian: shortAt:bigEndian: shortAt:put:bigEndian: unsignedLong64At:bigEndian: unsignedLong64At:put:bigEndian: unsignedLongAt:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:bigEndian: unsignedShortAt:put:bigEndian:)
('converting' asByteArray asString base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement replaceFrom:to:with:startingAt:)
('comparing' = hash)
('printing' printOn: storeOn:)
('testing' isLiteral)
('initialization' readHexFrom:)
!

