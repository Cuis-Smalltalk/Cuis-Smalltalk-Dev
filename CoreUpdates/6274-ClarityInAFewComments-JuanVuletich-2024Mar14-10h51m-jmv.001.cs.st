'From Cuis6.3 [latest update: #6272] on 14 March 2024 at 10:53:34 am'!

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 3/14/2024 10:10:58'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	((ThirtyTwoBitSlotsObject new: 1) at: 1 put: 16rFF32791B ; bytesAt: 1) hex
	"

	| bytes word |
	bytes := ByteArray new: 4.
	word := self basicAt: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 3/14/2024 10:52:31'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	((ThirtyTwoBitSlotsObject new: 1) bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]; basicAt: 1) hex
	"

	| word |
	word := 0.
	4 to: 1 by: -1 do: [ :i | word := word * 256 + (aByteArray at: i) ].
	self basicAt: index put: word! !


!WordArray methodsFor: 'accessing' stamp: 'jmv 3/14/2024 10:52:38'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	((WordArray with: 16rFF32791B) bytesAt: 1) hex
	"

	| bytes word |
	bytes := ByteArray new: 4.
	word := self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!WordArray methodsFor: 'accessing' stamp: 'jmv 3/14/2024 10:52:43'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	((WordArray new: 1) bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]) first hex
	"

	| word |
	word := 0.
	4 to: 1 by: -1 do: [ :i | word := word * 256 + (aByteArray at: i) ].
	self at: index put: word! !


!Bitmap methodsFor: 'accessing' stamp: 'jmv 3/14/2024 10:52:48'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	((WordArray with: 16rFF32791B) bytesAt: 1) hex
	"

	| bytes word |
	bytes := ByteArray new: 4.
	word := self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!Bitmap methodsFor: 'accessing' stamp: 'jmv 3/14/2024 10:52:52'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	((WordArray new: 1) bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]) first hex
	"

	| word |
	word := 0.
	4 to: 1 by: -1 do: [ :i | word := word * 256 + (aByteArray at: i) ].
	self at: index put: word! !

