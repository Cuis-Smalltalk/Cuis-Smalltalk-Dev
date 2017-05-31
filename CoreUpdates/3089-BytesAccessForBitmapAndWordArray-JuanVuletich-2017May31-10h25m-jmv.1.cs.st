'From Cuis 5.0 of 7 November 2016 [latest update: #3088] on 31 May 2017 at 10:25:43 am'!

!WordArray methodsFor: 'accessing' stamp: 'jmv 5/31/2017 09:48:37'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	WordArray with: 16rFF32791B :: bytesAt: 1 :: hex
	"

	| bytes word |
	bytes _ ByteArray new: 4.
	word _ self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!WordArray methodsFor: 'accessing' stamp: 'jmv 5/31/2017 09:53:07'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex
	"

	| word |
	word _ 0.
	4 to: 1 by: -1 do: [ :i | word _ word * 256 + (aByteArray at: i) ].
	self at: index put: word! !


!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/31/2017 10:16:34'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	WordArray with: 16rFF32791B :: bytesAt: 1 :: hex
	"

	| bytes word |
	bytes _ ByteArray new: 4.
	word _ self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/31/2017 10:16:38'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex
	"

	| word |
	word _ 0.
	4 to: 1 by: -1 do: [ :i | word _ word * 256 + (aByteArray at: i) ].
	self at: index put: word! !

