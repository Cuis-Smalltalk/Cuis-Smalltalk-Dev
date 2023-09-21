'From Cuis 6.0 [latest update: #6033] on 21 September 2023 at 12:33:12 pm'!

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 9/21/2023 12:06:05'!
nextUnicodeString
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
	^UnicodeString fromUtf8Bytes: bytes! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 9/21/2023 12:08:35'!
readUnicodeString

	^byteStream nextUnicodeString! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/21/2023 12:24:30'!
readUnicodeSymbol
	"PRIVATE -- Read the contents of a Symbol.
	Note: Even if what was saved was an instance of UnicodeSymbol, if it
	only contains ASCII, then we'll answer an instance of Symbol.
	See #asSymbol and #basicInternNew:withHash:"

    ^ self readUnicodeString asSymbol! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/21/2023 12:20:39'!
writeUnicodeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeUnicodeString: aSymbol! !

!methodRemoval: UnicodeSymbol #comeFullyUpOnReload: stamp: 'jmv 9/21/2023 12:32:34'!
UnicodeSymbol removeSelector: #comeFullyUpOnReload:!
!methodRemoval: Character #comeFullyUpOnReload: stamp: 'jmv 9/21/2023 12:32:34'!
Character removeSelector: #comeFullyUpOnReload:!
