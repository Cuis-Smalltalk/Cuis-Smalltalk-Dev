'From Cuis 6.0 [latest update: #5623] on 7 January 2023 at 9:51:18 am'!

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 1/6/2023 17:31:25'!
nextUnicodeStringPut: s
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| bytes length |
	bytes _ s bytes.
	(length _ bytes size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: bytes.
	^s! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 1/6/2023 17:22:04'!
readUnicodeString

	| str |
	"byteStream ascii."
	str _ byteStream nextUnicodeString.
	byteStream binary.
	^ str
! !

