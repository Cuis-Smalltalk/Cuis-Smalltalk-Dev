'From Cuis 6.0 [latest update: #5373] on 7 July 2022 at 4:19:39 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 16:19:21'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this file. All 3 kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement _ aStringOrBytes last.! !

