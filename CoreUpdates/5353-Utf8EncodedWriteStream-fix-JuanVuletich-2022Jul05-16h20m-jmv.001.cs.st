'From Cuis 6.0 [latest update: #5354] on 5 July 2022 at 4:20:53 pm'!

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/5/2022 16:20:26'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this stream. All 3 kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes.
		numberOfCodePointsWritten ifNotNil: [
			"As aStringOrBytes is a kind of String, not utf-8 byteArray, we can use its size."
			numberOfCodePointsWritten _ numberOfCodePointsWritten + aStringOrBytes size ]].
	super nextPutAll: bytes.
	lastWrittenElement _ aStringOrBytes last.! !

