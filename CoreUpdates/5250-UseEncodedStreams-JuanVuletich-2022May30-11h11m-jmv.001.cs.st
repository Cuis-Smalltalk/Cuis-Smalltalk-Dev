'From Cuis 6.0 [latest update: #5205] on 30 May 2022 at 11:14:39 am'!

!ByteArray class methodsFor: 'stream creation' stamp: 'jmv 5/29/2022 12:17:32'!
streamUtf8Contents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings.
	Answer is an instance of us, holding UTF-8 encoded bytes."

	| stream |	
	stream _ EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream byteContents! !


!Utf8String methodsFor: 'as yet unclassified' stamp: 'jmv 5/28/2022 17:37:51'!
readStream
	| stream |
	stream _ EncodedReadStream on: bytes.
	stream useUtf8String.
	^stream! !


!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 5/29/2022 12:19:27'!
streamContents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings.
	Answer is an instance of us."

	| stream |	
	stream _ EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream utf8Contents! !

!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 5/29/2022 12:19:34'!
writeStream
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings."

	| stream |	
	stream _ EncodedWriteStream on: (ByteArray new: 100).
	^stream! !

