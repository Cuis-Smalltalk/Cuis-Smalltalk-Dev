'From Cuis 6.0 [latest update: #5256] on 10 June 2022 at 10:00:18 am'!
!classDefinition: #EncodedWriteStream category: #'Collections-Streams'!
WriteStream subclass: #EncodedWriteStream
	instanceVariableNames: 'lastWrittenElement numberOfCodePointsWritten '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 6/10/2022 09:50:58'!
on: aCollection
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless.
	Overwrite them."

	super on: aCollection.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ 0.! !

!EncodedWriteStream methodsFor: 'positioning' stamp: 'jmv 6/9/2022 20:00:15'!
reset
	super reset.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ 0.! !

!EncodedWriteStream methodsFor: 'positioning' stamp: 'jmv 6/9/2022 20:00:04'!
resetToStart
	super resetToStart.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ 0.! !

!EncodedWriteStream methodsFor: 'positioning' stamp: 'jmv 6/9/2022 20:01:18'!
setToEnd 
	super setToEnd.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ nil.! !


!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/9/2022 14:28:06'!
next: anInteger putAll: aString startingAt: startIndex
'--------' print.
thisContext printStack: 10.
self halt.
	lastWrittenElement _ aString at: anInteger.
	"?
	numberOfCodePointsWritten ifNotNil: [
		numberOfCodePointsWritten _ numberOfCodePointsWritten + aString size ]]"! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/10/2022 09:56:59'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this stream. All 3 kinds of Strings are UTF-8 encoded."

	| bytes |
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes.
		numberOfCodePointsWritten ifNotNil: [
			"As aStringOrBytes is a kind of String, not utf-8 byteArray, we can use its size."
			numberOfCodePointsWritten _ numberOfCodePointsWritten + aStringOrBytes size ]].
	super nextPutAll: bytes.
	lastWrittenElement _ aStringOrBytes last.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/10/2022 09:57:27'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	| n |
	"Save in a temp because #nextPutByte: smashes the ivar."
	n _ numberOfCodePointsWritten ifNotNil: [ numberOfCodePointsWritten + 1 ].
	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOfUnicodeCodePoint: aNumber.
	numberOfCodePointsWritten _ n.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/10/2022 09:55:39'!
position: pos

	| m s |
	"Chequear que los senders sepan que estan mandando una posicion a byte!!"
	m _ thisContext sender method.
	thisContext sender receiver == self ifFalse: [
		(EncodedFileStream positionValidatedSenders includes: m) ifFalse: [
			"SendersOfPositionColon _ Set new"
			s _ Smalltalk at: #SendersOfPositionColon ifAbsentPut: [ Set new ].
			(s includes: m) ifFalse: [
				s add: m.
				{ '-------- Sender de #position: '. m. pos} print.
				thisContext printStack: 10 ]]].
	super position: pos.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ nil.! !

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 6/10/2022 09:57:32'!
nextPutByte: aByte 
	"Insert the argument at the next position in the Stream represented by the receiver."

	numberOfCodePointsWritten _ nil.	"We can no longer know"
	position >= writeLimit
		ifTrue: [^ self pastEndPutByte: aByte]
		ifFalse: [
			position _ position + 1.
			^collection at: position put: aByte]! !

!EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 6/9/2022 14:29:47'!
utf8Contents
	"Answer the Utf8String built so far."

	^Utf8String fromUtf8Bytes: self byteContents codePointCount: numberOfCodePointsWritten.! !

!classDefinition: #EncodedWriteStream category: #'Collections-Streams'!
WriteStream subclass: #EncodedWriteStream
	instanceVariableNames: 'lastWrittenElement numberOfCodePointsWritten'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
