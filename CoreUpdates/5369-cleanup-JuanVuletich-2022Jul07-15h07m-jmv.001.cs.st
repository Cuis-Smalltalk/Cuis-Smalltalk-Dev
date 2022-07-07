'From Cuis 6.0 [latest update: #5368] on 7 July 2022 at 3:08:36 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 14:50:33'!
resetToStart
	super resetToStart.
	lastWrittenElement _ nil.! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 14:45:02'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this file. All 3 kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aStringOrBytes startingAt: startIndex count: anInteger.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 14:49:36'!
position: pos
	"Set the receiver's position as indicated."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	self primSetPosition: fileID to: pos.
	lastWrittenElement _ nil.! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 14:48:27'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this file. All 3 kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	super next: anInteger putAll: aStringOrBytes startingAt: startIndex.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.
	numberOfCodePointsWritten _ nil.	"We can no longer know"! !

